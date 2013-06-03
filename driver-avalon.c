/*
 * Copyright 2012-2013 Xiangfu
 * Copyright 2013 Con Kolivas <kernel@kolivas.org>
 * Copyright 2012 Luke Dashjr
 * Copyright 2012 Andrew Smith
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 3 of the License, or (at your option)
 * any later version.  See COPYING for more details.
 */

#include "config.h"

#include <limits.h>
#include <pthread.h>
#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>
#include <dirent.h>
#include <unistd.h>
#ifndef WIN32
  #include <sys/select.h>
  #include <termios.h>
  #include <sys/stat.h>
  #include <fcntl.h>
  #ifndef O_CLOEXEC
    #define O_CLOEXEC 0
  #endif
#else
  #include "compat.h"
  #include <windows.h>
  #include <io.h>
#endif

#include "deviceapi.h"
#include "miner.h"
#include "fpgautils.h"
#include "driver-avalon.h"
#include "logging.h"
#include "util.h"

static int option_offset = -1;
struct avalon_info **avalon_infos;
struct device_drv avalon_drv;

static int avalon_init_task(struct avalon_task *at,
			    uint8_t reset, uint8_t ff, uint8_t fan,
			    uint8_t timeout, uint8_t asic_num,
			    uint8_t miner_num, uint8_t nonce_elf,
			    uint8_t gate_miner, int frequency)
{
	uint8_t *buf;
	static bool first = true;

	if (unlikely(!at))
		return -1;

	if (unlikely(timeout <= 0 || asic_num <= 0 || miner_num <= 0))
		return -1;

	memset(at, 0, sizeof(struct avalon_task));

	if (unlikely(reset)) {
		at->reset = 1;
		at->fan_eft = 1;
		at->timer_eft = 1;
		first = true;
	}

	at->flush_fifo = (ff ? 1 : 0);
	at->fan_eft = (fan ? 1 : 0);

	if (unlikely(first && !at->reset)) {
		at->fan_eft = 1;
		at->timer_eft = 1;
		first = false;
	}

	at->fan_pwm_data = (fan ? fan : AVALON_DEFAULT_FAN_MAX_PWM);
	at->timeout_data = timeout;
	at->asic_num = asic_num;
	at->miner_num = miner_num;
	at->nonce_elf = nonce_elf;

	at->gate_miner_elf = 1;
	at->asic_pll = 1;

	if (unlikely(gate_miner)) {
		at-> gate_miner = 1;
		at->asic_pll = 0;
	}

	buf = (uint8_t *)at;
	buf[5] = 0x00;
	buf[8] = 0x74;
	buf[9] = 0x01;
	buf[10] = 0x00;
	buf[11] = 0x00;
	if (frequency == 256) {
		buf[6] = 0x03;
		buf[7] = 0x08;
	} else if (frequency == 270) {
		buf[6] = 0x73;
		buf[7] = 0x08;
	} else if (frequency == 282) {
		buf[6] = 0xd3;
		buf[7] = 0x08;
	} else if (frequency == 300) {
		buf[6] = 0x63;
		buf[7] = 0x09;
	}

	return 0;
}

static inline void avalon_create_task(struct avalon_task *at,
				      struct work *work)
{
	memcpy(at->midstate, work->midstate, 32);
	memcpy(at->data, work->data + 64, 12);
}

static int avalon_write(int fd, char *buf, ssize_t len)
{
	ssize_t wrote = 0;

	while (len > 0) {
		struct timeval timeout;
		ssize_t ret;
		fd_set wd;

		timeout.tv_sec = 0;
		timeout.tv_usec = 100000;
		FD_ZERO(&wd);
		FD_SET((SOCKETTYPE)fd, &wd);
		ret = select(fd + 1, NULL, &wd, NULL, &timeout);
		if (unlikely(ret < 1)) {
			applog(LOG_WARNING, "Select error on avalon_write");
			return AVA_SEND_ERROR;
		}
		ret = write(fd, buf + wrote, len);
		if (unlikely(ret < 1)) {
			applog(LOG_WARNING, "Write error on avalon_write");
			return AVA_SEND_ERROR;
		}
		wrote += ret;
		len -= ret;
	}

	return AVA_SEND_OK;
}

static int avalon_send_task(int fd, const struct avalon_task *at,
			    struct cgpu_info *avalon)

{
	struct timespec p;
	uint8_t buf[AVALON_WRITE_SIZE + 4 * AVALON_DEFAULT_ASIC_NUM];
	size_t nr_len;
	struct avalon_info *info;
	uint64_t delay = 32000000; /* Default 32ms for B19200 */
	uint32_t nonce_range;
	int ret, i;

	if (at->nonce_elf)
		nr_len = AVALON_WRITE_SIZE + 4 * at->asic_num;
	else
		nr_len = AVALON_WRITE_SIZE;

	memcpy(buf, at, AVALON_WRITE_SIZE);

	if (at->nonce_elf) {
		nonce_range = (uint32_t)0xffffffff / at->asic_num;
		for (i = 0; i < at->asic_num; i++) {
			buf[AVALON_WRITE_SIZE + (i * 4) + 3] =
				(i * nonce_range & 0xff000000) >> 24;
			buf[AVALON_WRITE_SIZE + (i * 4) + 2] =
				(i * nonce_range & 0x00ff0000) >> 16;
			buf[AVALON_WRITE_SIZE + (i * 4) + 1] =
				(i * nonce_range & 0x0000ff00) >> 8;
			buf[AVALON_WRITE_SIZE + (i * 4) + 0] =
				(i * nonce_range & 0x000000ff) >> 0;
		}
	}
#if defined(__BIG_ENDIAN__) || defined(MIPSEB)
	uint8_t tt = 0;

	tt = (buf[0] & 0x0f) << 4;
	tt |= ((buf[0] & 0x10) ? (1 << 3) : 0);
	tt |= ((buf[0] & 0x20) ? (1 << 2) : 0);
	tt |= ((buf[0] & 0x40) ? (1 << 1) : 0);
	tt |= ((buf[0] & 0x80) ? (1 << 0) : 0);
	buf[0] = tt;

	tt = (buf[4] & 0x0f) << 4;
	tt |= ((buf[4] & 0x10) ? (1 << 3) : 0);
	tt |= ((buf[4] & 0x20) ? (1 << 2) : 0);
	tt |= ((buf[4] & 0x40) ? (1 << 1) : 0);
	tt |= ((buf[4] & 0x80) ? (1 << 0) : 0);
	buf[4] = tt;
#endif
	if (likely(avalon)) {
		info = avalon->device_data;
		delay = nr_len * 10 * 1000000000ULL;
		delay = delay / info->baud;
	}

	if (at->reset)
		nr_len = 1;
	if (opt_debug) {
		applog(LOG_DEBUG, "Avalon: Sent(%u):", (unsigned int)nr_len);
		hexdump(buf, nr_len);
	}
	ret = avalon_write(fd, (char *)buf, nr_len);

	p.tv_sec = 0;
	p.tv_nsec = (long)delay + 4000000;
	nanosleep(&p, NULL);
	applog(LOG_DEBUG, "Avalon: Sent: Buffer delay: %ld", p.tv_nsec);

	return ret;
}

static void avalon_decode_nonce(struct thr_info *thr, struct cgpu_info *avalon,
				struct avalon_info *info, struct avalon_result *ar,
				struct work *work)
{
	uint32_t nonce;

	info = avalon->device_data;
	info->matching_work[work->subid]++;
	nonce = htole32(ar->nonce);
	applog(LOG_DEBUG, "Avalon: nonce = %0x08x", nonce);
	submit_nonce(thr, work, nonce);
}

static int avalon_read(int fd, char *buf, ssize_t len)
{
	ssize_t aread = 0;

	while (len > 0) {
		struct timeval timeout;
		ssize_t ret;
		fd_set rd;

		timeout.tv_sec = 0;
		timeout.tv_usec = 100000;
		FD_ZERO(&rd);
		FD_SET(fd, &rd);
		ret = select(fd + 1, &rd, NULL, NULL, &timeout);
		if (unlikely(ret < 1)) {
			applog(LOG_WARNING, "Select error on avalon_read");
			return AVA_GETS_ERROR;
		}
		ret = read(fd, buf + aread, len);
		if (unlikely(ret < 1)) {
			applog(LOG_WARNING, "Read error on avalon_read");
			return AVA_GETS_ERROR;
		}
		aread += ret;
		len -= ret;
	}

	return AVA_GETS_OK;
}

static int avalon_reset(struct cgpu_info *avalon, int fd)
{
	struct avalon_result ar;
	struct avalon_task at;
	uint8_t *buf;
	int ret, i = 0;
	struct timespec p;

	/* Send reset, then check for result */
	avalon_init_task(&at, 1, 0,
			 AVALON_DEFAULT_FAN_MAX_PWM,
			 AVALON_DEFAULT_TIMEOUT,
			 AVALON_DEFAULT_ASIC_NUM,
			 AVALON_DEFAULT_MINER_NUM,
			 0, 0,
			 AVALON_DEFAULT_FREQUENCY);
	ret = avalon_send_task(fd, &at, NULL);
	if (unlikely(ret == AVA_SEND_ERROR))
		return -1;

	ret = avalon_read(fd, (char *)&ar, AVALON_READ_SIZE);
	if (unlikely(ret == AVA_GETS_ERROR))
		return -1;

	/* What do these sleeps do?? */
	p.tv_sec = 0;
	p.tv_nsec = AVALON_RESET_PITCH;
	nanosleep(&p, NULL);

	buf = (uint8_t *)&ar;
	/* We may also get 0x00 and 0x18 first */
	if (buf[0] != 0xAA)
		buf = &buf[1];
	if (buf[0] != 0xAA)
		buf = &buf[1];
	if (buf[0] == 0xAA && buf[1] == 0x55 &&
	    buf[2] == 0xAA && buf[3] == 0x55) {
		for (i = 4; i < 11; i++)
			if (buf[i] != 0)
				break;
	}

	if (i != 11) {
		applog(LOG_ERR, "AVA%d: Reset failed! not an Avalon?"
		       " (%d: %02x %02x %02x %02x)", avalon->device_id,
		       i, buf[0], buf[1], buf[2], buf[3]);
		/* FIXME: return 1; */
	} else
		applog(LOG_WARNING, "AVA%d: Reset succeeded",
		       avalon->device_id);

	return 0;
}

static void get_options(int this_option_offset, int *baud, int *miner_count,
			int *asic_count, int *timeout, int *frequency)
{
	char buf[BUFSIZ+1];
	char *ptr, *comma, *colon, *colon2, *colon3, *colon4;
	size_t max;
	int i, tmp;

	if (opt_avalon_options == NULL)
		buf[0] = '\0';
	else {
		ptr = opt_avalon_options;
		for (i = 0; i < this_option_offset; i++) {
			comma = strchr(ptr, ',');
			if (comma == NULL)
				break;
			ptr = comma + 1;
		}

		comma = strchr(ptr, ',');
		if (comma == NULL)
			max = strlen(ptr);
		else
			max = comma - ptr;

		if (max > BUFSIZ)
			max = BUFSIZ;
		strncpy(buf, ptr, max);
		buf[max] = '\0';
	}

	*baud = AVALON_IO_SPEED;
	*miner_count = AVALON_DEFAULT_MINER_NUM - 8;
	*asic_count = AVALON_DEFAULT_ASIC_NUM;
	*timeout = AVALON_DEFAULT_TIMEOUT;
	*frequency = AVALON_DEFAULT_FREQUENCY;

	if (!(*buf))
		return;

	colon = strchr(buf, ':');
	if (colon)
		*(colon++) = '\0';

	tmp = atoi(buf);
	switch (tmp) {
	case 115200:
		*baud = 115200;
		break;
	case 57600:
		*baud = 57600;
		break;
	case 38400:
		*baud = 38400;
		break;
	case 19200:
		*baud = 19200;
		break;
	default:
		quit(1,
			"Invalid avalon-options for baud (%s) "
			"must be 115200, 57600, 38400 or 19200", buf);
	}

	if (colon && *colon) {
		colon2 = strchr(colon, ':');
		if (colon2)
			*(colon2++) = '\0';

		if (*colon) {
			tmp = atoi(colon);
			if (tmp > 0 && tmp <= AVALON_DEFAULT_MINER_NUM) {
				*miner_count = tmp;
			} else {
				quit(1,
					"Invalid avalon-options for "
					"miner_count (%s) must be 1 ~ %d",
					colon, AVALON_DEFAULT_MINER_NUM);
			}
		}

		if (colon2 && *colon2) {
			colon3 = strchr(colon2, ':');
			if (colon3)
				*(colon3++) = '\0';

			tmp = atoi(colon2);
			if (tmp > 0 && tmp <= AVALON_DEFAULT_ASIC_NUM)
				*asic_count = tmp;
			else {
				quit(1,
					"Invalid avalon-options for "
					"asic_count (%s) must be 1 ~ %d",
					colon2, AVALON_DEFAULT_ASIC_NUM);
			}

			if (colon3 && *colon3) {
				colon4 = strchr(colon3, ':');
				if (colon4)
					*(colon4++) = '\0';

				tmp = atoi(colon3);
				if (tmp > 0 && tmp <= 0xff)
					*timeout = tmp;
				else {
					quit(1,
						"Invalid avalon-options for "
						"timeout (%s) must be 1 ~ %d",
						colon3, 0xff);
				}
				if (colon4 && *colon4) {
					tmp = atoi(colon4);
					switch (tmp) {
					case 256:
					case 270:
					case 282:
					case 300:
						*frequency = tmp;
						break;
					default:
						quit(1,
							"Invalid avalon-options for "
							"frequency must be 256/270/282/300");
					}
				}
			}
		}
	}
}

static bool avalon_detect_one(const char *devpath)
{
	struct avalon_info *info;
	int fd, ret;
	int baud, miner_count, asic_count, timeout, frequency = 0;
	struct cgpu_info *avalon;

	int this_option_offset = ++option_offset;
	get_options(this_option_offset, &baud, &miner_count, &asic_count,
		    &timeout, &frequency);

	applog(LOG_DEBUG, "Avalon Detect: Attempting to open %s "
	       "(baud=%d miner_count=%d asic_count=%d timeout=%d frequency=%d)",
	       devpath, baud, miner_count, asic_count, timeout, frequency);

	fd = avalon_open2(devpath, baud, true);
	if (unlikely(fd == -1)) {
		applog(LOG_ERR, "Avalon Detect: Failed to open %s", devpath);
		return false;
	}

	/* We have a real Avalon! */
	avalon = calloc(1, sizeof(struct cgpu_info));
	avalon->drv = &avalon_drv;
	avalon->device_path = strdup(devpath);
	avalon->device_fd = fd;
	avalon->threads = AVALON_MINER_THREADS;
	add_cgpu(avalon);

	avalon_infos = realloc(avalon_infos,
			       sizeof(struct avalon_info *) *
			       (total_devices + 1));

	applog(LOG_INFO, "Avalon Detect: Found at %s, mark as %d",
	       devpath, avalon->device_id);

	avalon_infos[avalon->device_id] = calloc(sizeof(struct avalon_info), 1);
	if (unlikely(!(avalon_infos[avalon->device_id])))
		quit(1, "Failed to calloc avalon_infos");

	avalon->device_data = avalon_infos[avalon->device_id];
	info = avalon->device_data;

	info->baud = baud;
	info->miner_count = miner_count;
	info->asic_count = asic_count;
	info->timeout = timeout;

	info->fan_pwm = AVALON_DEFAULT_FAN_MIN_PWM;
	info->temp_max = 0;
	/* This is for check the temp/fan every 3~4s */
	info->temp_history_count = (4 / (float)((float)info->timeout * ((float)1.67/0x32))) + 1;
	if (info->temp_history_count <= 0)
		info->temp_history_count = 1;

	info->temp_history_index = 0;
	info->temp_sum = 0;
	info->temp_old = 0;
	info->frequency = frequency;

	ret = avalon_reset(avalon, fd);
	if (ret) {
		; /* FIXME: I think IT IS avalon and wait on reset;
		   * avalon_close(fd);
		   * return false; */
	}

	return true;
}

static inline void avalon_detect()
{
	serial_detect(&avalon_drv, avalon_detect_one);
}

static void avalon_init(struct cgpu_info *avalon)
{
	applog(LOG_INFO, "Avalon: Opened on %s", avalon->device_path);
}

static struct work *avalon_valid_result(struct cgpu_info *avalon, struct avalon_result *ar)
{
	return find_queued_work_bymidstate(avalon, (char *)ar->midstate, 32,
					   (char *)ar->data, 64, 12);
}

static void avalon_update_temps(struct cgpu_info *avalon, struct avalon_info *info,
				struct avalon_result *ar);

static void avalon_inc_nvw(struct avalon_info *info, struct thr_info *thr)
{
	if (unlikely(info->idle))
		return;

	applog(LOG_WARNING, "%s%d: No valid work - HW error",
			thr->cgpu->drv->name, thr->cgpu->device_id);

	inc_hw_errors(thr);
	mutex_lock(&info->lock);
	info->no_matching_work++;
	mutex_unlock(&info->lock);
}

static void avalon_parse_results(struct cgpu_info *avalon, struct avalon_info *info,
				 struct thr_info *thr, char *buf, int *offset)
{
	int i, spare = *offset - AVALON_READ_SIZE;
	bool found = false;

	for (i = 0; i <= spare; i++) {
		struct avalon_result *ar;
		struct work *work;

		ar = (struct avalon_result *)&buf[i];
		work = avalon_valid_result(avalon, ar);
		if (work) {
			bool gettemp = false;

			found = true;

			mutex_lock(&info->lock);
			if (!(++avalon->results % info->miner_count)) {
				gettemp = true;
				avalon->results = 0;
			}
 			info->nonces++;
			mutex_unlock(&info->lock);

			avalon_decode_nonce(thr, avalon, info, ar, work);
			if (gettemp)
				avalon_update_temps(avalon, info, ar);
			break;
		}
	}

	if (!found) {
		spare = *offset - AVALON_READ_SIZE;
		/* We are buffering and haven't accumulated one more corrupt
		 * work result. */
		if (spare < (int)AVALON_READ_SIZE)
			return;
		avalon_inc_nvw(info, thr);
	} else {
		spare = AVALON_READ_SIZE + i;
		if (i) {
			if (i >= (int)AVALON_READ_SIZE)
				avalon_inc_nvw(info, thr);
			else
				applog(LOG_WARNING, "Avalon: Discarding %d bytes from buffer", i);
		}
	}

	*offset -= spare;
	memmove(buf, buf + spare, *offset);
}

static void *avalon_get_results(void *userdata)
{
	struct cgpu_info *avalon = (struct cgpu_info *)userdata;
	struct avalon_info *info = avalon->device_data;
	const int rsize = AVALON_FTDI_READSIZE;
	char readbuf[AVALON_READBUF_SIZE];
	struct thr_info *thr = info->thr;
	int fd = avalon->device_fd;
	char threadname[24];
	int offset = 0;

	pthread_detach(pthread_self());

	snprintf(threadname, 24, "ava_recv/%d", avalon->device_id);
	RenameThread(threadname);

	while (42) {
		struct timeval timeout;
		char buf[rsize];
		ssize_t ret;
		fd_set rd;

		if (offset >= (int)AVALON_READ_SIZE)
			avalon_parse_results(avalon, info, thr, readbuf, &offset);

		if (unlikely(offset + rsize >= AVALON_READBUF_SIZE)) {
			/* This should never happen */
			applog(LOG_ERR, "Avalon readbuf overflow, resetting buffer");
			offset = 0;
		}

		timeout.tv_sec = 0;
		timeout.tv_usec = AVALON_READ_TIMEOUT * 1000;
		FD_ZERO(&rd);
		FD_SET(fd, &rd);
		ret = select(fd + 1, &rd, NULL, NULL, &timeout);
		if (ret < 1) {
			if (unlikely(ret < 0))
				applog(LOG_WARNING, "Select error in avalon_get_results");
			continue;
		}
		ret = read(fd, buf, AVALON_FTDI_READSIZE);
		if (unlikely(ret < 1)) {
			if (unlikely(ret < 0))
				applog(LOG_WARNING, "Read error in avalon_get_results");
			continue;
		}

		if (opt_debug) {
			applog(LOG_DEBUG, "Avalon: get:");
			hexdump((uint8_t *)buf, ret);
		}

		memcpy(&readbuf[offset], buf, ret);
		offset += ret;
	}
	return NULL;
}

static void avalon_rotate_array(struct cgpu_info *avalon)
{
	avalon->queued = 0;
	if (++avalon->work_array >= AVALON_ARRAY_SIZE)
		avalon->work_array = 0;
}

static void wait_avalon_ready(int fd)
{
	while (avalon_buffer_full(fd) == AVA_BUFFER_FULL) {
		nmsleep(40);
	}
}

static void *avalon_send_tasks(void *userdata)
{
	struct cgpu_info *avalon = (struct cgpu_info *)userdata;
	struct avalon_info *info = avalon->device_data;
	const int avalon_get_work_count = info->miner_count;
	int fd = avalon->device_fd;
	char threadname[24];

	pthread_detach(pthread_self());

	snprintf(threadname, 24, "ava_send/%d", avalon->device_id);
	RenameThread(threadname);

	while (42) {
		int start_count, end_count, i, j, ret;
		struct avalon_task at;
		int idled = 0;

		wait_avalon_ready(fd);

		mutex_lock(&info->qlock);
		start_count = avalon->work_array * avalon_get_work_count;
		end_count = start_count + avalon_get_work_count;
		for (i = start_count, j = 0; i < end_count; i++, j++) {
			if (unlikely(avalon_buffer_full(fd) == AVA_BUFFER_FULL)) {
				applog(LOG_WARNING,
				       "AVA%i: Buffer full before all work queued",
					avalon->device_id);
				break;
			}

			if (likely(j < avalon->queued)) {
				info->idle = false;
				avalon_init_task(&at, 0, 0, info->fan_pwm,
						info->timeout, info->asic_count,
						info->miner_count, 1, 0, info->frequency);
				avalon_create_task(&at, avalon->works[i]);
			} else {
				idled++;
				avalon_init_task(&at, 0, 0, info->fan_pwm,
						info->timeout, info->asic_count,
						info->miner_count, 1, 1, info->frequency);
			}
			ret = avalon_send_task(fd, &at, avalon);
			if (unlikely(ret == AVA_SEND_ERROR)) {
				applog(LOG_ERR, "AVA%i: Comms error(buffer)",
				       avalon->device_id);
				dev_error(avalon, REASON_DEV_COMMS_ERROR);
				avalon_reset(avalon, fd);
			}
		}
		avalon_rotate_array(avalon);
		pthread_cond_signal(&info->qcond);
		mutex_unlock(&info->qlock);

		if (unlikely(idled && !info->idle)) {
			info->idle = true;
			applog(LOG_WARNING, "AVA%i: Idled %d miners",
			       avalon->device_id, idled);
		}
	}
	return NULL;
}

static bool avalon_prepare(struct thr_info *thr)
{
	struct cgpu_info *avalon = thr->cgpu;
	struct avalon_info *info = avalon->device_data;
	struct timeval now;

	free(avalon->works);
	avalon->works = calloc(info->miner_count * sizeof(struct work *),
			       AVALON_ARRAY_SIZE);
	if (!avalon->works)
		quit(1, "Failed to calloc avalon works in avalon_prepare");

	info->idle = true;
	info->thr = thr;
	mutex_init(&info->lock);
	mutex_init(&info->qlock);
	if (unlikely(pthread_cond_init(&info->qcond, NULL)))
		quit(1, "Failed to pthread_cond_init avalon qcond");

	if (pthread_create(&info->write_thr, NULL, avalon_send_tasks, (void *)avalon))
		quit(1, "Failed to create avalon write_thr");

	mutex_lock(&info->qlock);
	pthread_cond_wait(&info->qcond, &info->qlock);
	mutex_unlock(&info->qlock);

	if (pthread_create(&info->read_thr, NULL, avalon_get_results, (void *)avalon))
		quit(1, "Failed to create avalon read_thr");

	avalon_init(avalon);

	cgtime(&now);
	get_datestamp(avalon->init, &now);
	return true;
}

static void avalon_free_work(struct thr_info *thr)
{
	struct cgpu_info *avalon;
	struct avalon_info *info;
	struct work **works;
	int i;

	avalon = thr->cgpu;
	avalon->queued = 0;
	if (unlikely(!avalon->works))
		return;
	works = avalon->works;
	info = avalon->device_data;

	for (i = 0; i < info->miner_count * 4; i++) {
		if (works[i]) {
			work_completed(avalon, works[i]);
			works[i] = NULL;
		}
	}
}

static void do_avalon_close(struct thr_info *thr)
{
	struct cgpu_info *avalon = thr->cgpu;
	struct avalon_info *info = avalon->device_data;
	int i, fd = avalon->device_fd;

	pthread_cancel(info->read_thr);
	pthread_cancel(info->write_thr);
	avalon_reset(avalon, fd);
	wait_avalon_ready(fd);
	applog(LOG_WARNING, "AVA%i: Idling %d miners", avalon->device_id,
	       info->miner_count);
	/* Send idle to all miners */
	for (i = 0; i < info->miner_count; i++) {
		struct avalon_task at;

		avalon_init_task(&at, 0, 0, info->fan_pwm, info->timeout,
				 info->asic_count, info->miner_count, 1, 1,
				 info->frequency);
		avalon_send_task(fd, &at, avalon);
	}
	avalon_free_work(thr);
	avalon_close(fd);
	avalon->device_fd = -1;

	info->no_matching_work = 0;
}

static inline void record_temp_fan(struct avalon_info *info, struct avalon_result *ar, float *temp_avg)
{
	info->fan0 = ar->fan0 * AVALON_FAN_FACTOR;
	info->fan1 = ar->fan1 * AVALON_FAN_FACTOR;
	info->fan2 = ar->fan2 * AVALON_FAN_FACTOR;

	info->temp0 = ar->temp0;
	info->temp1 = ar->temp1;
	info->temp2 = ar->temp2;
	if (ar->temp0 & 0x80) {
		ar->temp0 &= 0x7f;
		info->temp0 = 0 - ((~ar->temp0 & 0x7f) + 1);
	}
	if (ar->temp1 & 0x80) {
		ar->temp1 &= 0x7f;
		info->temp1 = 0 - ((~ar->temp1 & 0x7f) + 1);
	}
	if (ar->temp2 & 0x80) {
		ar->temp2 &= 0x7f;
		info->temp2 = 0 - ((~ar->temp2 & 0x7f) + 1);
	}

	*temp_avg = info->temp2 > info->temp1 ? info->temp2 : info->temp1;

	if (info->temp0 > info->temp_max)
		info->temp_max = info->temp0;
	if (info->temp1 > info->temp_max)
		info->temp_max = info->temp1;
	if (info->temp2 > info->temp_max)
		info->temp_max = info->temp2;
}

static inline void adjust_fan(struct avalon_info *info)
{
	int temp_new;

	temp_new = info->temp_sum / info->temp_history_count;

	if (temp_new < 35) {
		info->fan_pwm = AVALON_DEFAULT_FAN_MIN_PWM;
		info->temp_old = temp_new;
	} else if (temp_new > 55) {
		info->fan_pwm = AVALON_DEFAULT_FAN_MAX_PWM;
		info->temp_old = temp_new;
	} else if (abs(temp_new - info->temp_old) >= 2) {
		info->fan_pwm = AVALON_DEFAULT_FAN_MIN_PWM + (temp_new - 35) * 6.4;
		info->temp_old = temp_new;
	}
}

static void avalon_update_temps(struct cgpu_info *avalon, struct avalon_info *info,
				struct avalon_result *ar)
{
	record_temp_fan(info, ar, &(avalon->temp));
	applog(LOG_INFO,
		"Avalon: Fan1: %d/m, Fan2: %d/m, Fan3: %d/m\t"
		"Temp1: %dC, Temp2: %dC, Temp3: %dC, TempMAX: %dC",
		info->fan0, info->fan1, info->fan2,
		info->temp0, info->temp1, info->temp2, info->temp_max);
	info->temp_history_index++;
	info->temp_sum += avalon->temp;
	applog(LOG_DEBUG, "Avalon: temp_index: %d, temp_count: %d, temp_old: %d",
		info->temp_history_index, info->temp_history_count, info->temp_old);
	if (info->temp_history_index == info->temp_history_count) {
		adjust_fan(info);
		info->temp_history_index = 0;
		info->temp_sum = 0;
	}
}

/* We use a replacement algorithm to only remove references to work done from
 * the buffer when we need the extra space for new work. */
static bool avalon_fill(struct cgpu_info *avalon)
{
	int subid, slot, mc = avalon_infos[avalon->device_id]->miner_count;
	struct avalon_info *info = avalon->device_data;
	struct work *work;
	bool ret = true;

	mutex_lock(&info->qlock);
	if (avalon->queued >= mc)
		goto out_unlock;
	work = get_queued(avalon);
	if (unlikely(!work)) {
		ret = false;
		goto out_unlock;
	}
	subid = avalon->queued++;
	work->subid = subid;
	slot = avalon->work_array * mc + subid;
	if (likely(avalon->works[slot]))
		work_completed(avalon, avalon->works[slot]);
	avalon->works[slot] = work;
	if (avalon->queued < mc)
		ret = false;
out_unlock:
	mutex_unlock(&info->qlock);

	return ret;
}

static int64_t avalon_scanhash(struct thr_info *thr)
{
	struct cgpu_info *avalon = thr->cgpu;
	struct avalon_info *info = avalon->device_data;
	struct timeval now, then, tdiff;
	int64_t hash_count, us_timeout;
	struct timespec abstime;

	/* Full nonce range */
	us_timeout = 0x100000000ll / info->asic_count / info->frequency;
	tdiff.tv_sec = us_timeout / 1000000;
	tdiff.tv_usec = us_timeout - (tdiff.tv_sec * 1000000);
	cgtime(&now);
	timeradd(&now, &tdiff, &then);
	abstime.tv_sec = then.tv_sec;
	abstime.tv_nsec = then.tv_usec * 1000;

	/* Wait until avalon_send_tasks signals us that it has completed
	 * sending its work or a full nonce range timeout has occurred */
	mutex_lock(&info->qlock);
	pthread_cond_timedwait(&info->qcond, &info->qlock, &abstime);
	mutex_unlock(&info->qlock);

	mutex_lock(&info->lock);
	hash_count = 0xffffffffull * (uint64_t)info->nonces;
	info->nonces = 0;
	mutex_unlock(&info->lock);

	/* This hashmeter is just a utility counter based on returned shares */
	return hash_count;
}

static void avalon_flush_work(struct cgpu_info *avalon)
{
	struct avalon_info *info = avalon->device_data;
	struct thr_info *thr = info->thr;

	thr->work_restart = false;

	mutex_lock(&info->qlock);
	/* Will overwrite any work queued */
	avalon->queued = 0;
	pthread_cond_signal(&info->qcond);
	mutex_unlock(&info->qlock);
}

static struct api_data *avalon_api_stats(struct cgpu_info *cgpu)
{
	struct api_data *root = NULL;
	struct avalon_info *info = cgpu->device_data;
	int i;

	root = api_add_int(root, "baud", &(info->baud), false);
	root = api_add_int(root, "miner_count", &(info->miner_count),false);
	root = api_add_int(root, "asic_count", &(info->asic_count), false);
	root = api_add_int(root, "timeout", &(info->timeout), false);
	root = api_add_int(root, "frequency", &(info->frequency), false);

	root = api_add_int(root, "fan1", &(info->fan0), false);
	root = api_add_int(root, "fan2", &(info->fan1), false);
	root = api_add_int(root, "fan3", &(info->fan2), false);

	root = api_add_int(root, "temp1", &(info->temp0), false);
	root = api_add_int(root, "temp2", &(info->temp1), false);
	root = api_add_int(root, "temp3", &(info->temp2), false);
	root = api_add_int(root, "temp_max", &(info->temp_max), false);

	root = api_add_int(root, "no_matching_work", &(info->no_matching_work), false);
	for (i = 0; i < info->miner_count; i++) {
		char mcw[24];

		sprintf(mcw, "match_work_count%d", i + 1);
		root = api_add_int(root, mcw, &(info->matching_work[i]), false);
	}

	return root;
}

static void avalon_shutdown(struct thr_info *thr)
{
	do_avalon_close(thr);
}

struct device_drv avalon_drv = {
	.dname = "avalon",
	.name = "AVA",
	.drv_detect = avalon_detect,
	.thread_prepare = avalon_prepare,
	.minerloop = hash_queued_work,
	.queue_full = avalon_fill,
	.scanwork = avalon_scanhash,
	.flush_work = avalon_flush_work,
	.get_api_stats = avalon_api_stats,
	.reinit_device = avalon_init,
	.thread_shutdown = avalon_shutdown,
};
