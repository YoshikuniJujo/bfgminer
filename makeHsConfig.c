#include "config.h"
#include <stdio.h>

int
main(int argc, char *argv[])
{
	printf("module Bfgminerhs.Config(configHaveCurses) where\n");
	printf("\n");
	printf("configHaveCurses :: Bool\n");
	printf("configHaveCurses = toEnum $ %d\n", HAVE_CURSES);

	return 0;
}
