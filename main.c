#include <stdio.h>
#include <unistd.h>

int main_body(int argc, char *argv[]);

int main(int argc, char *argv[])
{

	int r;

	printf("bfgminer begin!\n");
	sleep(1);
	r = main_body(argc, argv);
	printf("bfgminer done!\n");
	return r;

}
