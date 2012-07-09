/* gcc -c -DARCH_X86_64 rtc.c */
/* gcc -o rtc.exe -D_TEST_ -DARCH_X86_64 rtc.c */
/*
In Fortron code: do this:
  integer(kind=8)    :: st,et,res
  real(kind=8)       :: tt,rtmp
  call get_rtc(st)
  .....

  call get_rtc(et)

  call get_rtc_res(res)
  rtmp=res
  tt=(et-st)/rtmp

*/

#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

/*#define ARCH_X86*/
/*#define ARCH_X86_64*/
/*#define ARCH_IA64*/
/*#define ARCH_PPC64*/

#if defined(ARCH_X86)
static __inline__ unsigned long long get_rtc(void) {
    unsigned long low, high;

    do {
	asm volatile ("rdtsc\n\t"
		      : "=a" (low), "=d" (high));
    } while (0);
    
    return (((unsigned long long)high << 32)) | ((unsigned long long)low);
}
#elif defined(ARCH_X86_64)
static __inline__ unsigned long long get_rtc(void) {
    unsigned long long rtc;

    do {
	asm volatile ("rdtsc\n\t"
		      "salq $32, %%rdx\n\t"
		      "orq %%rdx, %%rax\n\t"
		      "movq %%rax, %0"
		      : "=r" (rtc)
		      : /* no inputs */
		      : "%rax", "%rdx");
    } while (0);

    return rtc;
}
#elif defined(ARCH_IA64)
static __inline__ unsigned long long get_rtc(void) {
   unsigned long long rtc;

   do {
       asm volatile ("mov %0=ar.itc"
		     : "=r" (rtc)
		     : /* no inputs */);
   } while (0);

    return rtc;
}
#elif defined(ARCH_PPC64)
static __inline__ unsigned long long get_rtc(void) {
    unsigned long low, high, high2;

    do {
	asm volatile ("mftbu %0\n\t"
		      "mftb %1\n\t"
		      "mftbu %2"
		      : "=r" (high), "=r" (low), "=r" (high2));
    } while (high != high2);

    return ((((unsigned long long int)high) << 32) + 
	    ((unsigned long long)low)) * 8;
}
#elif defined(ARCH_NONE)
static __inline__ unsigned long long get_rtc(void) {
	struct timeval Time;
	unsigned long long time_microsec;
	
	gettimeofday(&Time, NULL);
	time_microsec = (Time.tv_sec*1000000) + Time.tv_usec;
	return time_microsec;
}
#else
#error please defined ARCH_XXXX
#endif

/* general rtc_res function. Some processors (such Itanium) can return the */
/* clock resolution, some others don't. Use the general clock to sleep 1s */
/* and make a diff beetween RTC. The precission is enougth to handle */
/* sample of 10e-4 to 10e-2. If you need bigger sample, use the standard OS */
/* method */
static __inline__ unsigned long long get_rtc_res(void) {
#if defined(ARCH_NONE)
	return 1000000L;
#endif
    static unsigned long long res = 0;
    unsigned long long rtc;

    if (res != 0)
	/* the value is in the cache */
	return res;

    rtc = get_rtc();
    usleep(1000000); /* usleep doesn't work as desired */
    res = get_rtc() - rtc;

    return res;
}

static __inline__ unsigned long long get_rtc_perturb(void) {
    static unsigned long long perturb;
    unsigned long long rtc1, rtc2;
    int i;

    if (perturb != 0)
	return perturb;

    for (i = 0; i < 10; ++i)
	rtc1 = get_rtc(); /* some shoot to load code cache ... */

    rtc1 = get_rtc(); /* get base time */
    rtc2 = get_rtc(); /* get end time */

    perturb = rtc2 - rtc1; /* make diff */

    return perturb;
}


void get_rtc_(unsigned long long *rtc) {
    *rtc = get_rtc();
}

void get_rtc_res_(unsigned long long *res) {
    *res = get_rtc_res();
}

void get_rtc_perturb_(unsigned long long *perturb) {
    *perturb = get_rtc_perturb();
}

#if _TEST_
#define SLEEP_TIME 10

int main(int argc, char *argv[]) {
    int i, sleep_time;
    unsigned long long rtc1, rtc2, diff, res, perturb;
    cpu_set_t mask;

    printf("pine process to cpu 0\n");
    __CPU_ZERO(&mask);
    __CPU_SET(0, &mask);
#if !defined(X86_64) && !defined(X86)
    if (sched_setaffinity(getpid(), sizeof(cpu_set_t) / sizeof(__cpu_mask),
			  (void *)&mask) == -1) {
	perror("sched_setaffinity");
	return EXIT_FAILURE;
    }
#else
    if (sched_setaffinity(getpid(), (void *)&mask) == -1) {
	perror("sched_setaffinity");
	return EXIT_FAILURE;
    }
#endif

    if (argv[1] != NULL) {
	sleep_time = atoi(argv[1]);
	if (sleep_time == 0)
	    sleep_time = SLEEP_TIME;
    } else 
	sleep_time = SLEEP_TIME;
 
    printf("sleep %d seconds\n", sleep_time);

    rtc1 = get_rtc();
    sleep(sleep_time);
    rtc2 = get_rtc();

    diff = rtc2 - rtc1;
    res = get_rtc_res();
    perturb = get_rtc_perturb();

    printf("rtc1 = %llu ticks\n", (unsigned long long)rtc1);
    printf("rtc2 = %llu ticks\n", (unsigned long long)rtc2);
    printf("diff = %lu ticks\n", (unsigned long)(rtc2 - rtc1));
    printf("res = %lu ticks = %LG s\n", (unsigned long)(res),
	   ((long double)1.0) / res);
    printf("perturb = %lu ticks = %LG s\n", (unsigned long)(perturb),
	   ((long double)perturb) / res);
    printf("time = %LG s\n", (long double)diff / res);

    return EXIT_SUCCESS;
}
#endif
