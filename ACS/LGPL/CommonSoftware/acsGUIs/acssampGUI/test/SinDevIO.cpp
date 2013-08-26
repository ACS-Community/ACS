#include <math.h>
#include <time.h>

#include "SinDevIO.h"

CORBA::Float SinDevIO::read(ACS::Time & timestamp) throw (ACSErr::ACSbaseExImpl) {
	struct timespec time;
	clock_gettime(CLOCK_REALTIME,&time);
	float tmp = time.tv_sec*1000 + time.tv_nsec/1000000ULL;
	CORBA::Float value = sin(tmp*2*M_PI/2000);
	return value;
}
