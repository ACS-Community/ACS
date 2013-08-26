#include <math.h>
#include <time.h>

#include "CosDevIO.h"

CORBA::Double CosDevIO::read(ACS::Time & timestamp) throw (ACSErr::ACSbaseExImpl) {
	struct timespec time;
	clock_gettime(CLOCK_REALTIME,&time);
	float tmp = time.tv_sec*1000 + time.tv_nsec/1000000ULL;
	CORBA::Double value = cos(tmp*2*M_PI/2000);
	return value;
}
