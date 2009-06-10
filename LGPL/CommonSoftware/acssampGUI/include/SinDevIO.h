#ifndef SINDEVIO_H
#define SINDEVIO_H

#include "baciDevIO.h"

class SinDevIO : public DevIO<CORBA::Float>
{
public:
	CORBA::Float read(ACS::Time & timestamp) throw (ACSErr::ACSbaseExImpl);
};

#endif
