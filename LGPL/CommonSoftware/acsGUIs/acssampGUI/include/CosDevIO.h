#ifndef COSDEVIO_H
#define COSDEVIO_H

#include "baciDevIO.h"

class CosDevIO : public DevIO<CORBA::Double>
{
public:
	CORBA::Double read(ACS::Time & timestamp) throw (ACSErr::ACSbaseExImpl);
};

#endif
