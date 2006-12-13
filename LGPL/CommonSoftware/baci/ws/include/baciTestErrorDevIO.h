#ifndef BACI_TEST_ERROR_DevIO_h
#define BACI_TEST_ERROR_DevIO_h
/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration)
*    and Cosylab 2002, All rights reserved
*
*    This library is free software; you can redistribute it and/or
*    modify it under the terms of the GNU Lesser General Public
*    License as published by the Free Software Foundation; either
*    version 2.1 of the License, or (at your option) any later version.
*
*    This library is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*    Lesser General Public License for more details.
*
*    You should have received a copy of the GNU Lesser General Public
*    License along with this library; if not, write to the Free Software
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
*
* "@(#) $Id: baciTestErrorDevIO.h,v 1.1 2006/12/13 11:34:22 bjeram Exp $"
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

///Need the defition of the class to inherit from
#include <baciDevIO.h>
#include <ACSErrTypeCommon.h>

using namespace baci;
/**
 * The purpose of this DevIO is to test error handling of DevIO
 */
class TestErrorDevIO : public DevIO<CORBA::Double>
{

  public:

    	virtual CORBA::Double
    	read(ACS::Time& timestamp) 
	    throw (ACSErr::ACSbaseExImpl)
	{
	    throw ACSErrTypeCommon::GenericErrorExImpl(__FILE__, __LINE__, "TestErrorDevIO::read");
	    return 1;
	}

    virtual void 
    write(const CORBA::Double &value, ACS::Time& timestamp)
	throw (ACSErr::ACSbaseExImpl)
	{
	  	    throw ACSErrTypeCommon::GenericErrorExImpl(__FILE__, __LINE__, "TestErrorDevIO::write");  
	}
};
#endif


