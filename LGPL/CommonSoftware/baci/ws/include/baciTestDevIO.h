#ifndef acsexmplLongDevIO_h
#define acsexmplLongDevIO_h
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
* "@(#) $Id: baciTestDevIO.h,v 1.2 2006/10/18 08:14:16 bjeram Exp $"
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

///Need the defition of the class to inherit from
#include <baciDevIO.h>
#include <iostream>

using namespace baci;

class TestDevIO : public DevIO<CORBA::Double>
{

  public:

	double m_value;
	TestDevIO() {m_value=-1;};
	virtual ~TestDevIO() {};

	
    	virtual CORBA::Double
    	read(ACS::Time& timestamp) 
	    throw (ACSErr::ACSbaseExImpl)
	{
	    timestamp = getTimeStamp();
	    std::cout << "TestDevIO::read(...) - the value is:" << m_value << std::endl;
	    return 1;
	}

    virtual void 
    write(const CORBA::Double &value, ACS::Time& timestamp)
	throw (ACSErr::ACSbaseExImpl)
	{
	    timestamp = getTimeStamp();
	    std::cout << "TestDevIO::write(...) - the value before is:" << m_value << std::endl;
	    m_value = value;
	    std::cout << "                      - the value after is:" << value <<" - " <<m_value << std::endl;
	}
};
#endif


