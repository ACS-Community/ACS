#ifndef _BAC_DEVIO_MEM_H_
#define _BAC_DEVIO_MEM_H_
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2003 
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: baciDevIOMem.h,v 1.101 2008/10/09 06:18:16 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2003-06-02 renamed to DevIOMem
* bjeram 2003-05-30 replaced void* value_m  with T&
* bjeram  25/02/02  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

/** 
 * @file 
 * Header file BACI Device I/O Memory classes.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciDevIO.h>

template <class T> class DevIOMem : public DevIO<T>
{
 public:

    /**
     * Constructor that accepts a reference to an object of any type using templates.
     * \param value templated reference to the allocated memory to be used for the data
     */
    DevIOMem(T &value) : value_m(value) {};

    virtual ~DevIOMem() {};

    virtual bool initializeValue(){ return true; }
  
    /**
     *  @throw ACSErr::ACSbaseExImpl
     */
    virtual T read(ACS::Time& timestamp)
	{
	    timestamp = getTimeStamp();
	    return value_m;
	}

    /**
     *  @throw ACSErr::ACSbaseExImpl
     */
    virtual void write(const T& value, ACS::Time& timestamp) 
	{
	    timestamp = getTimeStamp();
	    value_m = value;
	}

  private:
    T &value_m;
};

#endif


