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
* "@(#) $Id: acsexmplLongDevIO.h,v 1.11 2008/10/09 08:41:11 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2003-10-20 created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

///Need the defition of the class to inherit from
#include <baciDevIO.h>
#include <iostream>

/** @file acsexmplLongDevIO.h
 */

/** @addtogroup ACSEXMPLTOC
*/
/*@{
*/

/** @defgroup ACSEXMPLTOMISCELLANEOUS Miscellaneous Examples 
*/
/*@{
*/

/** @defgroup ACSEXMPLLONGDEVIODOC Long DevIO Subclass
 *  @{
 * @htmlonly
 <hr size="2" width="100%">
 <div align="left">
 <h2>Description</h2>
 This example should prove to be particularly useful to those developers dealing directly
 with hardware. The full implementation of a DevIO subclass is presented here and it's usage
 can be found in the <a href="group__ACSEXMPLRPSDOC.html">Ramped Power Supply</a> example. DevIOs should be 
 used when you need to keep a BACI property read-only, but also have to change the value of
 the property dynamically.
 <br>
 <br>
 <h2>What can I gain from this example?</h2>
 <ul>
   <li>the complete implementation of a DevIO subclass.</li>
   <li>limited asynchronous error handling.</li>
 </ul>
 <br>
 <br>
 <h2>Links</h2>
 <ul>
   <li><a href="classLongDevIO.html">LongDevIO Class Reference</a></li>
 </ul>
 <br>
 <br>
 </div>
   @endhtmlonly
 * @}
 */


/* @}*/
/* @}*/

/**
 * This class provides an example of the implementation of a devIO subclass.  Developers 
 * should use devIOs when you have a BACI property where you need to change the underlying value,
 * but do not want the property to be read-write where anyone can change it. Please see the
 * Ramped Power Supply example to see how this class is used in conjunction with BACI long
 * properties.
 * @version "@(#) $Id: acsexmplLongDevIO.h,v 1.11 2008/10/09 08:41:11 cparedes Exp $"
 */
class acsexmpl_EXPORT LongDevIO : public DevIO<CORBA::Long>
{
  public:
    
    /**
     * Default constuctor - nothing to do.
     */
    LongDevIO() {};

    /**
     * Destructor - nothing to destroy.
     */
    virtual ~LongDevIO() {};

    /**
     * ???
     * @return True if the value has been initialized.
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual bool 
    initializeValue()
	{ return true; }
  
    /**
     * Reads the long value from "somewhere".  We could get this value from
     * physical hardware, files on disk, etc.  For the purpose of this simple example,
     * 42 is returned.  This implies every time the value of a BACI long property
     * that uses this class is accessed, 42 will be returned.
     *
     * Last but definitely not least, it is important to note that this method can be invoked
     * from both ROlong and RWlong properties.
     * @param errcode The error code (as defined in the ACS Error System) that occured while
     *        reading this value or 0 if no error occured.  It's up to the developer to 
     *        determine whether an error occured or not.
     * @param timestamp The time when the long value was read (presumably from hardware).
     * @return The value we are looking for.
     * @throw ACSErr::ACSbaseExImpl
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual CORBA::Long 
    read(unsigned long long& timestamp)
	{
	
	    //Must set the timestamp.  This value is in 100s of nanoseconds that have passed
	    //since some date in the 1500s.  Most developers will just want to use the getTimeStamp
	    //method, but you can find more information on the time defintion in the ACS Time 
	    //System.
	    timestamp = getTimeStamp();

	    //Normally you'd want to access hardware, files, pipes, etc. here. We just return 42
	    return 42;
	}

    /**
     * Writes the long value to "someplace".  This could be to physical hardware, files on 
     * disk, etc.  Every time the value of a BACI RWlong property using this class is set,
     * a message will be printed to standard out.
     *
     * Last but definitely not least, it is important to note this method can be only be invoked
     * from RWlong properties.
     * @param value The long value some ACS client (human or software) is trying to set for a
     *              BACI RWlong property that uses this devIO subclass.
     * @param errcode The error code (as defined in the ACS Error System) that occured while
     *        writing this value or 0 if no error occured.  It's up to the developer to 
     *        determine whether an error occured or not.
     * @param timestamp The time when the long value was written (presumably to hardware).
     * @throw ACSErr::ACSbaseExImpl
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual void 
    write(const CORBA::Long &value, unsigned long long& timestamp)
	{
	    //Must set the timestamp.  This value is in 100s of nanoseconds that have passed
	    //since some date in the 1500s (see ACS Time System documentation for specific details).  
	    //Most developers will just want to use the getTimeStamp method.
	    timestamp = getTimeStamp();
	    
	    //Print a message to standard out
	    std::cout << "LongDevIO::write(...) - the value is:" << value << std::endl;
	}
};

#endif



