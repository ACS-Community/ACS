#ifndef baciRecoverableObject_h
#define baciRecoverableObject_h

/*******************************************************************
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
* "@(#) $Id: baciRecoverableObject.h,v 1.95 2011/09/02 11:39:00 bjeram Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001/02/20  created
*/

/** 
 * @file 
 * Header file for BACI Recoverable Objects.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>
#include <baciExport.h>

namespace baci {

/**
 * Monitor ID (needed for decoding)
 */
#define ID_MONITOR     0

/**
 * Event ID (needed for decoding)
 */
#define ID_EVENT       1

/**
 * Interface which recoverable classes must implement
 * This interface provides all methods needed by BACIRecoveryManager to
 * create unique stringified data containing object state. <br>
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 * @see BACIRecoveryManager
 */

class baci_EXPORT RecoverableObject {

public:

    /**
     * Destructor
     */
    virtual ~RecoverableObject() {}

  /**
   * Get object's "recovery" id; each object type sould have unique id
   * @return object's id 
   */
  virtual int getId(void) = 0;

  /**
   * Returns objects name; each instance should have unique name
   * @return object's name 
   */
  virtual const char* getName(void) = 0;

  /**
   * Get object's state represented as string 
   * @return pointer to stringified object's state
   */
  virtual const char* getObjectState(void) = 0;

  /**
   * Set object's state stored in given string
   * @param stringified object's state
   */
  virtual void setObjectState(const char* state) = 0;

};

 }; 

#endif /* baciRecoverableObject_h */

