#ifndef ALARM_SYSTEM_INTERFACE_FACTORY_H
#define ALARM_SYSTEM_INTERFACE_FACTORY_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2006 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: AlarmSystemInterfaceFactory.h,v 1.5 2009/10/09 13:07:15 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  2006-08-30  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "AlarmSystemInterface.h"
#include "FaultState.h"
#include "maciS.h"

/**
 * The abstract base class (actually just an interface) to create sources and fault states.
 */
class AlarmSystemInterfaceFactory 
{
	public:

    virtual ~AlarmSystemInterfaceFactory(){}

	/**
	 * Init the object of the class: must be called before using the other
	 * methods of this class otherwise an exception will be thrown.
	 * Return true if the initialization went ok
	 */
	virtual bool init() = 0;
		
	/**
	 * Release the resources: must be called when finished using the
	 * methods of this class
	 */
	virtual void done() = 0;
	
	/**
 	 * Create a new instance of an alarm system interface.
	 * @param sourceName the source name.
	 * @return the interface instance.
	 */
	virtual acsalarm::AlarmSystemInterface* createSource(std::string sourceName) = 0;
		
	/**
	 * Create a new instance of an alarm system interface without binding it to any source.
	 * @return the interface instance.
	 */
	virtual acsalarm::AlarmSystemInterface* createSource() = 0;
	
	/**
	 * Create a fault state with the given family, member and code
	 */
	virtual std::auto_ptr<acsalarm::FaultState>createFaultState(std::string family, std::string member, int code);
	
	/**
	 * Create a fault state 
	 */
	virtual std::auto_ptr<acsalarm::FaultState>createFaultState();
};

#endif /* !ALARM_SYSTEM_INTERFACE_FACTORY_H */

