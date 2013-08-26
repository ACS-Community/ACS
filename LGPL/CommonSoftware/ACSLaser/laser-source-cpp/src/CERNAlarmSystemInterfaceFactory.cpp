/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) National Research Council of Canada, 2005
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
*/
#include "AlarmSystemInterface.h"
#include "CERNAlarmSystemInterfaceProxy.h"
#include "CERNAlarmSystemInterfaceFactory.h"
#include "asiConfigurationConstants.h"

using asiConfigurationConstants::ALARM_SOURCE_NAME;
using std::string;
using std::auto_ptr;
using laserSource::CERNAlarmSystemInterfaceFactory;
using acsalarm::AlarmSystemInterface;

/**
 * Constructor
 */
CERNAlarmSystemInterfaceFactory::CERNAlarmSystemInterfaceFactory()
{
	ACS_TRACE("CERNAlarmSystemInterfaceFactory::constructor()");
}

/**
 * Destructor
 */
CERNAlarmSystemInterfaceFactory::~CERNAlarmSystemInterfaceFactory()
{
	ACS_TRACE("CERNAlarmSystemInterfaceFactory::destructor()");
	done();
}

/**
 * Init the object of the class: must be called before using the other
 * methods of this class otherwise an exception will be thrown.
 * Return true if the initialization went ok
 */
bool CERNAlarmSystemInterfaceFactory::init()
{
	ACS_TRACE("CERNAlarmSystemInterfaceFactory::init()");
	return true;
}
	
/**
 * Release the resources: must be called when finished using the
 * methods of this class
 */
void CERNAlarmSystemInterfaceFactory::done()
{
	ACS_TRACE("CERNAlarmSystemInterfaceFactory::done()");
}

/**
 * Create a new instance of an alarm system interface.
 * @param sourceName the source name.
 * @return the interface instance.
 * @throws ASIException if the AlarmSystemInterface instance can not be created.
 */
AlarmSystemInterface* CERNAlarmSystemInterfaceFactory::createSource(string sourceName)
{
	ACS_TRACE("CERNAlarmSystemInterfaceFactory::createSource()");
	return new CERNAlarmSystemInterfaceProxy(sourceName);
}
	
/**
 * Create a new instance of an alarm system interface binding it to the default source source.
 * @return the interface instance.
 * @throws ASIException if the AlarmSystemInterface instance can not be created.
 */
AlarmSystemInterface* CERNAlarmSystemInterfaceFactory::createSource()
{
	ACS_TRACE("CERNAlarmSystemInterfaceFactory::createSource()");
	return createSource(ALARM_SOURCE_NAME);
}

/*
 * Simple factory method to return an instance of CERNAlarmSystemInterfaceFactory as an entry point 
 * for clients of the shared library. 
 */
extern "C" 
{
	CERNAlarmSystemInterfaceFactory * getAlarmSystemInterfaceFactory()
	{
		ACS_TRACE("CERNAlarmSystemInterfaceFactory::getAlarmSystemInterface()");
		CERNAlarmSystemInterfaceFactory * retVal = new CERNAlarmSystemInterfaceFactory();
		return retVal;
	}
};

