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
* "@(#) $Id$"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2006-07-12  created 
*/

#include "vltPort.h"

static char *rcsId="@(#) $Id$"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "ACSAlarmSystemInterfaceFactory.h"

bool* ACSAlarmSystemInterfaceFactory::m_useACSAlarmSystem=NULL;

/**
 * Create a new instance of an alarm system interface without binding it to any source.
 * @return the interface instance.
 * @throws ASIException if the AlarmSystemInterface instance can not be created.
 */
auto_ptr<laserSource::AlarmSystemInterface> ACSAlarmSystemInterfaceFactory::createSource() 
{
	return createSource("UNDEFINED");
}

bool ACSAlarmSystemInterfaceFactory::cernImplementationRequested() {
	if (m_useACSAlarmSystem!=NULL) {
		return *m_useACSAlarmSystem;
	}
	m_useACSAlarmSystem = new bool();
	ConfigPropertyGetter* pGetter;
	pGetter = new ConfigPropertyGetter();
	string str = pGetter->getProperty("Implementation");
	delete pGetter;
	*m_useACSAlarmSystem= (str=="CERN");
		return *m_useACSAlarmSystem;
}

/**
 * Create a new instance of an alarm system interface.
 * @param sourceName the source name.
 * @return the interface instance.
 * @throws ASIException if the AlarmSystemInterface instance can not be created.
 */
auto_ptr<laserSource::AlarmSystemInterface> ACSAlarmSystemInterfaceFactory::createSource(string sourceName)
{
	std::cout<<"ACSAlarmSystemInterfaceFactory::createSource(<name>)"<<std::endl;
	if (cernImplementationRequested()) {
		std::cout<<"Creating LASER source\n";
		return laserSource::AlarmSystemInterfaceFactory::createSource(sourceName);
	} else {
		std::cout<<"Creating ACS source\n";
		ACSAlarmSystemInterfaceProxy * asIfProxyPtr = new ACSAlarmSystemInterfaceProxy(sourceName);
		auto_ptr<laserSource::AlarmSystemInterface> asIfAutoPtr(asIfProxyPtr);
		return asIfAutoPtr;
	}
}

void ACSAlarmSystemInterfaceFactory::init(maci::Manager_ptr manager) {
}
