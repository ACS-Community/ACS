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

#include "ConfigPropertyGetter.h"

static char *rcsId="@(#) $Id$"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "ACSAlarmSystemInterfaceFactory.h"

bool* ACSAlarmSystemInterfaceFactory::m_useACSAlarmSystem=NULL;
maci::Manager_ptr ACSAlarmSystemInterfaceFactory::m_manager=maci::Manager::_nil();

void ACSAlarmSystemInterfaceFactory::done() {
	delete m_useACSAlarmSystem;
	m_useACSAlarmSystem=NULL;
	CORBA::release(m_manager);
	m_manager=maci::Manager::_nil();
}

/**
 * Create a new instance of an alarm system interface without binding it to any source.
 * @return the interface instance.
 * @throws ASIException if the AlarmSystemInterface instance can not be created.
 */
auto_ptr<laserSource::AlarmSystemInterface> ACSAlarmSystemInterfaceFactory::createSource() 
{
	return createSource("UNDEFINED");
}

/**
 * Create a new instance of an alarm system interface.
 * @param sourceName the source name.
 * @return the interface instance.
 * @throws ASIException if the AlarmSystemInterface instance can not be created.
 */
auto_ptr<laserSource::AlarmSystemInterface> ACSAlarmSystemInterfaceFactory::createSource(string sourceName)
{
	if (m_useACSAlarmSystem==NULL) {
		throw acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl(__FILE__,__LINE__,"ACSAlarmSystemInterfaceFactory::createSource");
	}
	if (!(*m_useACSAlarmSystem)) {
		return laserSource::AlarmSystemInterfaceFactory::createSource(sourceName);
	} else {
		ACSAlarmSystemInterfaceProxy * asIfProxyPtr = new ACSAlarmSystemInterfaceProxy(sourceName);
		auto_ptr<laserSource::AlarmSystemInterface> asIfAutoPtr(asIfProxyPtr);
		return asIfAutoPtr;
	}
}

bool ACSAlarmSystemInterfaceFactory::init(maci::Manager_ptr manager) {
	if (CORBA::is_nil(manager)) {
		throw acsErrTypeAlarmSourceFactory::InavalidManagerExImpl(__FILE__,__LINE__,"ACSAlarmSystemInterfaceFactory::init");
	}
	m_manager=maci::Manager::_duplicate(manager);
	m_useACSAlarmSystem = new bool(); // It implicitly says that the init has been called
	ConfigPropertyGetter* pGetter;
	pGetter = new ConfigPropertyGetter(m_manager);
	string str = pGetter->getProperty("Implementation");
	delete pGetter;
	*m_useACSAlarmSystem = !(str=="CERN");
	return true;
}
