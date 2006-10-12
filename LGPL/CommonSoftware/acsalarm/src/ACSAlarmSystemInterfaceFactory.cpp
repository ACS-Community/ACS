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
#include "ACSFaultState.h"
#include <logging.h>

using namespace laserUtil;

bool* ACSAlarmSystemInterfaceFactory::m_useACSAlarmSystem=NULL;
maci::Manager_ptr ACSAlarmSystemInterfaceFactory::m_manager=maci::Manager::_nil();
AbstractAlarmSystemInterfaceFactory * ACSAlarmSystemInterfaceFactory::m_AlarmSystemInterfaceFactory_p = NULL;

void ACSAlarmSystemInterfaceFactory::done() {
	if (m_manager!=maci::Manager::_nil()) {
		CORBA::release(m_manager);
		m_manager=maci::Manager::_nil();
	}
	 
	if (NULL != m_useACSAlarmSystem && !(*m_useACSAlarmSystem) && NULL != m_AlarmSystemInterfaceFactory_p) {
		m_AlarmSystemInterfaceFactory_p->done();
		delete m_AlarmSystemInterfaceFactory_p;
		m_AlarmSystemInterfaceFactory_p = NULL;
	}
	if(NULL != m_useACSAlarmSystem) 
	{
		delete m_useACSAlarmSystem;
		m_useACSAlarmSystem=NULL;
	}
}

/**
 * Create a new instance of an alarm system interface without binding it to any source.
 * @return the interface instance.
 * @throws ASIException if the AlarmSystemInterface instance can not be created.
 */
auto_ptr<laserSource::ACSAlarmSystemInterface> ACSAlarmSystemInterfaceFactory::createSource() 
{
	if (m_useACSAlarmSystem == NULL) {
		throw acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl(__FILE__,__LINE__,"ACSAlarmSystemInterfaceFactory::createSource");
	}
	if (!(*m_useACSAlarmSystem)) {
		return m_AlarmSystemInterfaceFactory_p->createSource();
	} else {
		return createSource("UNDEFINED");
	}
}

/**
 * Create a new instance of an alarm system interface.
 * @param sourceName the source name.
 * @return the interface instance.
 * @throws ASIException if the AlarmSystemInterface instance can not be created.
 */
auto_ptr<laserSource::ACSAlarmSystemInterface> ACSAlarmSystemInterfaceFactory::createSource(string sourceName)
{
	if (m_useACSAlarmSystem==NULL) {
		throw acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl(__FILE__,__LINE__,"ACSAlarmSystemInterfaceFactory::createSource");
	}
	if (!(*m_useACSAlarmSystem)) {
		return m_AlarmSystemInterfaceFactory_p->createSource(sourceName);
	} else {
		ACSAlarmSystemInterfaceProxy * asIfProxyPtr = new ACSAlarmSystemInterfaceProxy(sourceName);
		auto_ptr<laserSource::ACSAlarmSystemInterface> asIfAutoPtr(asIfProxyPtr);
		return asIfAutoPtr;
	}
}

bool ACSAlarmSystemInterfaceFactory::init(maci::Manager_ptr manager) 
{
	if (manager!=maci::Manager::_nil()) 
	{
		m_manager=maci::Manager::_duplicate(manager);
		m_useACSAlarmSystem = new bool(); // It implicitly says that the init has been called
		try
		{
			ConfigPropertyGetter pGetter(m_manager);
			string str = pGetter.getProperty("Implementation");
			*m_useACSAlarmSystem = !(str=="CERN");
		}
		catch(...)
		{
			// if we get any exception from accessing CDB we use the default ACS alarm system
			*m_useACSAlarmSystem=true;
		}
	} 
	else 
	{
		// if we were passed a NULL for the manager reference, this means we should use the ACS (logging) style for alarms
		// this typically will only happen in test code within the acsalarm module, which due to build/dependency order issues
		// cannot access things in the ACSLaser package (which is built later). 
		m_useACSAlarmSystem = new bool();
		*m_useACSAlarmSystem=true;
	}
	if (!(*m_useACSAlarmSystem)) 
	{
		Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
		// load the DLL and then set pointer m_AlarmSystemInterfaceFactory_p to point to the object
		// that is returned from the DLL's entry point function. From then on, we can use the pointer/object directly.
		void *hndl = dlopen(CERN_ALARM_SYSTEM_DLL_PATH, RTLD_NOW|RTLD_GLOBAL);
		if(hndl == NULL)
		{
			string errString = "ACSAlarmSystemInterfaceFactory::init(): could not open DLL; error was:\n\n" + string(dlerror()); 
			myLoggerSmartPtr->log(Logging::Logger::LM_ERROR, errString);
			throw acsErrTypeAlarmSourceFactory::ErrorLoadingCERNDLLExImpl(__FILE__,__LINE__,"ACSAlarmSystemInterfaceFactory::init");
		}
		// Call the well-defined entry point function of the DLL, to get an object 
		// which implements the AbstractAlarmSystemInterfaceFactory interface, which will be used for publishing 
		// CERN style alarms (i.e. alarms that go over the notification channel as opposed to just being logged)
		void * publisherFactoryFunctionPtr = dlsym(hndl, CERN_ALARM_SYSTEM_DLL_FUNCTION_NAME);
		m_AlarmSystemInterfaceFactory_p = ((AbstractAlarmSystemInterfaceFactory*(*)())(publisherFactoryFunctionPtr))();
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::init() successfully loaded DLL");
		return m_AlarmSystemInterfaceFactory_p->init();
	}
	return true;
}

auto_ptr<laserSource::ACSFaultState>ACSAlarmSystemInterfaceFactory::createFaultState(string family, string member, int code) {
	if (m_useACSAlarmSystem==NULL) {
		throw acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl(__FILE__,__LINE__,"ACSAlarmSystemInterfaceFactory::createSource");
	}
	if (!(*m_useACSAlarmSystem)) {
		return m_AlarmSystemInterfaceFactory_p->createFaultState(family, member, code);
	} else {
		laserSource::ACSFaultState * asFaultStatePtr = new laserSource::ACSFaultState(family, member, code);
		auto_ptr<laserSource::ACSFaultState> asFaultStateAutoPtr(asFaultStatePtr);
		return asFaultStateAutoPtr;
	}
}
	
/**
 * Getter for whether we're using the ACS Alarm system (true) or not (false).
 */
bool ACSAlarmSystemInterfaceFactory::usingACSAlarmSystem()
{ 
	bool retVal = true;
	if(NULL == m_useACSAlarmSystem)
	{ 
		throw acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl(__FILE__,__LINE__,"ACSAlarmSystemInterfaceFactory::usingACSAlarmSystem"); 
	}
	else	
	{
		retVal = *m_useACSAlarmSystem; 
	}
	return retVal;
}

auto_ptr<laserSource::ACSFaultState>ACSAlarmSystemInterfaceFactory::createFaultState() {
	if (m_useACSAlarmSystem==NULL) {
		throw acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl(__FILE__,__LINE__,"ACSAlarmSystemInterfaceFactory::createSource");
	}
	if (!(*m_useACSAlarmSystem)) {
		return m_AlarmSystemInterfaceFactory_p->createFaultState();
	} else {
		laserSource::ACSFaultState * asIfProxyPtr = new laserSource::ACSFaultState();
		auto_ptr<laserSource::ACSFaultState> asIfAutoPtr(asIfProxyPtr);
		return asIfAutoPtr;
	}
}
