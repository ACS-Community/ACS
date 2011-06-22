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
#include "faultStateConstants.h"

static char *rcsId="@(#) $Id$"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "ACSAlarmSystemInterfaceFactory.h"
#include "FaultState.h"
#include "asiConfigurationConstants.h"
#include <logging.h>

using asiConfigurationConstants::ALARM_SOURCE_NAME;
using acsalarm::CERN_ALARM_SYSTEM_DLL_PATH;
using acsalarm::CERN_ALARM_SYSTEM_DLL_FUNCTION_NAME;
using std::auto_ptr;
using std::string;
using acsalarm::FaultState;
using acsalarm::AlarmSystemInterface;
using acsalarm::Properties;
using acsalarm::Timestamp;

AlarmSystemType ACSAlarmSystemInterfaceFactory::m_AlarmSystemType = NOT_YET_INITIALIZED;
maci::Manager_ptr ACSAlarmSystemInterfaceFactory::m_manager = maci::Manager::_nil();
AlarmSystemInterfaceFactory * ACSAlarmSystemInterfaceFactory::m_AlarmSystemInterfaceFactory_p = NULL;
void* ACSAlarmSystemInterfaceFactory::dllHandle = NULL;
ACE_Recursive_Thread_Mutex ACSAlarmSystemInterfaceFactory::main_mutex;
acsalarm::AlarmSystemInterface* ACSAlarmSystemInterfaceFactory::m_sourceSingleton_p=NULL;


/**
 * Create a new instance of an alarm system interface without binding it to any source.
 */
acsalarm::AlarmSystemInterface* ACSAlarmSystemInterfaceFactory::createSource() throw (acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl)
{
	ACS_TRACE("ACSAlarmSystemInterfaceFactory::createSource()");
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	return getSourceSingleton();
}

/**
 * Create a new instance of an alarm system interface.
 */
acsalarm::AlarmSystemInterface* ACSAlarmSystemInterfaceFactory::createSource(string sourceName) throw (acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl)
{
	ACS_TRACE("ACSAlarmSystemInterfaceFactory::createSource(string)");
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	return getSourceSingleton();
}

acsalarm::AlarmSystemInterface* ACSAlarmSystemInterfaceFactory::getSourceSingleton() {
	ACS_TRACE("ACSAlarmSystemInterfaceFactory::getSourceSingleton()");
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	if (m_sourceSingleton_p!=NULL) {
		return m_sourceSingleton_p;
	}
	if (m_AlarmSystemType  == CERN_AS) {
		m_sourceSingleton_p = m_AlarmSystemInterfaceFactory_p->createSource(ALARM_SOURCE_NAME);
	} else if  (m_AlarmSystemType  == ACS_AS) {
		m_sourceSingleton_p = new ACSAlarmSystemInterfaceProxy(ALARM_SOURCE_NAME);
	} else {
		// OPS: alarm system not yet initialized
		throw acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl(__FILE__,__LINE__,"ACSAlarmSystemInterfaceFactory::getSourceSingleton");
	}
	return m_sourceSingleton_p;
}

/**
 * Getter for whether we're using the ACS Alarm system (true) or not (false).
 */
AlarmSystemType ACSAlarmSystemInterfaceFactory::usingACSAlarmSystem() throw (acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl)
{ 
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	ACS_TRACE("ACSAlarmSystemInterfaceFactory::usingACSAlarmSystem()");
	if(m_AlarmSystemType  == NOT_YET_INITIALIZED)
	{ 
		throw acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl(__FILE__,__LINE__,"ACSAlarmSystemInterfaceFactory::usingACSAlarmSystem"); 
	}
	return m_AlarmSystemType;
}

auto_ptr<acsalarm::FaultState>ACSAlarmSystemInterfaceFactory::createFaultState(string family, string member, int code) 
	throw (acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl)
{
	ACS_TRACE("ACSAlarmSystemInterfaceFactory::createFaultState(string, string, int)");
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	auto_ptr<acsalarm::FaultState> retVal;
	if (m_AlarmSystemType  == NOT_YET_INITIALIZED) {
		throw acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl(__FILE__,__LINE__,"ACSAlarmSystemInterfaceFactory::createFaultState(string, string, int)");
	}
	if (m_AlarmSystemType  == CERN_AS) {
		retVal = m_AlarmSystemInterfaceFactory_p->createFaultState(family, member, code);
	} else {
		retVal.reset(new acsalarm::FaultState(family, member, code));
	}
	return retVal;
}
	
auto_ptr<acsalarm::FaultState>ACSAlarmSystemInterfaceFactory::createFaultState() throw (acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl)
{
	ACS_TRACE("ACSAlarmSystemInterfaceFactory::createFaultState()");
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	auto_ptr<acsalarm::FaultState> retVal;
	if (m_AlarmSystemType  == NOT_YET_INITIALIZED) {
		throw acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl(__FILE__,__LINE__,"ACSAlarmSystemInterfaceFactory::createFaultState()");
	}
	if (m_AlarmSystemType  == CERN_AS) {
		retVal = m_AlarmSystemInterfaceFactory_p->createFaultState();
	} else {
		retVal.reset(new acsalarm::FaultState());
	}
	return retVal;
}

maci::Manager_ptr ACSAlarmSystemInterfaceFactory::getManager()
{ 
	ACS_TRACE("ACSAlarmSystemInterfaceFactory::getManager()");
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	return m_manager; 
}

/**
 * Short-hand convenience API to create and send an alarm in a single step.
 */
void ACSAlarmSystemInterfaceFactory::createAndSendAlarm(string & faultFamily, string & faultMember, int faultCode, bool active, string sourceName) 
	throw (acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl)
{
	ACS_TRACE("ACSAlarmSystemInterfaceFactory::createAndSendAlarm(string, string, int, bool, string)");
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	// create a Properties object and configure it, then assign to the FaultState
	Properties properties;

	ACSAlarmSystemInterfaceFactory::createAndSendAlarm(faultFamily, faultMember, faultCode, active, properties, sourceName);
}

/**
 * Short-hand convenience API to create and send an alarm in a single step.
 */
void ACSAlarmSystemInterfaceFactory::createAndSendAlarm(string & faultFamily, string & faultMember, int faultCode, bool active, Properties & faultProperties, string sourceName) 
	throw (acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl)
{
	ACS_TRACE("ACSAlarmSystemInterfaceFactory::createAndSendAlarm(string, string, int, bool, Properties, string)");
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	// create the FaultState
	auto_ptr<acsalarm::FaultState> fltstate = ACSAlarmSystemInterfaceFactory::createFaultState(faultFamily, faultMember, faultCode);

	// set the fault state's descriptor
	string stateString;
	if (active) 
	{
		stateString = faultState::ACTIVE_STRING;
	} 
	else 
	{
		stateString = faultState::TERMINATE_STRING;
	}
	fltstate->setDescriptor(stateString);
		
	// create a Timestamp and use it to configure the FaultState
	auto_ptr<Timestamp> tstampAutoPtr(new Timestamp());
	fltstate->setUserTimestamp(tstampAutoPtr);

	// create a Properties object (using copy constructor) and assign to the FaultState 
	Properties * propsPtr = new Properties(faultProperties);
	auto_ptr<Properties> propsAutoPtr(propsPtr);
	fltstate->setUserProperties(propsAutoPtr);

	// push the FaultState using the source
	getSourceSingleton()->push(*fltstate);
}

// called at shutdown by maciContainer
void ACSAlarmSystemInterfaceFactory::done() 
{
	ACS_TRACE("ACSAlarmSystemInterfaceFactory::done()");

	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	cleanUpAlarmSystemInterfacePtr();
	cleanUpSourceSingleton();
	cleanUpDLL();
	cleanUpManagerReference();
}

// private method called at shutdown
void ACSAlarmSystemInterfaceFactory::cleanUpManagerReference()
{
	ACS_TRACE("ACSAlarmSystemInterfaceFactory::cleanUpManagerReference()");
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	if (!CORBA::is_nil(m_manager)) {
		CORBA::release(m_manager);
		m_manager = maci::Manager::_nil();
	}
}

// private method called at shutdown
void ACSAlarmSystemInterfaceFactory::cleanUpDLL()
{
	ACS_TRACE("ACSAlarmSystemInterfaceFactory::cleanUpDLL()");

	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	if(NULL != ACSAlarmSystemInterfaceFactory::dllHandle)
	{
		dlclose(ACSAlarmSystemInterfaceFactory::dllHandle);
		ACSAlarmSystemInterfaceFactory::dllHandle = NULL;
	}
}

// private method called at shutdown
void ACSAlarmSystemInterfaceFactory::cleanUpSourceSingleton()
{
	ACS_TRACE("ACSAlarmSystemInterfaceFactory::cleanUpSharedSource()");

	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	if(m_sourceSingleton_p!=NULL)
	{
		// force the deletion of the allocated memory for the shared source auto_ptr
		delete m_sourceSingleton_p;
		m_sourceSingleton_p=NULL;
	}
}

// private method called at shutdown
void ACSAlarmSystemInterfaceFactory::cleanUpAlarmSystemInterfacePtr()
{
	ACS_TRACE("ACSAlarmSystemInterfaceFactory::cleanUpAlarmSystemInterfacePtr()");

	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	if (m_AlarmSystemType==CERN_AS && NULL != m_AlarmSystemInterfaceFactory_p) {
		m_AlarmSystemInterfaceFactory_p->done();
		delete m_AlarmSystemInterfaceFactory_p;
		m_AlarmSystemInterfaceFactory_p = NULL;
	}
}

// public method: called at startup by maciContainer
bool ACSAlarmSystemInterfaceFactory::init(maci::Manager_ptr manager) throw (acsErrTypeAlarmSourceFactory::ErrorLoadingCERNDLLExImpl)
{
	ACS_TRACE("ACSAlarmSystemInterfaceFactory::init()");

	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	if (m_AlarmSystemType!=NOT_YET_INITIALIZED) {
		return true;
	}
	bool retVal = true;

	initImplementationType(manager);

	if (m_AlarmSystemType==CERN_AS)
	{
		retVal = initDLL();
	}

	return retVal;
}

// private method called during initialization (at container startup)
void ACSAlarmSystemInterfaceFactory::initImplementationType(maci::Manager_ptr manager)
{
	ACS_TRACE("ACSAlarmSystemInterfaceFactory::initImplementationType(maci::Manager_ptr)");

	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	if (manager!=maci::Manager::_nil()) 
	{
		m_manager = maci::Manager::_duplicate(manager);
		try
		{
			ConfigPropertyGetter pGetter(m_manager);
			string str = pGetter.getProperty("Implementation");
			if (str=="CERN")
			{
				m_AlarmSystemType=CERN_AS;
			}
			else
			{
				m_AlarmSystemType=ACS_AS;
			}
		}
		catch(...)
		{
			// if we get any exception from accessing CDB we use the default ACS alarm system
			m_AlarmSystemType=ACS_AS;
		}
	} 
	else 
	{
		// if we were passed a NULL for the manager reference, this means we should use the ACS (logging) style for alarms
		// this typically will only happen in test code within the acsalarm module, which due to build/dependency order issues
		// cannot access things in the ACSLaser package (which is built later). 
		m_AlarmSystemType=ACS_AS;
	}

	// Print a debug message
	if (m_AlarmSystemType==ACS_AS) {
		ACS_SHORT_LOG((LM_DEBUG, "Using ACS alarm system"));
	} else if (m_AlarmSystemType==CERN_AS) {
		ACS_SHORT_LOG((LM_DEBUG, "Using CERN alarm system"));
	} else {
		ACS_SHORT_LOG((LM_WARNING, "Alarm system not initialized"));
	}
}

// private method callled during initialization (at container startup), if using CERN alarm style
bool ACSAlarmSystemInterfaceFactory::initDLL()
{
	ACS_TRACE("ACSAlarmSystemInterfaceFactory::initDLL()");
	bool retVal = true;

	ACS_SHORT_LOG((LM_DEBUG, "ACSAlarmSystemInterfaceFactory::initDLL() loading CERN DLL..."));

	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	// load the DLL and then set pointer m_AlarmSystemInterfaceFactory_p to point to the object
	// that is returned from the DLL's entry point function. From then on, we can use the pointer/object directly.
	ACSAlarmSystemInterfaceFactory::dllHandle = dlopen(CERN_ALARM_SYSTEM_DLL_PATH, RTLD_NOW|RTLD_GLOBAL);
	if(ACSAlarmSystemInterfaceFactory::dllHandle == NULL)
	{
		string errString = "ACSAlarmSystemInterfaceFactory::initDLL(): could not open DLL; error was:\n\n" + string(dlerror());
		ACS_SHORT_LOG((LM_ERROR, errString.c_str()));
		throw acsErrTypeAlarmSourceFactory::ErrorLoadingCERNDLLExImpl(__FILE__,__LINE__,"ACSAlarmSystemInterfaceFactory::initDLL");
	}
	// Call the well-defined entry point function of the DLL, to get an object 
	// which implements the AlarmSystemInterfaceFactory interface, which will be used for publishing 
	// CERN style alarms (i.e. alarms that go over the notification channel as opposed to just being logged)
	void * publisherFactoryFunctionPtr = dlsym(ACSAlarmSystemInterfaceFactory::dllHandle, CERN_ALARM_SYSTEM_DLL_FUNCTION_NAME);
	m_AlarmSystemInterfaceFactory_p = ((AlarmSystemInterfaceFactory*(*)())(publisherFactoryFunctionPtr))();
	ACS_SHORT_LOG((LM_DEBUG, "ACSAlarmSystemInterfaceFactory::initDLL() successfully loaded DLL"));
	retVal = m_AlarmSystemInterfaceFactory_p->init();

	return retVal;
}
