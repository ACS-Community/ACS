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

using namespace acsalarm;
using asiConfigurationConstants::ALARM_SOURCE_NAME;

bool* ACSAlarmSystemInterfaceFactory::m_useACSAlarmSystem = NULL;
maci::Manager_ptr ACSAlarmSystemInterfaceFactory::m_manager = maci::Manager::_nil();
AlarmSystemInterfaceFactory * ACSAlarmSystemInterfaceFactory::m_AlarmSystemInterfaceFactory_p = NULL;
void* ACSAlarmSystemInterfaceFactory::dllHandle = NULL;
ACE_Recursive_Thread_Mutex ACSAlarmSystemInterfaceFactory::main_mutex;
auto_ptr<acsalarm::AlarmSystemInterface> ACSAlarmSystemInterfaceFactory::sharedSource;

/**
 * Create a new instance of an alarm system interface without binding it to any source.
 */
auto_ptr<acsalarm::AlarmSystemInterface> ACSAlarmSystemInterfaceFactory::createSource() throw (acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::createSource entering");

	auto_ptr<acsalarm::AlarmSystemInterface> retVal;
	if (m_useACSAlarmSystem == NULL) {
		throw acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl(__FILE__,__LINE__,"ACSAlarmSystemInterfaceFactory::createSource");
	}
	if (!(*m_useACSAlarmSystem)) {
		retVal = m_AlarmSystemInterfaceFactory_p->createSource();
	} else {
		retVal = createSource("UNDEFINED");
	}

	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::createSource exiting");
	return retVal;
}

/**
 * Create a new instance of an alarm system interface.
 */
auto_ptr<acsalarm::AlarmSystemInterface> ACSAlarmSystemInterfaceFactory::createSource(string sourceName) throw (acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::createSource(string) entering...");

	auto_ptr<acsalarm::AlarmSystemInterface> retVal;
	if (m_useACSAlarmSystem == NULL) {
		throw acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl(__FILE__,__LINE__,"ACSAlarmSystemInterfaceFactory::createSource");
	}
	if (!(*m_useACSAlarmSystem)) {
		retVal = m_AlarmSystemInterfaceFactory_p->createSource(sourceName);
	} else {
		retVal.reset(new ACSAlarmSystemInterfaceProxy(sourceName));
	}

	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::createSource(string) exiting");
	return retVal;
}

/**
 * Getter for whether we're using the ACS Alarm system (true) or not (false).
 */
bool ACSAlarmSystemInterfaceFactory::usingACSAlarmSystem() throw (acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl)
{ 
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::usingACSAlarmSystem() entering");
	bool retVal = true;
	if(NULL == m_useACSAlarmSystem)
	{ 
		throw acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl(__FILE__,__LINE__,"ACSAlarmSystemInterfaceFactory::usingACSAlarmSystem"); 
	}
	else	
	{
		retVal = *m_useACSAlarmSystem; 
	}
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::usingACSAlarmSystem() exiting");
	return retVal;
}

auto_ptr<acsalarm::FaultState>ACSAlarmSystemInterfaceFactory::createFaultState(string family, string member, int code) 
	throw (acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::createFaultState(string, string, int) entering");
	auto_ptr<acsalarm::FaultState> retVal;
	if (m_useACSAlarmSystem==NULL) {
		throw acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl(__FILE__,__LINE__,"ACSAlarmSystemInterfaceFactory::createFaultState(string, string, int)");
	}
	if (!(*m_useACSAlarmSystem)) {
		retVal = m_AlarmSystemInterfaceFactory_p->createFaultState(family, member, code);
	} else {
		retVal.reset(new acsalarm::FaultState(family, member, code));
	}
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::createFaultState(string, string, int) exiting");
	return retVal;
}
	
auto_ptr<acsalarm::FaultState>ACSAlarmSystemInterfaceFactory::createFaultState() throw (acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::createFaultState() entering");
	auto_ptr<acsalarm::FaultState> retVal;
	if (m_useACSAlarmSystem==NULL) {
		throw acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl(__FILE__,__LINE__,"ACSAlarmSystemInterfaceFactory::createFaultState()");
	}
	if (!(*m_useACSAlarmSystem)) {
		retVal = m_AlarmSystemInterfaceFactory_p->createFaultState();
	} else {
		retVal.reset(new acsalarm::FaultState());
	}
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::createFaultState() exiting");
	return retVal;
}

maci::Manager_ptr ACSAlarmSystemInterfaceFactory::getManager()
{ 
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::getManager() entering/exiting");
	return m_manager; 
}

/**
 * Short-hand convenience API to create and send an alarm in a single step.
 */
void ACSAlarmSystemInterfaceFactory::createAndSendAlarm(string & faultFamily, string & faultMember, int faultCode, bool active, string sourceName) 
	throw (acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::createAndSendAlarm(string, string, int, bool, string) entering");

	// create a Properties object and configure it, then assign to the FaultState
	Properties properties;

	ACSAlarmSystemInterfaceFactory::createAndSendAlarm(faultFamily, faultMember, faultCode, active, properties, sourceName);
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::createAndSendAlarm(string, string, int, bool, string) exiting");
}

/**
 * Short-hand convenience API to create and send an alarm in a single step.
 */
void ACSAlarmSystemInterfaceFactory::createAndSendAlarm(string & faultFamily, string & faultMember, int faultCode, bool active, Properties & faultProperties, string sourceName) 
	throw (acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::createAndSendAlarm(string, string, int, bool, Properties, string) entering");

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

	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	if(NULL == sharedSource.get())
	{
		// Send the fault. We must use the "ALARM_SYSTEM_SOURCES" to match the name defined on CDB
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::createAndSendAlarm(string, string, int, bool, Properties, string) creating shared source");
		sharedSource = ACSAlarmSystemInterfaceFactory::createSource(ALARM_SOURCE_NAME);
	}

	// push the FaultState using the source
	ACSAlarmSystemInterfaceFactory::sharedSource->push(*fltstate);

	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::createAndSendAlarm(string, string, int, bool, Properties, string) exiting");
}

// called at shutdown by maciContainer
void ACSAlarmSystemInterfaceFactory::done() 
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::done entering");

	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	cleanUpAlarmSystemInterfacePtr();
	cleanUpSharedSource();
	cleanUpDLL();
	cleanUpBooleanPtr();
	cleanUpManagerReference();
	 
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::done exiting");
}

// private method called at shutdown
void ACSAlarmSystemInterfaceFactory::cleanUpManagerReference()
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::cleanUpManagerReference entering");

	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	if (!CORBA::is_nil(m_manager)) {
		CORBA::release(m_manager);
		m_manager = maci::Manager::_nil();
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::cleanUpManagerReference nilling manager reference");
	}
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::cleanUpManagerReference exiting");
}

// private method called at shutdown
void ACSAlarmSystemInterfaceFactory::cleanUpBooleanPtr()
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::cleanUpBooleanPtr entering");

	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	if(NULL != m_useACSAlarmSystem) 
	{
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::cleanUpBooleanPtr deleting m_useACSAlarmSystem ptr");
		delete m_useACSAlarmSystem;
		m_useACSAlarmSystem = NULL;
	}
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::cleanUpBooleanPtr exiting");
}

// private method called at shutdown
void ACSAlarmSystemInterfaceFactory::cleanUpDLL()
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::cleanUpDLL entering");

	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	if(NULL != ACSAlarmSystemInterfaceFactory::dllHandle)
	{
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::cleanUpDLL closing DLL");
		dlclose(ACSAlarmSystemInterfaceFactory::dllHandle);
		ACSAlarmSystemInterfaceFactory::dllHandle = NULL;
	}
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::cleanUpDLL exiting");
}

// private method called at shutdown
void ACSAlarmSystemInterfaceFactory::cleanUpSharedSource()
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::cleanUpSharedSource entering");

	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	if(NULL != sharedSource.get())
	{
		// force the deletion of the allocated memory for the shared source auto_ptr
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::cleanUpSharedSource nulling out shared source auto_ptr");
		sharedSource.reset();
	}
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::cleanUpSharedSource exiting");
}

// private method called at shutdown
void ACSAlarmSystemInterfaceFactory::cleanUpAlarmSystemInterfacePtr()
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::cleanUpAlarmSystemInterfacePtr entering");

	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	if (NULL != m_useACSAlarmSystem && !(*m_useACSAlarmSystem) && NULL != m_AlarmSystemInterfaceFactory_p) {
		m_AlarmSystemInterfaceFactory_p->done();
		myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::cleanUpAlarmSystemInterfacePtr deleting alarm system interface ptr");
		delete m_AlarmSystemInterfaceFactory_p;
		m_AlarmSystemInterfaceFactory_p = NULL;
	}
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::cleanUpAlarmSystemInterfacePtr exiting");
}

// public method: called at startup by maciContainer
bool ACSAlarmSystemInterfaceFactory::init(maci::Manager_ptr manager) throw (acsErrTypeAlarmSourceFactory::ErrorLoadingCERNDLLExImpl)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::init() entering");

	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	bool retVal = true;

	initImplementationType(manager);

	if (!(*m_useACSAlarmSystem)) 
	{
		retVal = initDLL();
	}

	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::init() exiting");
	return retVal;
}

// private method callled during initialization (at container startup)
void ACSAlarmSystemInterfaceFactory::initImplementationType(maci::Manager_ptr manager)
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::initImplementationType entering");

	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	if (manager!=maci::Manager::_nil()) 
	{
		m_manager = maci::Manager::_duplicate(manager);
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

	// Print a debug message
	if (*m_useACSAlarmSystem) {
		myLoggerSmartPtr->log(Logging::Logger::LM_DEBUG, "Using ACS alarm system");
	} else {
		myLoggerSmartPtr->log(Logging::Logger::LM_DEBUG, "Using CERN alarm system");
	}

	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::initImplementationType exiting");
}

// private method callled during initialization (at container startup), if using CERN alarm style
bool ACSAlarmSystemInterfaceFactory::initDLL()
{
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::initDLL entering");
	bool retVal = true;

	myLoggerSmartPtr->log(Logging::Logger::LM_DEBUG, "ACSAlarmSystemInterfaceFactory::initDLL() loading CERN DLL...");

	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(main_mutex);
	// load the DLL and then set pointer m_AlarmSystemInterfaceFactory_p to point to the object
	// that is returned from the DLL's entry point function. From then on, we can use the pointer/object directly.
	ACSAlarmSystemInterfaceFactory::dllHandle = dlopen(CERN_ALARM_SYSTEM_DLL_PATH, RTLD_NOW|RTLD_GLOBAL);
	if(ACSAlarmSystemInterfaceFactory::dllHandle == NULL)
	{
		string errString = "ACSAlarmSystemInterfaceFactory::initDLL(): could not open DLL; error was:\n\n" + string(dlerror()); 
		myLoggerSmartPtr->log(Logging::Logger::LM_ERROR, errString);
		throw acsErrTypeAlarmSourceFactory::ErrorLoadingCERNDLLExImpl(__FILE__,__LINE__,"ACSAlarmSystemInterfaceFactory::initDLL");
	}
	// Call the well-defined entry point function of the DLL, to get an object 
	// which implements the AlarmSystemInterfaceFactory interface, which will be used for publishing 
	// CERN style alarms (i.e. alarms that go over the notification channel as opposed to just being logged)
	void * publisherFactoryFunctionPtr = dlsym(ACSAlarmSystemInterfaceFactory::dllHandle, CERN_ALARM_SYSTEM_DLL_FUNCTION_NAME);
	m_AlarmSystemInterfaceFactory_p = ((AlarmSystemInterfaceFactory*(*)())(publisherFactoryFunctionPtr))();
	myLoggerSmartPtr->log(Logging::Logger::LM_DEBUG, "ACSAlarmSystemInterfaceFactory::initDLL() successfully loaded DLL");
	retVal = m_AlarmSystemInterfaceFactory_p->init();

	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "ACSAlarmSystemInterfaceFactory::initDLL() exiting.");
	return retVal;
}
