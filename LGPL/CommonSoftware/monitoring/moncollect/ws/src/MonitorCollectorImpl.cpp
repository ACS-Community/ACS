/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2009
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
* "@(#) $Id: MonitorCollectorImpl.cpp,v 1.5 2012/02/16 15:19:40 hsommer Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2009-02-11  created
*/

#include "vltPort.h"

static char *rcsId="@(#) $Id: MonitorCollectorImpl.cpp,v 1.5 2012/02/16 15:19:40 hsommer Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "MonitorCollectorImpl.h"
#include "MonitorErr.h"

using namespace TMCDB;
using namespace MonitorErr;


MonitorCollectorImpl::MonitorCollectorImpl(const ACE_CString& name,
			     maci::ContainerServices * containerServices)
    : acscomponent::ACSComponentImpl(name, containerServices),
    numOfComponents_m(0)
{
    AUTO_TRACE("MonitorCollectorImpl::MonitorCollectorImpl");
}//MonitorCollectorImpl

MonitorCollectorImpl::~MonitorCollectorImpl()
{
    AUTO_TRACE("MonitorCollectorImpl::~MonitorCollectorImpl");
}//~MonitorCollectorImpl

void MonitorCollectorImpl::initialize()
{
	AUTO_TRACE("MonitorCollectorImpl::initialize");


	try
	{
		contServ_m = getContainerServices();

		// we register with the controller, which will assign a blobber to this collector
		// @TODO (HSO): Isn't the controller supposed to be a singleton component,
		//       so that we could obtain it by type ("IDL:alma/MonitorArchiver/Controller:1.0") rather than by configured instance name?
		archiveMonitorController_m = contServ_m->getComponentNonSticky<MonitorArchiver::Controller>("ARCHIVE/TMCDB/MONITOR_CONTROL");
		archiveMonitorController_m->registerCollector(name());
	}
	catch(MonitorErr::CollectorRegistrationFailedEx &ex)
	{
		// This is just a dummy impl, after having added the exception to IDL.
		// TODO: deal with this exception, e.g. raise an alarm.
	}
	catch(ACSErr::ACSbaseExImpl &_ex)
	{
		throw;
	}
	catch( CORBA::SystemException &ex )
	{
		ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__, "MonitorCollectorImpl::initialize");
		corbaProblemEx.setMinor(ex.minor());
		corbaProblemEx.setCompletionStatus(ex.completed());
		corbaProblemEx.setInfo(ex._info().c_str());

		throw corbaProblemEx;
	}
	catch(...)
	{
		ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__, "MonitorCollectorImpl::initialize");
		throw uex;
	}//try-catch
}//initialize

void MonitorCollectorImpl::cleanUp()
{
	AUTO_TRACE("MonitorCollectorImpl::cleanUp");
	ACE_Hash_Map_Entry <ACE_CString, MonitorComponent*> *entry;
	ACE_Hash_Map_Iterator <ACE_CString, MonitorComponent*, ACE_Recursive_Thread_Mutex> iter(monitorComponents_m);

	archiveMonitorController_m->deregisterCollector(name());

	for( ;iter.next(entry)!=0; iter.advance() )
		delete entry->int_id_;

}//cleanUp

void MonitorCollectorImpl::registerMonitoredComponentWithSerial (const char * componentName, const char* serialNumber, bool checkCollocation)
{
	AUTO_TRACE("MonitorCollectorImpl::registerMonitoredComponentWithSerial");

	MonitorComponent*mc = 0;
	try{
		mc = registerMonitoredComponent(componentName, checkCollocation);
		if (!mc)
		{
			ACSErrTypeCommon::NullPointerExImpl nex(__FILE__, __LINE__, "MonitorCollectorImpl::registerMonitoredComponentWithSerial");
			nex.setVariable("mc");
			throw nex;
		}

		mc->setDeviceSerialNumber(const_cast<char*>(serialNumber));
	}
	catch(ACSErr::ACSbaseExImpl &_ex)
	{
		if (mc)
		{
			monitorComponents_m.unbind(componentName);
			delete mc;
		}//if

		RegisteringDeviceProblemExImpl ex(_ex, __FILE__, __LINE__, "MonitorCollectorImpl::registerMonitoredComponentWithSerial");
		ex.setDevice(componentName);

		throw ex.getRegisteringDeviceProblemEx();
	}
}//MonitorCollectorImpl::registerMonitoredComponentWithSerial


void MonitorCollectorImpl::registerMonitoredDevice (const char * componentName, const char* serialNumber)
{
	AUTO_TRACE("MonitorCollectorImpl::registerMonitoredDevice");
	//it is depreciated, so we just delegate
	registerCollocatedMonitoredDevice(componentName, serialNumber);
}//MonitorCollectorImpl::registerMonitoredDevice

void MonitorCollectorImpl::registerCollocatedMonitoredDevice (const char * componentName, const char* serialNumber)
{
	AUTO_TRACE("MonitorCollectorImpl::registerCollocatedMonitoredDevice");
	registerMonitoredComponentWithSerial(componentName, serialNumber, true);
}//MonitorCollectorImpl::registerCollocatedMonitoredDevice

void MonitorCollectorImpl::registerNonCollocatedMonitoredDevice (const char * componentName, const char* serialNumber)
{
	AUTO_TRACE("MonitorCollectorImpl::registerNonCollocatedMonitoredDevice");
	registerMonitoredComponentWithSerial(componentName, serialNumber, false);
}//MonitorCollectorImpl::registerNonCollocatedMonitoredDevice

/*****************************/
void MonitorCollectorImpl::registerMonitoredDeviceWithMultipleSerial(const char*componentName, const TMCDB::propertySerialNumberSeq& serialNumbers)
{
	AUTO_TRACE("MonitorCollectorImpl::registerMonitoredDeviceWithMultipleSerial");
	registerCollocatedMonitoredDeviceWithMultipleSerial(componentName, serialNumbers);
}

void MonitorCollectorImpl::registerNonCollocatedMonitoredDeviceWithMultipleSerial(const char*componentName, const TMCDB::propertySerialNumberSeq& serialNumbers)
{
	AUTO_TRACE("MonitorCollectorImpl::registerNonCollocatedMonitoredDeviceWithMultipleSerial");
	registerMonitoredComponentWithMultipleSerial(componentName, serialNumbers, false);
}

void MonitorCollectorImpl::registerCollocatedMonitoredDeviceWithMultipleSerial(const char*componentName, const TMCDB::propertySerialNumberSeq& serialNumbers)
{
	AUTO_TRACE("MonitorCollectorImpl::registerCollocatedMonitoredDeviceWithMultipleSerial");
	registerMonitoredComponentWithMultipleSerial(componentName, serialNumbers, true);
}


void MonitorCollectorImpl::registerMonitoredComponentWithMultipleSerial(const char*componentName, const TMCDB::propertySerialNumberSeq& serialNumbers, bool checkCollocation)
{
	AUTO_TRACE("MonitorCollectorImpl::registerMonitoredComponentWithMultipleSerial");
	MonitorComponent*mc=0;

	try{
		mc = registerMonitoredComponent(componentName, checkCollocation);
		if (!mc)
		{
			ACSErrTypeCommon::NullPointerExImpl nex(__FILE__, __LINE__, "MonitorCollectorImpl::registerMonitoredComponentWithMultipleSerial");
			nex.setVariable("mc");
			throw nex;
		}

		unsigned int len = serialNumbers.length();
		for(unsigned int i=0; i<len; i++)
		{
			mc->setPropertySerialNumber(const_cast<char*>(serialNumbers[i].propertyName.in()), serialNumbers[i].serialNumbers);
		}
	}
	catch(ACSErr::ACSbaseExImpl &_ex)
	{
		if (mc)
		{
			monitorComponents_m.unbind(componentName);
			delete mc;
		}//if

		RegisteringDeviceProblemExImpl ex(_ex, __FILE__, __LINE__, "MonitorCollectorImpl::registerMonitoredComponentWithMultipleSerial");
		ex.setDevice(componentName);

		throw ex.getRegisteringDeviceProblemEx();
	}
}//registerMonitoredDeviceWithMultipleSerial

MonitorComponent* MonitorCollectorImpl::registerMonitoredComponent (const char * componentName, bool checkCollocation)
{
    AUTO_TRACE("MonitorCollectorImpl::registerMonitoredComponent");
    MonitorComponent* mc = 0;

    ACE_GUARD_RETURN (ACE_Recursive_Thread_Mutex, proSect, mcMutex_m, 0); // protection

    if (!monitorComponents_m.find(componentName))
    {
    	DeviceAlreadyRegisteredExImpl ex(__FILE__, __LINE__, "MonitorCollectorImpl::registerMonitoredDevice");
	    ex.setDevice(componentName);

	    throw ex;
    }//if

    try
	{
	ACS::CharacteristicComponent * comp = contServ_m->getComponentNonSticky<ACS::CharacteristicComponent>(componentName);


	if ( checkCollocation && (!comp->_is_collocated()) )
		{
			NotCollocatedComponentExImpl ex(__FILE__, __LINE__, "MonitorCollectorImpl::registerMonitoredDevice");
			ex.setComponent(componentName);
			throw ex;
		}//if

	mc = new MonitorComponent(comp, contServ_m);
	mc->addAllProperties();
	monitorComponents_m.bind(componentName,  mc);
	ACS_LOG(LM_FULL_INFO ,"MonitorCollectorImpl::registerMonitoredDevice", (LM_DEBUG, "Device %s has been registered at pos: %d.",
						componentName, numOfComponents_m));
	numOfComponents_m++;

	return mc;
	}
    catch(ACSErr::ACSbaseExImpl &_ex)
	{
    	/// we did not manage to add a device
    	if (mc) delete mc;
    	throw;
	}
    catch(...)
    {
    	/// we did not manage to add a device
    	if (mc) delete mc;

    	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							"MonitorCollectorImpl::registerMonitoredDevice");

    	throw uex;
	}//try-catch
}//registerMonitoredComponent


void MonitorCollectorImpl::deregisterMonitoredDevice (const char * componentName)
{
    AUTO_TRACE("MonitorCollectorImpl::deregisterMonitoredDevice");
    MonitorComponent* mc=0;

    ACE_GUARD (ACE_Recursive_Thread_Mutex, proSect, mcMutex_m); // protection

    if (monitorComponents_m.find(componentName, mc))
    {
    	DeviceNotRegisteredExImpl ex(__FILE__, __LINE__, "MonitorCollectorImpl::deregisterMonitoredDevice");
    	ex.setDevice(componentName);

    	throw ex.getDeviceNotRegisteredEx();
    }//if
    monitorComponents_m.unbind(componentName);
    numOfComponents_m--;
    if (mc)
    {
			mc->stopMonitoring();
			delete mc;
    }
    else
    {
    	ACSErrTypeCommon::NullPointerExImpl ex(__FILE__, __LINE__, "MonitorCollectorImpl::deregisterMonitoredDevice");
    	ex.setVariable("mc");
    	ex.log(LM_WARNING);
    }//if-else

}//deregisterMonitoredDevice

void MonitorCollectorImpl::startMonitoring (const char * componentName)
{
    AUTO_TRACE("MonitorCollectorImpl::startMonitoring");
    MonitorComponent* mc=0;

    ACE_GUARD (ACE_Recursive_Thread_Mutex, proSect, mcMutex_m); // protection
    if (monitorComponents_m.find(componentName, mc))
        {
    	StartMonitoringProblemExImpl ex(__FILE__, __LINE__, "MonitorCollectorImpl::startMonitoring");
    	ex.setDevice(componentName);

    	throw ex.getStartMonitoringProblemEx();
        }//if
    try
    {
    	mc->startMonitoring();
    }
    catch(ACSErr::ACSbaseExImpl &_ex)
    {

    	StartMonitoringProblemExImpl ex(_ex, __FILE__, __LINE__, "MonitorCollectorImpl::startMonitoring");
    	ex.setDevice(componentName);

    	throw ex.getStartMonitoringProblemEx();
    }
    catch(...)
    {
    	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
    			"MonitorCollectorImpl::startMonitoring");

    	StartMonitoringProblemExImpl ex(uex, __FILE__, __LINE__, "MonitorCollectorImpl::startMonitoring");
    	ex.setDevice(componentName);

    	throw ex.getStartMonitoringProblemEx();
    }//try-catch
 }//startMonitoring

void MonitorCollectorImpl::stopMonitoring (const char * componentName)
{
	AUTO_TRACE("MonitorCollectorImpl::stopMonitoring");
	MonitorComponent* mc=0;

	ACE_GUARD (ACE_Recursive_Thread_Mutex, proSect, mcMutex_m); // protection
	if (monitorComponents_m.find(componentName, mc))
	{
		StopMonitoringProblemExImpl ex(__FILE__, __LINE__, "MonitorCollectorImpl::stopMonitoring");
    	ex.setDevice(componentName);

		throw ex.getStopMonitoringProblemEx();
	}//if
	try
	{
		mc->stopMonitoring();
	}
	catch(...)
	{
		//TBD:: improved catch statement
		ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
				"MonitorCollectorImpl::stopMonitoring");

		StopMonitoringProblemExImpl ex(uex, __FILE__, __LINE__, "MonitorCollectorImpl::stopMonitoring");
		ex.setDevice(componentName);

		throw ex.getStopMonitoringProblemEx();
	}//try-catch
}//stopMonitoring


TMCDB::MonitorDataBlocks * MonitorCollectorImpl::getMonitorData ()
{
    AUTO_TRACE("MonitorCollectorImpl::getMonitorData");

    ACE_Hash_Map_Entry <ACE_CString, MonitorComponent*> *entry;

    ACE_GUARD_RETURN (ACE_Recursive_Thread_Mutex, proSect, mcMutex_m, new TMCDB::MonitorDataBlocks()); // protection

    ACE_Hash_Map_Iterator <ACE_CString, MonitorComponent*, ACE_Recursive_Thread_Mutex> iter(monitorComponents_m);

    TMCDB::MonitorDataBlocks *monitorDataBlock = new TMCDB::MonitorDataBlocks(numOfComponents_m);
    monitorDataBlock->length(numOfComponents_m);

    // we ask all the components for MonitorDataBlock that contains blob data sequence
    for(int i=0; iter.next(entry)!=0; iter.advance() )
    {
        	entry->int_id_->fillSeq();
        	(*monitorDataBlock)[i] = entry->int_id_->getMonitorDataBlock();
        	 i++;

    }//for

    return monitorDataBlock;
}//getMonitorData

void MonitorCollectorImpl::set_archiving_interval(const char* componentName, const char* propertyName, ACS::TimeInterval time)
{
	AUTO_TRACE("MonitorCollectorImpl::set_archiving_interval");
	MonitorComponent* mc=0;
	
	ACE_GUARD (ACE_Recursive_Thread_Mutex, proSect, mcMutex_m); // protection
	
	if (monitorComponents_m.find(componentName, mc)) {
		DeviceNotRegisteredExImpl ex(__FILE__, __LINE__, "MonitorCollectorImpl::set_archiving_interval");
		ex.setDevice(componentName);
		throw ex.getDeviceNotRegisteredEx();
	}//if
	if (mc) {
		mc->set_archiving_interval(propertyName, time);
	} else {
		ACSErrTypeCommon::NullPointerExImpl ex(__FILE__, __LINE__, "MonitorCollectorImpl::deregisterMonitoredDevice");
		ex.setVariable("mc");
		ex.log(LM_WARNING);
	}//if-else
}

void MonitorCollectorImpl::suppress_archiving(const char* componentName, const char* propertyName)
{
	AUTO_TRACE("MonitorCollectorImpl::suppress_archiving");
	MonitorComponent* mc=0;
	
	ACE_GUARD (ACE_Recursive_Thread_Mutex, proSect, mcMutex_m); // protection
	
	if (monitorComponents_m.find(componentName, mc)) {
		DeviceNotRegisteredExImpl ex(__FILE__, __LINE__, "MonitorCollectorImpl::suppress_archiving");
		ex.setDevice(componentName);
		throw ex.getDeviceNotRegisteredEx();
	}//if
	if (mc) {
		mc->suppress_archiving(propertyName);
	} else {
		ACSErrTypeCommon::NullPointerExImpl ex(__FILE__, __LINE__, "MonitorCollectorImpl::suppress_archiving");
		ex.setVariable("mc");
		ex.log(LM_WARNING);
	}//if-else
}

void MonitorCollectorImpl::enable_archiving(const char* componentName, const char* propertyName)
{
	AUTO_TRACE("MonitorCollectorImpl::enable_archiving");
	MonitorComponent* mc=0;
	
	ACE_GUARD (ACE_Recursive_Thread_Mutex, proSect, mcMutex_m); // protection
	
	if (monitorComponents_m.find(componentName, mc)) {
		DeviceNotRegisteredExImpl ex(__FILE__, __LINE__, "MonitorCollectorImpl::enable_archiving");
		ex.setDevice(componentName);
		throw ex.getDeviceNotRegisteredEx();
	}//if
	if (mc) {
		mc->enable_archiving(propertyName);
	} else {
		ACSErrTypeCommon::NullPointerExImpl ex(__FILE__, __LINE__, "MonitorCollectorImpl::enable_archiving");
		ex.setVariable("mc");
		ex.log(LM_WARNING);
	}//if-else
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(MonitorCollectorImpl)
/* ----------------------------------------------------------------*/


/*___oOo___*/
