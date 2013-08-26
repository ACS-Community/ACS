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
#include <ACSErrTypeCORBA.h>
#include <errTypeAlarmService.h>
#include "AcsAlarmPublisher.h"
#include "ACSJMSMessageEntityS.h"
#include <orbsvcs/CosNamingC.h>
#include <logging.h>
#include <acsncHelper.h>
#include <ACSAlarmSystemInterfaceFactory.h>

using acsalarm::ASIMessage;
using nc::Helper;
using laserSource::AcsAlarmPublisher;

CosNaming::NamingContext_var AcsAlarmPublisher::naming_v;

/*
 * Constructor.
 * @param topicName the name of the topic for the notification channel which will
 *        be used for communication with the laser alarm server.
 */
AcsAlarmPublisher::AcsAlarmPublisher(std::string topicName)
{
	ACS_TRACE("AcsAlarmPublisher::AcsAlarmPublisher()");

	ACS_SHORT_LOG((LM_DEBUG,"AcsAlarmPublisher::AcsAlarmPublisher(): about to instantiate the alarm supplier."));
	alarmSupplier = new AlarmSupplier(topicName.c_str());
	ACS_SHORT_LOG((LM_DEBUG,"AcsAlarmPublisher::AcsAlarmPublisher(): instantiated the alarm supplier."));

	// initialize the AlarmSupplier with the naming context
	CosNaming::NamingContext_ptr naming_p=getNamingService();
	alarmSupplier->init(naming_p);

	ACS_SHORT_LOG((LM_DEBUG,"AcsAlarmPublisher::AcsAlarmPublisher(): init called on alarm supplier."));
}

/*
 * Destructor. Cleans up the shared SimpleSupplier instance.
 */
AcsAlarmPublisher::~AcsAlarmPublisher()
{
	ACS_TRACE("AcsAlarmPublisher::~AcsAlarmPublisher()");
	if(NULL != alarmSupplier)
	{
		// disconnect the AlarmSupplier.
		alarmSupplier->disconnect();
		delete(alarmSupplier);
		alarmSupplier = NULL;
	}
}

/*
 * Public method to publish an alarm to the laser alarm server.
 */
void AcsAlarmPublisher::publishAlarm(ASIMessage msg)
{
	ACS_TRACE("AcsAlarmPublisher::publishAlarm()");
	try {
		alarmSupplier->publishEvent(msg);
	} catch (ACSErr::ACSbaseExImpl ex) {
		errTypeAlarmService::AlarmsNotSentExImpl ex2(ex,__FILE__, __LINE__, "AcsAlarmPublisher::publishAlarm");
		throw ex2;
	} catch (...) {
		errTypeAlarmService::AlarmsNotSentExImpl ex(__FILE__, __LINE__, "AcsAlarmPublisher::publishAlarm");
		throw ex;
	}
}

CosNaming::NamingContext_var AcsAlarmPublisher::getNamingService() {
	ACS_TRACE("AcsAlarmPublisher::getNamingService()");
	if (!CORBA::is_nil(naming_v)) {
		return naming_v;
	}
	maci::Manager_ptr mgr = ACSAlarmSystemInterfaceFactory::getManager();
	if (CORBA::is_nil(mgr)) {
		ACSErrTypeCORBA::CORBAReferenceNilExImpl
			ex(__FILE__, __LINE__, "AcsAlarmPublisher::getNamingService");
		ex.setVariable("mgr");
		throw ex;
	}

	CORBA::Object_var namingObj = mgr->get_service(0, "NameService", true);
	if (CORBA::is_nil(namingObj)) {
		ACSErrTypeCORBA::CORBAReferenceNilExImpl
					ex(__FILE__, __LINE__, "AcsAlarmPublisher::getNamingService");
				ex.setVariable("namingObj");
				throw ex;
	}
	try {
		naming_v = CosNaming::NamingContext::_narrow(namingObj.ptr());
	} catch (...) {
		ACSErrTypeCORBA::NarrowFailedExImpl
							ex(__FILE__, __LINE__, "AcsAlarmPublisher::getNamingService");
						ex.setNarrowType("NamingContext");
						throw ex;
	}

	return AcsAlarmPublisher::naming_v;
}

