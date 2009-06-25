#include <typeinfo>
#include <iostream>

#include <acscommonC.h>
#include <acsutilAnyAide.h>

#include "loggingAcsLogServiceImpl.h"


AcsLogServiceImpl::AcsLogServiceImpl(CORBA::ORB_ptr orb,
		PortableServer::POA_ptr poa,
		TAO_LogMgr_i &logmgr_i,
		DsLogAdmin::LogMgr_ptr factory,
		DsLogAdmin::LogId id) : ACSLog_i(orb, poa, logmgr_i, factory, id)
{
}

AcsLogServiceImpl::~AcsLogServiceImpl()
{
}

void AcsLogServiceImpl::writeRecords (const Logging::XmlLogRecordSeq &reclist)
{
	
	if (reclist.length() <= 0)
		return;

	// Check if supplyer is given
	if (!this->m_logging_supplier)
		return;

	// Check the operational status.
	if (this->op_state_ == DsLogAdmin::disabled)
		return;

	// Check if the log is on duty
	// @@ Wait for Comittee ruling on the proper behavior
	DsLogAdmin::AvailabilityStatus avail_stat =
		this->get_availability_status ();

	//I don't know what do this code, but I put anyway...
	if (avail_stat.off_duty == 1)
	{

		// why are we off duty? investigate ...
		// Check if the log is full.
		if (avail_stat.log_full == 1)
		{
			throw DsLogAdmin::LogFull (0);
		}
		else   // Check the administrative state.
			if (this->get_administrative_state() == DsLogAdmin::locked)
			{
				throw DsLogAdmin::LogLocked ();
			}
			else
				return; // we are not scheduled at this time.
	}

	CosNotification::StructuredEvent logging_event;
	logging_event.header.fixed_header.event_type.domain_name = 
		CORBA::string_dup(acscommon::LOGGING_DOMAIN);
	logging_event.header.fixed_header.event_type.type_name =  
		CORBA::string_dup(acscommon::LOGGING_TYPE);
	logging_event.header.fixed_header.event_name = CORBA::string_dup("");
	logging_event.header.variable_header.length (0); // put nothing here
	logging_event.filterable_data.length (0);

	for (CORBA::ULong i = 0; i < reclist.length (); i++)
	{
		logging_event.remainder_of_body <<= reclist[i].xml;
		if(supOutput == NULL)
		   m_logging_supplier->send_event (logging_event);
		logStat.receivedLogs++;
	}
}

Logging::LogStatistics AcsLogServiceImpl::getStatistics()
{
	return logStat;
}
