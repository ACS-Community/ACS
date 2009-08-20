/*******************************************************************************
*     ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration)
*    and Cosylab 2002, All rights reserved
*
*    This library is free software; you can redistribute it and/or
*    modify it under the terms of the GNU Lesser General Public
*    License as published by the Free Software Foundation; either
*    version 2.1 of the License, or (at your option) any later version.
*
*    This library is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*    Lesser General Public License for more details.
*
*    You should have received a copy of the GNU Lesser General Public
*    License along with this library; if not, write to the Free Software
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: loggingACSLog_i.cpp,v 1.12 2009/08/20 21:33:50 javarias Exp $"
*
* who       when        what
* --------  ---------   ----------------------------------------------
*/

#include "loggingACSLog_i.h"
#include "loggingXMLParser.h"
#include <acsutilAnyAide.h>
#include <sstream>

#include <acscommonC.h>
#include "logging_idlC.h"

using namespace loggingXMLParser;

/*****************************************************************/
 ACSLog_i::ACSLog_i (CORBA::ORB_ptr orb,
		  PortableServer::POA_ptr poa,
                  TAO_LogMgr_i &logmgr_i,
                  DsLogAdmin::LogMgr_ptr factory,
		     DsLogAdmin::LogId id):
     TAO_BasicLog_i(orb, poa, logmgr_i, factory, id),
     m_logging_supplier(0)
{


  m_logBin =false; 
  char *acsLogType = getenv("ACS_LOG_BIN");
  if (acsLogType && *acsLogType){
    if(strcmp("true", acsLogType) == 0)
        m_logBin = true; 
  }

  supOutput = ACE_OS::getenv("LOG_SERVICE_SUPPRESS_OUTPUT");
  if (supOutput != NULL){
			 std::cout << "Suppressing output to the Notification Channel. NOT SENDING LOGS" << std::endl;
  }
}


ACSLog_i::~ACSLog_i ()
{
}

void
ACSLog_i::write_recordlist (const DsLogAdmin::RecordList &reclist)
{
    
    if (reclist.length() <= 0)
	return; // return without a squeak.
    
    // Check if supplyer is given
    if (!this->m_logging_supplier)
	return; // return without a squeak.
    
    // Check the operational status.
    if (this->op_state_ == DsLogAdmin::disabled)
	return; // return without a squeak.
    
    // Check if the log is on duty
    // @@ Wait for Comittee ruling on the proper behavior
    DsLogAdmin::AvailabilityStatus avail_stat =
	this->get_availability_status ();
    
    
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
    logging_event.header.fixed_header.event_type.domain_name = CORBA::string_dup(acscommon::LOGGING_DOMAIN);
    logging_event.header.fixed_header.event_type.type_name =  CORBA::string_dup(acscommon::LOGGING_TYPE);
    logging_event.header.fixed_header.event_name = CORBA::string_dup("");
    logging_event.header.variable_header.length (0); // put nothing here
    logging_event.filterable_data.length (0);
   

    if(!m_logBin){
 
        int result;
        ACE_CString XMLtype(size_t(32));
        const ACE_TCHAR * xml;
        
        for (CORBA::ULong i = 0; i < reclist.length (); i++)
        {
        /*    // Check if the log is full.
              if (avail_status_.log_full == 1)
              {
              ACE_THROW (DsLogAdmin::LogFull (num_written));
              }
              else*/
            
         //   CORBA::TCKind kin = AnyAide::getRealType(reclist[i].info); 
           
        reclist[i].info >>= xml; 
        result = XMLParser::parseElementType(xml, XMLtype);
        
        logging_event.remainder_of_body <<= xml;
		  if(supOutput == NULL){
           try{
            m_logging_supplier->send_event (logging_event);
           }catch(::CORBA::TRANSIENT &ex){
              //do nothing the logs will lost
           }
        }
        logStat.receivedLogs++;
        /*
          this->check_threshold_list ();
        */
        
        } // for   
    }else{

        ACSLoggingLog::LogBinaryRecord *record;
 
        for (CORBA::ULong i = 0; i < reclist.length (); i++)
        {

            CORBA::TCKind kin = AnyAide::getRealType(reclist[i].info);
           

            if(kin == CORBA::tk_struct){
            
                reclist[i].info >>= record; 

                logging_event.remainder_of_body <<= *record;
                m_logging_supplier->send_event (logging_event);
            }else if(kin == CORBA::tk_any){
            }else{
               // ACE_OS::printf("Error??");
            }
            
        } // for   

    }
}

