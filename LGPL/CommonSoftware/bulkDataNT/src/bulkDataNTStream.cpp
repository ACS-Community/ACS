/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2011
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
* "@(#) $Id: bulkDataNTStream.cpp,v 1.49 2013/02/06 23:12:01 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/
#include "bulkDataNTStream.h"
#include <iostream>
using namespace ACS_BD_Errors;
using namespace ACSErrTypeCommon;
using namespace ACS_DDS_Errors;

using namespace AcsBulkdata;

unsigned int BulkDataNTStream::factoryRefCount_m = 0;
DDS::DomainParticipantFactory *BulkDataNTStream::factory_m=0;

unsigned int BulkDataNTStream::globalParticipantRefCount_m = 0;
DDS::DomainParticipant* BulkDataNTStream::globalParticipant_m=0;

BulkDataNTStream::BulkDataNTStream(const char* name, const StreamConfiguration &cfg) :
	    streamName_m(name), configuration_m(cfg), participant_m(0)
{
  AUTO_TRACE(__PRETTY_FUNCTION__);

  try
  {

	  //TBD: Maybe factory and global DDS stream should be treated with singleton, but at least for global stream there is no default ctor
	  {//critical section
		  // Maybe we should wait with timeout here
		  ACE_GUARD (ACE_Recursive_Thread_Mutex, protDDSFActory, DDSFactoryMutex_m); // protection

		  if (factory_m==0)	  createDDSFactory();  //it is enough to have one factory
		  factoryRefCount_m++;
	  }//end of critical section

	  addDDSQoSProfile(configuration_m);  //add QoS profile for stream (and flows)

	  if (configuration_m.isParticipantPerStream())
	  {
		  participant_m = createDDSParticipant(); //we have a participant per stream
	  }
	  else
	  {
		  {//Beginning of critical section
			  // Maybe we should wait with timeout here
			  ACE_GUARD (ACE_Recursive_Thread_Mutex, protGlobalPart, globalPartMutex_m); // protection
			  // for performance reason (number of threads) is better to have just one participant, but it might be good to have possibility to create
			  // also a participant per stream
			  if (BulkDataNTStream::globalParticipantRefCount_m==0 && BulkDataNTStream::globalParticipant_m==0)
			  {
				  participant_m = BulkDataNTStream::globalParticipant_m = createDDSParticipant();
			  }
			  else
			  {
				  participant_m = BulkDataNTStream::globalParticipant_m;
			  }
			  BulkDataNTStream::globalParticipantRefCount_m++;   // but we need to know how many users of participant do we have
		  }//end critical section
		  ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_DEBUG, "Going to use global participant for stream: %s.", streamName_m.c_str()));
	  }//if-else (configuration_m.participantPerStream)

	  ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_DEBUG, "Stream: %s has been created.", streamName_m.c_str()));
  }catch(const ACSErr::ACSbaseExImpl &e)
  {
	  destroyDDSFactory();
	  StreamCreateProblemExImpl ex (e, __FILE__, __LINE__, __PRETTY_FUNCTION__);
	  ex.setStreamName(name);
	  throw ex;
  }//try-catch
}//BulkDataNTStream


BulkDataNTStream::~BulkDataNTStream()
{
  AUTO_TRACE(__PRETTY_FUNCTION__);

  if (configuration_m.participantPerStream)
  {
	  destroyDDSParticipant(participant_m);
  }else
  {
	  ACE_GUARD (ACE_Recursive_Thread_Mutex, protGlobalPart, globalPartMutex_m); // protection
	  BulkDataNTStream::globalParticipantRefCount_m--;
	  if (BulkDataNTStream::globalParticipantRefCount_m==0)
	  {
		  ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_DEBUG, "Going to destroy global participant."));
		  destroyDDSParticipant(BulkDataNTStream::globalParticipant_m);
		  BulkDataNTStream::globalParticipant_m=0;
	  }
  }//if-else

  removeDDSQoSProfile(configuration_m);  //remove QoS profile for stream

  participant_m=0;
  {//critical section
	  // Maybe we should wait with timeout here
	  ACE_GUARD (ACE_Recursive_Thread_Mutex, protDDSFActory, DDSFactoryMutex_m); // protection
	  destroyDDSFactory(); // we do not need to call, but .... it just decrease the counter

  }//end of critical section

}//~BulkDataNTStream


void BulkDataNTStream::createDDSFactory()
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	DDS::ReturnCode_t ret;
	DDS::DomainParticipantFactoryQos factory_qos;

	factory_m = DDS::DomainParticipantFactory::get_instance();
	//factoryRefCount_m++; //anyway we have to increase the counter that we know when to call finalize_instance,
	// ... so it must be right after call get_instance call

	// needed by RTI only
	ret = factory_m->get_qos(factory_qos);
	if (ret!=DDS::RETCODE_OK)
	{
		DDSQoSSetProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDDSTypeCode(ret);
		ex.setQoS("factory_m->get_qos");
		throw ex;
	}//if


	factory_qos.entity_factory.autoenable_created_entities = DDS_BOOLEAN_FALSE;

	// if both values are true (default) we prevent that default values are taken from default places
	factory_qos.profile.ignore_user_profile = DDSConfiguration::ignoreUserProfileQoS;
	factory_qos.profile.ignore_environment_profile = DDSConfiguration::ignoreEnvironmentProfileQoS;
	factory_qos.resource_limits.max_objects_per_thread = 4096; //if we want to have more than 10 participants (at the moment can not be set in XML)

	if (DDSConfiguration::urlProfileQoS.length()>0)
	{
		factory_qos.profile.url_profile.ensure_length(1,1);
		factory_qos.profile.url_profile[0] = DDS_String_dup(DDSConfiguration::urlProfileQoS.c_str());
	}//if

	ret = factory_m->set_qos(factory_qos);
	if (ret!=DDS::RETCODE_OK)
	{
		DDSQoSSetProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDDSTypeCode(ret);
		ex.setQoS("factory_m->set_qos");
		throw ex;
	}//if

/*	DDS_StringSeq profile_names;
	ret = factory_m->get_qos_profiles(profile_names, "BulkDataQoSLibrary");
	printf("profile length: %d\n", profile_names.length());
*/

}//createDDSFactory


void BulkDataNTStream::destroyDDSFactory()
{
  // if factory_m is not NULL means that get_instance was successful , and that factoryRefCount_m was also increased
  if(factory_m!=0)
    {
      factoryRefCount_m--;
     //TBD: cause problem if all streams are deleted and new is created   if (factoryRefCount_m==0)  DDS::DomainParticipantFactory::finalize_instance();
    }
}//destroyDDSFactory

DDS::DomainParticipant* BulkDataNTStream::createDDSParticipant()
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	DDS::ReturnCode_t ret;
	DDS::DomainParticipantQos participant_qos;
	int domainID=0; //TBD: where to get domain ID
	DDS::DomainParticipant* domainParticipant;

	if (factory_m==NULL)
	{
		NullPointerExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setVariable("factory_m");
		throw ex;
	}

	// If the profileQoS set on the configuration corresponds to a default stream profile,
	// then we use the default library as the library to search for it.
	// Otherwise, it means that the user specified it, so it must belong
	// to the specified library too
	const char *library = 0;
	if( configuration_m.libraryQos.compare((const char*)DDSConfiguration::DEFAULT_LIBRARY) == 0 &&
		(configuration_m.profileQos.compare((const char*)DDSConfiguration::DEFAULT_RECEIVER_STREAM_PROFILE) == 0 ||
	    configuration_m.profileQos.compare((const char*)DDSConfiguration::DEFAULT_SENDER_STREAM_PROFILE) == 0)
		)
		library = DDSConfiguration::DEFAULT_LIBRARY;
	else
		library = configuration_m.libraryQos.c_str();

	ret = factory_m->get_participant_qos_from_profile(participant_qos, library, configuration_m.profileQos.c_str());
	if (ret!=DDS::RETCODE_OK)
	{
		DDSQoSSetProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDDSTypeCode(ret);
		ex.setQoS("get_participant_qos_from_profile");
		throw ex;
	}//if

	// we make sure that what participant creates is created disabled
	participant_qos.entity_factory.autoenable_created_entities=DDS_BOOLEAN_FALSE;

	domainParticipant =factory_m->create_participant(domainID, participant_qos, NULL, DDS::STATUS_MASK_NONE );
	if (domainParticipant==NULL)
	{
		DDSParticipantCreateProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDomainID(domainID);
		throw ex;
	}

	ret = domainParticipant->enable();
	if (ret!=DDS::RETCODE_OK)
	  {
	    DDSParticipantEnableProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
	    ex.setDDSTypeCode(ret);
	    ex.setDomainID(domainID);
	    throw ex;
	  }//if

	const char* type_name = ACSBulkData::BulkDataNTFrameTypeSupport::get_type_name();
	ret = ACSBulkData::BulkDataNTFrameTypeSupport::register_type(domainParticipant, type_name);
	if (ret != DDS::RETCODE_OK)
	{
		DDSRegisterTypeProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDDSTypeCode(ret);
		ex.setTypeName(type_name);
		throw ex;
	}

	return domainParticipant;
}//createDDSParticipant

void BulkDataNTStream::destroyDDSParticipant(DDS::DomainParticipant* domainParticipant)
{
	DDS::ReturnCode_t ret;
	AUTO_TRACE(__PRETTY_FUNCTION__);

	const char* type_name = ACSBulkData::BulkDataNTFrameTypeSupport::get_type_name();
	ret = ACSBulkData::BulkDataNTFrameTypeSupport::unregister_type(domainParticipant, type_name);
	if (ret != DDS::RETCODE_OK)
	{
		DDSUnregisterTypeProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDDSTypeCode(ret);
		ex.setTypeName(type_name);
		throw ex;
	}

	ret = factory_m->delete_participant(domainParticipant);
	if (ret != DDS_RETCODE_OK)
	{
		DDSParticipantDestroyProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.log();
	}//if
	domainParticipant=0;
}//destroyDDSParticipant


void BulkDataNTStream::addDDSQoSProfile(const DDSConfiguration &cfg)
{
  DDS::ReturnCode_t ret;
  DDS::DomainParticipantFactoryQos factory_qos;
  unsigned int profLen;

  if (cfg.stringProfileQoS.length()<=0) return; // if QoSprofile is empty

  // needed by RTI only
  ret = factory_m->get_qos(factory_qos);
  if (ret!=DDS::RETCODE_OK)
    {
      DDSQoSSetProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
      ex.setDDSTypeCode(ret);
      ex.setQoS("factory_m->get_qos");
      throw ex;
    }//if

  profLen=factory_qos.profile.string_profile.length(); //how many profiles do we have already ?

  std::string qoSlibTag("<qos_library name=\""); // we crate qos_libarary TAG
  qoSlibTag+=cfg.libraryQos;
  qoSlibTag+="\">";

  if (profLen>0) // is it the first profile?
    {
	  bool newLib=true;
	  // lets check if the library is already there
	  for (unsigned int j=1;j<profLen-2;j++)  // we can skip the first <dds> and also two </qos_library> and </dds>
	  {
		  if (strcmp(qoSlibTag.c_str(), factory_qos.profile.string_profile[j]) == 0 )
		  {
			  newLib=false;
			  break;
		  }//if
	  }//for

	  if (newLib) // if library is not already there we have to add it
	  {
		  profLen += 3; //each profile adds three new lines progfile + 2 tag for qos_library
		  factory_qos.profile.string_profile.ensure_length(profLen, profLen);
		  factory_qos.profile.string_profile[profLen-4] = DDS_String_dup(qoSlibTag.c_str());
	  }
	  else
	  {
		  profLen += 1; //each profile adds a new line (profile)
		  factory_qos.profile.string_profile.ensure_length(profLen, profLen);
	  }
    }
  else // first profile
    {
      profLen=5;
      factory_qos.profile.string_profile.ensure_length(profLen,profLen);
	  factory_qos.profile.string_profile[profLen-5] = DDS_String_dup("<dds>");
	  factory_qos.profile.string_profile[profLen-4] = DDS_String_dup(qoSlibTag.c_str());
    }

// we assume that profile can be added to recently added library, what should be the case
  factory_qos.profile.string_profile[profLen-3] = DDS_String_dup(cfg.stringProfileQoS.c_str());
 // factory_qos.profile.string_profile[profLen-3] = DDS_String_dup("</qos_profile>");
  factory_qos.profile.string_profile[profLen-2] = DDS_String_dup("</qos_library>");
  factory_qos.profile.string_profile[profLen-1] = DDS_String_dup("</dds>");

  if (DDSConfiguration::debugLevel>0)
  			ACS_SHORT_LOG((LM_DEBUG, "We are about to add QoS profile: %s.", cfg.stringProfileQoS.c_str()));

  if (DDSConfiguration::debugLevel>=2)
  {
	  printf("QoS profiles just before setting to DDS factory (addDDSQoSProfile):\n");
	  int u=factory_qos.profile.string_profile.length();
	  for (int j=0;j<u;j++)
		  printf("=>\t %d: %s \n", j, factory_qos.profile.string_profile[j] );
  }

  ret = factory_m->set_qos(factory_qos);
  if (ret!=DDS::RETCODE_OK)
    {
      DDSQoSSetProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
      ex.setDDSTypeCode(ret);
      std::string com("factory_m->set_qos: ");
      com+=cfg.libraryQos;
      com+=" : ";
      com+=cfg.profileQos;
      ex.setQoS(com.c_str());
      throw ex;
    }//if
}//addDDSQoSProfile

void BulkDataNTStream::removeDDSQoSProfile(const DDSConfiguration &cfg)
{
	DDS::ReturnCode_t ret;
	DDS::DomainParticipantFactoryQos factory_qos;
	DDS::StringSeq newQoSprofiles;
	unsigned int profLen;

	if (cfg.stringProfileQoS.length()<=0) return; // if QoSprofile is empty we do not need to do anything

	// needed by RTI only
	ret = factory_m->get_qos(factory_qos);
	if (ret!=DDS::RETCODE_OK)
	{
		DDSQoSSetProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDDSTypeCode(ret);
		ex.setQoS("factory_m->get_qos");
		throw ex;
	}//if

	profLen = factory_qos.profile.string_profile.length(); //how many profiles do we have already ?

	if (profLen>0) // we should have more than 0 profiles
	{
		newQoSprofiles.ensure_length(profLen-1, profLen-1); // one profile one line
		// lets check if the library is already there
		unsigned int i=0;
		for (unsigned int j=0;j<profLen;j++) //loop over all profiles
		{
			//what we add to the new profile
			if (strcmp(cfg.stringProfileQoS.c_str(), factory_qos.profile.string_profile[j]) != 0 )
			{
				newQoSprofiles[i] = DDS::String_dup(factory_qos.profile.string_profile[j]);
				i++;
			}else
			{
				if (DDSConfiguration::debugLevel>0)
					ACS_SHORT_LOG((LM_DEBUG, "We are about to remove QoS profile: %s.", cfg.stringProfileQoS.c_str()));
			}//if-else
		}//for

		factory_qos.profile.string_profile =  newQoSprofiles;

		if (DDSConfiguration::debugLevel>=2)
		{
			printf("QoS profiles just before setting to DDS factory (removeDDSQoSProfile):\n");
			int u=factory_qos.profile.string_profile.length();
			for (int j=0;j<u;j++)
				printf("=>\t %d: %s \n", j, factory_qos.profile.string_profile[j] );
		}

		ret = factory_m->set_qos(factory_qos);
		if (ret!=DDS::RETCODE_OK)
		{
			DDSQoSSetProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
			ex.setDDSTypeCode(ret);
			std::string com("factory_m->set_qos: ");
			com+=cfg.libraryQos;
			com+=" : ";
			com+=cfg.profileQos;
			ex.setQoS(com.c_str());
			throw ex;
		}//if
	}
	else
	{
		// it should be at least one profile ???
	}
}//removeDDSQoSProfile
