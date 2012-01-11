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
* "@(#) $Id: bulkDataNTDDSPublisher.cpp,v 1.25 2012/01/11 10:22:16 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/
#include "bulkDataNTDDSPublisher.h"
#include <iostream>

using namespace AcsBulkdata;
using namespace std;
using namespace ACSErrTypeCommon;
using namespace ACS_DDS_Errors;


BulkDataNTDDSPublisher::BulkDataNTDDSPublisher(DDS::DomainParticipant *p, const SenderFlowConfiguration &sfCfg) :
		BulkDataNTDDS(p, sfCfg), publisher_m(0), dataWriter_m(0)
{
	publisher_m = createDDSPublisher();
	frameTimeout_m = sfCfg.getFrameTimeout();
}

BulkDataNTDDSPublisher::~BulkDataNTDDSPublisher()
{
	try
	{
		destroyDDSPublisher();
		publisher_m=0;
		//TBD what should we do about dataWriter_m ?
	}
	catch(ACSErr::ACSbaseExImpl &ex)
	{
		ex.log();
	}
}//~BulkDataNTDDSPublisher

DDS::Publisher* BulkDataNTDDSPublisher::createDDSPublisher()
{
	AUTO_TRACE(__PRETTY_FUNCTION__);

	DDS::Publisher *pub = participant_m->create_publisher_with_profile(
			ddsCfg_m.libraryQos.c_str(), ddsCfg_m.profileQos.c_str(),
			0, DDS::STATUS_MASK_NONE);
	if(pub==0)
	{
		DDSPublisherCreateProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		throw ex;
	}//if

	return pub;
}//createDDSParticipant

void  BulkDataNTDDSPublisher::destroyDDSPublisher()
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	DDS::ReturnCode_t ret;
	ret = participant_m->delete_publisher(publisher_m);
	publisher_m = 0;
	if (ret!=DDS::RETCODE_OK)
	{
		DDSPublisherDestroyProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDDSTypeCode(ret);
		throw ex;
	}//if
}//destroyDDSPublisher

ACSBulkData::BulkDataNTFrameDataWriter* BulkDataNTDDSPublisher::createDDSWriter(DDS::Topic *topic, DDS::DataWriterListener *listener)
{
	AUTO_TRACE(__PRETTY_FUNCTION__);

	if (publisher_m==NULL || topic==NULL || listener==NULL)
	{
		NullPointerExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setVariable("publisher_m, topic or listener");
		throw ex;
	}

	// DW must be created disabled, that is achieved by setting  autoenable_created_entities QoS of publisher to false
	// which is set as default value in bulkDataNTDefaultQosProfiles.xml
	DDS::DataWriter* temp_dw = publisher_m->create_datawriter_with_profile(
			topic,
			ddsCfg_m.libraryQos.c_str(), ddsCfg_m.profileQos.c_str(),
			listener,
			DDS::STATUS_MASK_ALL
	);

	if(temp_dw==0)
	{
		DDSDWCreateProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		throw ex;
	}//if

	// we need dataWriter_m before setting the timeout
	dataWriter_m = ACSBulkData::BulkDataNTFrameDataWriter::narrow(temp_dw);

	setWriteBlockingTime(frameTimeout_m);

	DDS::ReturnCode_t ret = dataWriter_m->enable(); // we can enable DW after we set blocking time
	if (ret!=DDS::RETCODE_OK)
	{
		DDSDWEnableProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDDSTypeCode(ret);
		throw ex;
	}//if

	ACS_SHORT_LOG((LM_DEBUG, "Created DDS DataWriter"));
	//? is it ok to narrow a local temp_dw and return it
	return dataWriter_m;
}//createDDSWriter

void BulkDataNTDDSPublisher::destroyDDSWriter (ACSBulkData::BulkDataNTFrameDataWriter* dw)
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	DDS::ReturnCode_t ret;

	ret = publisher_m->delete_datawriter(dw);
	if (ret!=DDS::RETCODE_OK)
	{
		ACS_SHORT_LOG((LM_ERROR, "Problem deleting data writer (%d)", ret));
	}
}//destroyDDSWriter


void BulkDataNTDDSPublisher::setWriteBlockingTime(double frameTimeout)
{

	DDS::ReturnCode_t ret;
	DDS::DataWriterQos dwQoS;
	DDS::Long frameTimeoutSec = static_cast<DDS::Long>(frameTimeout);
	DDS::Long frameTimeoutNanosec = 1000000 * static_cast<DDS::Long>(frameTimeout - frameTimeoutSec);

	ret = dataWriter_m->get_qos(dwQoS);
	if (ret!=DDS::RETCODE_OK)
	{
		DDSQoSSetProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDDSTypeCode(ret);
		ex.setQoS("dataWriter_m->get_qos()");
		throw ex;
	}//if

	dwQoS.reliability.max_blocking_time.sec = frameTimeoutSec;
	dwQoS.reliability.max_blocking_time.nanosec = frameTimeoutNanosec;

	ret = dataWriter_m->set_qos(dwQoS);
	if (ret!=DDS::RETCODE_OK)
	{
		DDSQoSSetProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDDSTypeCode(ret);
		ex.setQoS("dataWriter_m->set_qos()");
		throw ex;
	}//if
	ACS_LOG(LM_RUNTIME_CONTEXT, __PRETTY_FUNCTION__, (LM_DEBUG, "max_blocking_time set to: %d sec %d nanosec",
			frameTimeoutSec, frameTimeoutNanosec));
}
/*___oOo___*/
