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
* "@(#) $Id: bulkDataNTDDSPublisher.cpp,v 1.20 2011/10/14 17:17:18 bjeram Exp $"
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


BulkDataNTDDSPublisher::BulkDataNTDDSPublisher(DDS::DomainParticipant *p, const DDSConfiguration &ddsCfg) :
		BulkDataNTDDS(p, ddsCfg)
{
	publisher_m = createDDSPublisher();
}

BulkDataNTDDSPublisher::~BulkDataNTDDSPublisher()
{
	try
	{
		destroyDDSPublisher();
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

	ACS_SHORT_LOG((LM_DEBUG, "Created DDS DataWriter"));
	//? is it ok to narrow a local temp_dw and return it
	return ACSBulkData::BulkDataNTFrameDataWriter::narrow(temp_dw);
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

/*___oOo___*/
