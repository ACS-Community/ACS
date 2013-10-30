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
* "@(#) $Id: bdNTReceiverImplTest.cpp,v 1.7 2012/07/11 08:46:26 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/
#include "bdNTMasterImplTest.h"



BulkDataNTMasterImpl::BulkDataNTMasterImpl(const ACE_CString& name,maci::ContainerServices* containerServices) :
	BulkDataNTReceiverImpl<TestCB>(name, containerServices)
{
	sndCfgParser_m = 0; rcvCfgParser_m = 0;
	rcvStrCfg_m=0; sndStrCfg_m=0;

	rcvFlowCfg_m=0;rcvStream1_m=0; rcvStream2_m=0;
	rcvFlow1_m=0; rcvFlow2_m=0;

	sndFlowCfg_m=0;sndStream1_m=0; sndStream2_m=0;
	sndFlow1_m=0; sndFlow2_m=0;
}

BulkDataNTMasterImpl::~BulkDataNTMasterImpl()
{

}

void BulkDataNTMasterImpl::initialize()
{
	ACS_TRACE("BulkDataNTReceiverImpl<>::initialize");

		char buf[BUFSIZ];

		// get the DAO and read the alma/ branch of the component out from the CDB
		CDB::DAL_ptr dal_p = getContainerServices()->getCDB();
		if(CORBA::is_nil(dal_p))
		{
			ACS_SHORT_LOG((LM_ERROR,"BulkDataNTMasterImpl::initialize error getting DAL reference"));
			ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataNTMasterImpl::openReceiver");
			throw err;
		}

		/*
		CDB::DAO_ptr dao_p = dal_p->get_DAO_Servant("DEFAULT_CFG");
		if(CORBA::is_nil(dao_p))
		{
			ACS_SHORT_LOG((LM_ERROR,"BulkDataNTMasterImpl::initialize error getting DAO reference"));
			ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(__FILE__,__LINE__,"BulkDataNTMasterImpl::openReceiver");
			throw err;
		}
*/
		// Use the new mechanism to parse the XML document and create the streams/flows
		char *senderXMLnode = dal_p->get_DAO("alma/DEFAULT_SENDER_CFG");
		char *receiverXMLnode = dal_p->get_DAO("alma/DEFAULT_RECEIVER_CFG");

		try {
			// Parse the XML document and check if we got any configuration object
			sndCfgParser_m = new AcsBulkdata::BulkDataConfigurationParser("SenderParser");
			sndCfgParser_m->parseSenderConfig(senderXMLnode);
			if( sndCfgParser_m->getAllSenderStreamNames().size() == 0 )
				ACS_SHORT_LOG((LM_WARNING,"BulkDataNTMasterImpl::initialize No Derfault Sender Streams configuration."));

			rcvCfgParser_m = new AcsBulkdata::BulkDataConfigurationParser("ReceiverParser");
			rcvCfgParser_m->parseReceiverConfig(receiverXMLnode);
			if( rcvCfgParser_m->getAllReceiverStreamNames().size() == 0 )
					ACS_SHORT_LOG((LM_WARNING,"BulkDataNTMasterImpl::initialize No Default Receiver Streams configuration."));

			// get (default) cfg for stream and flow (just once!!)
			sndStrCfg_m = sndCfgParser_m->getSenderStreamConfiguration("DefaultSenderStreamCfg");
			sndFlowCfg_m =  sndCfgParser_m->getSenderFlowConfiguration("DefaultSenderStreamCfg", "DefaultSenderFlowCfg");

			// get (default) cfg for stream and flow (just once!!)
			rcvStrCfg_m = rcvCfgParser_m->getReceiverStreamConfiguration("DefaultStreamCfg");
			rcvFlowCfg_m =  rcvCfgParser_m->getReceiverFlowConfiguration("DefaultStreamCfg", "DefaultFlowCfg");

		} catch(ACS_BD_Errors::CDBProblemExImpl &ex) {
			ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTMasterImpl::initialize");
			err.log(LM_DEBUG);
			throw err;
		} catch(...) {
			ACSErrTypeCommon::UnknownExImpl ex = ACSErrTypeCommon::UnknownExImpl(__FILE__,__LINE__,"BulkDataNTMasterImpl::initialize");
			ACSBulkDataError::AVOpenReceiverErrorExImpl err = ACSBulkDataError::AVOpenReceiverErrorExImpl(ex,__FILE__,__LINE__,"BulkDataNTMasterImpl::initialize");
			err.log(LM_DEBUG);
			throw err;
		}
}//initialize

void BulkDataNTMasterImpl::cleanUp()
{
	closeReceiver();
	delete sndCfgParser_m;
	delete rcvCfgParser_m;
}//cleanUp

void BulkDataNTMasterImpl::openReceiver()
{
// although the name of the method suggest to open "just" receiver, we actually here create (open) senders, too
	try{
		// we create here two rcv streams
		rcvStream1_m =  new AcsBulkdata::BulkDataNTReceiverStream<TestCB>("MyStream1", *rcvStrCfg_m);
		rcvFlow1_m = rcvStream1_m->createFlow("Flow1", *rcvFlowCfg_m);

		rcvStream2_m =  new AcsBulkdata::BulkDataNTReceiverStream<TestCB>("MyStream2", *rcvStrCfg_m);
		rcvFlow2_m = rcvStream2_m->createFlow("Flow1", *rcvFlowCfg_m);

		// we create here two snd streams
		sndStream1_m =  new AcsBulkdata::BulkDataNTSenderStream("MyStream1", *sndStrCfg_m);
		sndFlow1_m = sndStream1_m->createFlow("Flow1", *sndFlowCfg_m);

		sndStream2_m =  new AcsBulkdata::BulkDataNTSenderStream("MyStream2", *sndStrCfg_m);
		sndFlow2_m = sndStream2_m->createFlow("Flow1", *sndFlowCfg_m);
	}catch(ACSErr::ACSbaseExImpl &ex)
	{
		ex.log();
	} catch(...) {
		ACS_SHORT_LOG((LM_ERROR,"BulkDataNTMasterImpl::openReceiver exception!!"));
	}
}//openReceiver

void BulkDataNTMasterImpl::closeReceiver()
{
	try{
		delete sndFlow1_m; sndFlow1_m=0;
		delete sndStream1_m; sndStream1_m=0;

		delete sndStream2_m; sndStream2_m=0; sndFlow2_m=0;

		// we test the opposite order
		delete rcvFlow1_m; rcvFlow1_m=0;
		delete rcvStream1_m; rcvStream1_m=0;

		delete rcvStream2_m; rcvStream2_m=0; rcvFlow2_m=0;
	}catch(ACSErr::ACSbaseExImpl &ex)
	{
		ex.log();
	} catch(...) {
		ACS_SHORT_LOG((LM_ERROR,"BulkDataNTMasterImpl::closeReceiver exception!!"));
	}
}//closeReceiver



/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataNTMasterImpl)
/* ----------------------------------------------------------------*/
