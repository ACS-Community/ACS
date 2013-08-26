#include <vltPort.h>
#include <iostream>
#include <DDSPublisher.h>
#include <time.h>
#include "simpleExampleDDS.h"
#include "simpleMessageTypeSupportImpl.h"

SimpleExampleDDSImpl::SimpleExampleDDSImpl(const ACE_CString &name, 
			maci::ContainerServices *containerServices) : 
		ACSComponentImpl(name, containerServices)
{
	ACS_TRACE("::SimpleExampleDDSImpl::SimpleExampleDDSImpl");
}


void SimpleExampleDDSImpl::sendMessage() throw (::CORBA::SystemException)
{
        time_t actual;
	time(&actual);
	ACS_TRACE("SimpleExampleDDSImpl::sendMessage:");
//	ACS_NEW_DDS_PUBLISHER(pub_p, DDS_SIMPLE_EXAMPLE::simpleMessage, 
//			DDS_SIMPLE_EXAMPLE::CHANNEL_NAME);
	ddsnc::DDSPublisher *pub_p = 
		new ddsnc::DDSPublisher(DDS_SIMPLE_EXAMPLE::CHANNEL_NAME);
	DDS_SIMPLE_EXAMPLE::simpleMessage m;
        m.seqnum=actual;
	//m.seqnum=1;

	PUBLISH_DATA(pub_p, DDS_SIMPLE_EXAMPLE::simpleMessage, m);
	sleep(1);
	pub_p->disconnect();
	delete pub_p;

}

SimpleExampleDDSImpl::~SimpleExampleDDSImpl()
{

}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(SimpleExampleDDSImpl)
/* ----------------------------------------------------------------*/

