#include <vltPort.h>
#include <iostream>
#include <acsncSimpleSupplier.h>
#include <time.h>
#include "simpleExampleNC.h"

SimpleExampleNCImpl::SimpleExampleNCImpl(const ACE_CString &name, 
			maci::ContainerServices *containerServices) : 
		ACSComponentImpl(name, containerServices)
{
	ACS_TRACE("::SimpleExampleNCImpl::SimpleExampleNCImpl");
}

void SimpleExampleNCImpl::sendMessage() throw (::CORBA::SystemException)
{
	ACS_TRACE("SimpleExampleNCImpl::sendMessage");
	nc::SimpleSupplier *sup_p = 0;
	sup_p = new nc::SimpleSupplier(DDS_SIMPLE_EXAMPLE::CHANNEL_NAME, this);

	DDS_SIMPLE_EXAMPLE::simpleMessage m;
	m.seqnum=1;

	sup_p->publishData<DDS_SIMPLE_EXAMPLE::simpleMessage>(m);

	sleep(1);
	sup_p->disconnect();
	sup_p=0;

}

SimpleExampleNCImpl::~SimpleExampleNCImpl()
{

}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(SimpleExampleNCImpl)
/* ----------------------------------------------------------------*/

