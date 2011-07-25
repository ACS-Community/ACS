#include "bdNTSenderImplTest.h"

bdNTSenderImplTest::bdNTSenderImplTest(const ACE_CString& name,maci::ContainerServices* containerServices) :
BulkDataNTSenderImpl(name, containerServices)
{
    ACS_TRACE("bdNTSenderImplTest::bdNTSenderImplTest");

}

bdNTSenderImplTest::~bdNTSenderImplTest()
{
    ACS_TRACE("bdNTSenderImplTest::~bdNTSenderImplTest");
}

void bdNTSenderImplTest::startSend()
{
    ACS_TRACE("bdNTSenderImplTest::startSend");
    unsigned char parm[]="Test Parameter";

    getSenderStream()->getFlow("0")->startSend(parm, 14);
    getSenderStream()->getFlow("1")->startSend(parm, 14);
}

void bdNTSenderImplTest::paceData ()
{
	ACS_TRACE("bdNTSenderImplTest::paceData");
	unsigned char *data= new unsigned char[65000];
	for (unsigned int i=0; i<65000; i++)
		data[i]=i;
	getSenderStream()->getFlow("0")->sendData(data, 65000);

	for (unsigned int i=0; i<65000; i++)
		data[i]=i%10;
	getSenderStream()->getFlow("1")->sendData(data, 65000);

}

void bdNTSenderImplTest::stopSend()
{
    ACS_TRACE("bdNTSenderImplTest::stopSend");
    getSenderStream()->getFlow("0")->stopSend();
    getSenderStream()->getFlow("1")->stopSend();
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(bdNTSenderImplTest)
/* ----------------------------------------------------------------*/
