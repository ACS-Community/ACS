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

    getSender()->startSend(0, parm, 14);
    getSender()->startSend(1, parm, 14);
}

void bdNTSenderImplTest::paceData ()
{
	ACS_TRACE("bdNTSenderImplTest::paceData");
	unsigned char *data= new unsigned char[65000];
	for (unsigned int i=0; i<65000; i++)
		data[i]=i;
	getSender()->sendData(0, data, 65000);

	for (unsigned int i=0; i<65000; i++)
		data[i]=i%10;
	getSender()->sendData(1, data, 65000);

}

void bdNTSenderImplTest::stopSend()
{
    ACS_TRACE("bdNTSenderImplTest::stopSend");
    getSender()->stopSend(0);
    getSender()->stopSend(1);
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(bdNTSenderImplTest)
/* ----------------------------------------------------------------*/
