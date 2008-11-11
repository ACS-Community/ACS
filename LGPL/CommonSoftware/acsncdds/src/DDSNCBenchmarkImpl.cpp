#include <vltPort.h>
#include <iostream>
#include <DDSPublisher.h>
#include <time.h>
#include "DDSNCBenchmarkImpl.h"
#include "MessageTypeSupportImpl.h"

DDSNCBenchmarkSupplierImpl::DDSNCBenchmarkSupplierImpl(const ACE_CString &name, 
			maci::ContainerServices *containerServices) : 
		ACSComponentImpl(name, containerServices)
{
	ACS_TRACE("::DDSNCBenchmarkSupplierImpl::DDSNCBenchmarkSupplierImpl");
}

void DDSNCBenchmarkSupplierImpl::runTest(::CORBA::ULong freq,
		::CORBA::ULong duration) throw (::CORBA::SystemException)
{
	ACS_TRACE("DDSNCBenchmarkSupplierImpl::runTest");
	ACS_NEW_DDS_PUBLISHER(pub_p, NC_BENCHMARK::Message, 
			NC_BENCHMARK::CHANNEL_NAME);
	ACS_NEW_DDS_PUBLISHER(pub_p2, NC_BENCHMARK::Message,
			NC_BENCHMARK::CHANNEL_NAME);

	struct timeval time;
	NC_BENCHMARK::Message m;
	m.data.length(8192);

	for(unsigned int i=0;i<duration;i++){
		m.seqnum=i;
		gettimeofday(&time,NULL);
		m.time= (long long)time.tv_sec*1000000L + time.tv_usec;
		PUBLISH_DATA(pub_p, NC_BENCHMARK::Message, m);
		usleep((long)(1000000/freq));
	}

	pub_p->disconnect();
	delete pub_p;

}

DDSNCBenchmarkSupplierImpl::~DDSNCBenchmarkSupplierImpl()
{

}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(DDSNCBenchmarkSupplierImpl)
/* ----------------------------------------------------------------*/

