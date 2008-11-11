#include <vltPort.h>
#include <iostream>
#include <acsncSimpleSupplier.h>
#include <time.h>
#include "NCBenchmarkImpl.h"

NCBenchmarkSupplierImpl::NCBenchmarkSupplierImpl(const ACE_CString &name, 
			maci::ContainerServices *containerServices) : 
		ACSComponentImpl(name, containerServices)
{
	ACS_TRACE("::NCBenchmarkSupplierImpl::NCBenchmarkSupplierImpl");
}

void NCBenchmarkSupplierImpl::runTest(::CORBA::ULong freq,
		::CORBA::ULong duration) throw (::CORBA::SystemException)
{
	ACS_TRACE("NCBenchmarkSupplierImpl::runTest()");
	struct timeval time;
	nc::SimpleSupplier *sup_p = 0;
	sup_p = new nc::SimpleSupplier(NC_BENCHMARK::CHANNEL_NAME, this);
	NC_BENCHMARK::Message m;
	
	m.seqnum=1;
	m.time=1;
	m.data.length(8132);

	for(unsigned int i=0;i<duration;i++){
		m.seqnum=i;
		gettimeofday(&time,NULL);
		m.time= (long long)time.tv_sec*1000000L + time.tv_usec;
		sup_p->publishData<NC_BENCHMARK::Message>(m);
		usleep((long)(1000000/freq));
	}

	if(sup_p){
		sup_p->disconnect();
		sup_p=0;
	}
}

NCBenchmarkSupplierImpl::~NCBenchmarkSupplierImpl()
{

}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(NCBenchmarkSupplierImpl)
/* ----------------------------------------------------------------*/

