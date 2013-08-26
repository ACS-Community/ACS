#include <DDSPublisher.h>
#include <time.h>
#include "MessageTypeSupportImpl.h"

int main(int argc, char **argv)
{
	ddsnc::DDSPublisher<NC_BENCHMARK::MessageDataWriter_var> *pub = 0;
	pub = new ddsnc::DDSPublisher<NC_BENCHMARK::MessageDataWriter_var>
		(NC_BENCHMARK::CHANNEL_NAME);

	struct timeval time;
	NC_BENCHMARK::Message m;

	for(int i=0;i<100;i++){
		m.seqnum=i;
		gettimeofday(&time,NULL);
		m.time= (long long)time.tv_sec*1000000L + time.tv_usec;
		pub->publishData<NC_BENCHMARK::Message,
		  	NC_BENCHMARK::MessageDataWriter,
			NC_BENCHMARK::MessageTypeSupport_var,
			NC_BENCHMARK::MessageTypeSupportImpl>(m);
		sleep(1);
	}

	pub->disconnect();
	delete pub;

}
