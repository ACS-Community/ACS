#include <iostream>
#include <DDSSubscriber.h>
#include <MessageTypeSupportImpl.h>

void handlerFunction(NC_BENCHMARK::Message m, void *other)
{
	struct timeval time;
	gettimeofday(&time, NULL);
	long long t = (long long)time.tv_sec*1000000L + time.tv_usec;
	std::cout << "Arrived message number: " << m.seqnum
		<< " latency: " << t - m.time << std::endl;

}

int main(int argc, char**argv)
{
	ddsnc::DDSSubscriber *sub=0;
	
	ACS_NEW_DDS_SUBSCRIBER(sub, NC_BENCHMARK::Message,
			NC_BENCHMARK::CHANNEL_NAME, &handlerFunction, (void *)0);

	sub->consumerReady();

	sleep(120);

	sub->disconnect();
	delete sub;

}
