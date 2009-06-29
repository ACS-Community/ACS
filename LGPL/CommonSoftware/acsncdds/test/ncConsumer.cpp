#include <maciSimpleClient.h>
#include <acsncSimpleConsumer.h>
#include <time.h>
#include "NCBenchmarkS.h"

void myHandlerFunction(NC_BENCHMARK::Message m, void *other)
{
	struct timeval time;
	gettimeofday(&time, NULL);
	long long t = (long long)time.tv_sec*1000000L + time.tv_usec;
	std::cout << m.seqnum << "," << t - m.time << std::endl;
}

int main(int argc, char ** argv)
{
	maci::SimpleClient client;

	client.init(argc,argv);
	client.login();

	nc::SimpleConsumer<NC_BENCHMARK::Message> *cons = 0;
	ACS_NEW_SIMPLE_CONSUMER(cons, NC_BENCHMARK::Message,
			NC_BENCHMARK::CHANNEL_NAME,
			myHandlerFunction,
			(void *) 0);

	cons->consumerReady();

	ACE_Time_Value tv(200);
	client.run(tv);

	client.logout();
	ACE_OS::sleep(5);

	cons->disconnect();
	cons = 0;
	
	return 0;
}
