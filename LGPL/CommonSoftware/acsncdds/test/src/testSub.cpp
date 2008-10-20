#include <iostream>
#include <DDSSubscriber.h>
#include <temperatureDataBlockEventTypeSupportImpl.h>

void handlerFunction(TESTFRIDGE::temperatureDataBlockEvent joe, void *other)
{
	std::cout << "joe.absolutDiff: " << joe.absolutDiff << std::endl;
}

int main(int argc, char**argv)
{
	ddsnc::DDSSubscriber *sub=0;
	
	ACS_NEW_DDS_SUBSCRIBER(sub, TESTFRIDGE::temperatureDataBlockEvent,
			TESTFRIDGE::temperatureDataBlockEventTypeSupport_var,
			TESTFRIDGE::temperatureDataBlockEventTypeSupportImpl,
			TESTFRIDGE::temperatureDataBlockEventDataReader_var,
			TESTFRIDGE::temperatureDataBlockEventDataReader,
			"DefaultTopic", &handlerFunction, argc, argv);

	sub->consumerReady();

	sleep(120);

	sub->disconnect();
	delete sub;

}
