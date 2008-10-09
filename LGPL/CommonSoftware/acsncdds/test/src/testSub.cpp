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
	sub = new ddsnc::DDSSubscriber("DefaultTopic", argc, argv);

	sub->initialize<TESTFRIDGE::temperatureDataBlockEvent,
		TESTFRIDGE::temperatureDataBlockEventTypeSupport_var,
		TESTFRIDGE::temperatureDataBlockEventTypeSupportImpl>();
	
	sub->addSubscription<TESTFRIDGE::temperatureDataBlockEventDataReader_var,
		TESTFRIDGE::temperatureDataBlockEventDataReader,
		TESTFRIDGE::temperatureDataBlockEvent>
			(&handlerFunction, (void *)0);

	sleep(120);

	sub->disconnect();

}
