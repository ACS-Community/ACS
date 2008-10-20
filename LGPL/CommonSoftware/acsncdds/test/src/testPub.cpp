#include <DDSPublisher.h>
#include <temperatureDataBlockEventTypeSupportImpl.h>

int main(int argc, char** argv)
{
	ddsnc::DDSPublisher<TESTFRIDGE::temperatureDataBlockEventDataWriter_var> *pub;
	pub = new ddsnc::DDSPublisher
		<TESTFRIDGE::temperatureDataBlockEventDataWriter_var>
		("DefaultTopic", argc, argv);

	TESTFRIDGE::temperatureDataBlockEvent message;
	message.key=1;
	message.status=TESTFRIDGE::ATREF;
	message.absolutDiff = 20.0;

	for(int i=0;i<100;i++){
		message.absolutDiff=message.absolutDiff+i;
		pub->publishData<TESTFRIDGE::temperatureDataBlockEvent, 
			TESTFRIDGE::temperatureDataBlockEventDataWriter,
			TESTFRIDGE::temperatureDataBlockEventTypeSupport_var,
			TESTFRIDGE::temperatureDataBlockEventTypeSupportImpl> (message);
		sleep(1);
	}
	pub->disconnect();
	delete pub;
	return 0;
}
