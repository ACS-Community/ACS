#include <DDSPublisher.h>
#include <temperatureDataBlockEventTypeSupportImpl.h>

int main(int argc, char** argv)
{
	ddsnc::DDSPublisher<TESTFRIDGE::temperatureDataBlockEventDataWriter_var> *pub;
	pub = new ddsnc::DDSPublisher<TESTFRIDGE::temperatureDataBlockEventDataWriter_var>
		("partition_Default-topic", argc, argv);

	TESTFRIDGE::temperatureDataBlockEvent message;
	message.key=1;
	message.status=TESTFRIDGE::ATREF;
	message.absolutDiff = 20.0;

	pub->publishData<TESTFRIDGE::temperatureDataBlockEvent, 
		TESTFRIDGE::temperatureDataBlockEventDataWriter,
		TESTFRIDGE::temperatureDataBlockEventTypeSupport_var,
		TESTFRIDGE::temperatureDataBlockEventTypeSupportImpl> 
			(message);

	return 0;
}
