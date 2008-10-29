#include <DDSPublisher.h>

using namespace ddsnc;

template <class DWVAR>
int DDSPublisher<DWVAR>::attachToTransport()
{
	OpenDDS::DCPS::AttachStatus status =
		pub_impl->attach_transport(transport_impl.in());
	if (status != OpenDDS::DCPS::ATTACH_OK) {
		std::string status_str;
		switch (status) {
			case OpenDDS::DCPS::ATTACH_BAD_TRANSPORT:
				status_str = "ATTACH_BAD_TRANSPORT";
				break;
			case OpenDDS::DCPS::ATTACH_ERROR:
				status_str = "ATTACH_ERROR";
				break;
			case OpenDDS::DCPS::ATTACH_INCOMPATIBLE_QOS:
				status_str = "ATTACH_INCOMPATIBLE_QOS";
				break;
			default:
				status_str = "Unknown Status";
				break;
		}
		::std::cerr << "Failed to attach to the transport. Status == "
			<< status_str.c_str() << ::std::endl;
		return 1;
	}
	return 0;
}

//TODO:Create the disconnect or clean method :
