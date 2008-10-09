#ifndef _DDS_PUBLISHER_H
#define _DDS_PUBLISHER_H

#include <DDSHelper.h>
#include <dds/DCPS/PublisherImpl.h>

namespace ddsnc{
	template<class DWVAR>
	class DDSPublisher : public ::ddsnc::DDSHelper{
		private:
		DDS::Publisher_var pub;
		OpenDDS::DCPS::PublisherImpl *pub_impl;
		DDS::PublisherQos pub_qos;
		DDS::DataWriter_var dw;
		DWVAR dataWriter;	
		DDS::InstanceHandle_t handler;

		int attachToTransport()
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

		void initialize()
		{
			std::cerr<< "DDSPublisher::initialize()"<<std::endl;
			createParticipant();
			if (CORBA::is_nil (participant.in()))
				std::cerr << "Participant is nil" << std::endl;

			if(partitionName!=NULL){
				participant->get_default_publisher_qos(pub_qos);
				pub_qos.partition.name.length(1);
				pub_qos.partition.name[0]=CORBA::string_dup(partitionName);
			}
			initializeTransport();
			createPublisher();
		}

		template <class DW> void initializeDataWriter()
		{
			DDS::DataWriterQos dw_qos;
			pub->get_default_datawriter_qos (dw_qos);

			dw = pub->create_datawriter(topic.in(),
					dw_qos,  DDS::DataWriterListener::_nil());
			if(CORBA::is_nil(dw.in())){
				std::cerr << "create datawriter failed" << std::endl;
			}
			dataWriter = DW::_narrow(dw.in());
		}

		public:
		DDSPublisher(const char *channelName, int argv, char** argc): 
			::ddsnc::DDSHelper(channelName, argv, argc){}

		int createPublisher()
		{
			std::cerr << "DDSPublisher::createPublisher" << std::endl;

			if(partitionName==NULL){
				pub =  participant->create_publisher(PUBLISHER_QOS_DEFAULT,
						DDS::PublisherListener::_nil());
				std::cerr << "Creating Publisher with default Qos" << std::endl;
			}
			
			else{
				pub = participant->create_publisher(pub_qos,
						DDS::PublisherListener::_nil());
				std::cerr << "Creating Publisher with partition " << partitionName 
					<< std::endl;
			}

			if(CORBA::is_nil(pub.in())){
				std::cerr << "create publisher failed" << std::endl;
				return 1;
			}

			pub_impl= dynamic_cast<OpenDDS::DCPS::PublisherImpl*>(pub.in());
			if(pub_impl == NULL){
				std::cerr << "Failed to obtain publisher servant" << std::endl;
				return 1;
			}
			return attachToTransport();
		}

/* template requires:
 * D: <type> 
 * DW: <type>DataWriter class, 
 * TSV: <type>TypeSupport_var class, 
 * TSI: <type>TypeSupportImpl class
 */ 
		template <class D, class DW, class TSV, class TSI>
			void publishData(D data)
			{
				if(initialized==false){
					/*Initialize the rest of things*/
					initialize();
					/*Initialize Type Support*/
					TSV ts;
					ts = new TSI();
					if(DDS::RETCODE_OK != ts->register_type(participant.in(),""))
						std::cerr << "register_type failed" << std::endl;
					/*Initialize the Topic*/
					initializeTopic(topicName, ts->get_type_name());
					if(CORBA::is_nil(topic.in()))
							std::cerr<< "Topic is nil" << std::endl;
					initializeDataWriter<DW>();
					handler = dataWriter->_cxx_register(data);
					initialized=true;
				}
				dataWriter->write(data,handler);
			}

	};
}
#endif
