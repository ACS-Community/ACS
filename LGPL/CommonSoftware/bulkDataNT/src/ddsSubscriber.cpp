/*****************************************************************************/
/*         (c) Copyright, Real-Time Innovations, All rights reserved.        */
/*                                                                           */
/*         Permission to modify and use for internal purposes granted.       */
/* This software is provided "as is", without warranty, express or implied.  */
/*                                                                           */
/*****************************************************************************/

#include <iostream>
#include <ndds/ndds_cpp.h>

using namespace std;

/* By default DDS::String type manage strings up to 1k */
#define MAX_STRING_SIZE         1024

DDS_Boolean shutdown_flag = DDS_BOOLEAN_FALSE;

/* The listener of events and data from the middleware */
class HelloListener: public DDSDataReaderListener {
    public:
        void on_data_available(DDSDataReader *reader);
};


int main() {
    DDSDomainParticipant *        participant = NULL;
    DDSTopic *                    topic = NULL;
    DDSDataReader *               data_reader = NULL;
    HelloListener                 listener;
    DDS_ReturnCode_t              retcode;
    int                           main_result = 1; /* error by default */

    /* --- Set Up --------------------------------------------------------- */

    /* Uncomment the following lines to increase the verbosity of the log
     * messages output by RTI Connext:
    NDDSConfigLogger::get_instance()->set_verbosity_by_category(
                        NDDS_CONFIG_LOG_CATEGORY_API,
                        NDDS_CONFIG_LOG_VERBOSITY_STATUS_ALL );
    */

    /* Create the domain participant on domain ID 0 */
    participant = DDSDomainParticipantFactory::get_instance()->
                       create_participant(
                        0,                              /* Domain ID */
                        DDS_PARTICIPANT_QOS_DEFAULT,    /* QoS */
                        NULL,                           /* Listener */
                        DDS_STATUS_MASK_NONE);
    if (participant == NULL) {
        cerr << "Unable to create domain participant." << endl;
        goto clean_exit;
    }

    /* Create the topic "Hello, World" for the String type */    
    topic = participant->create_topic(
                        "Hello, World",                        /* Topic name*/
                        DDSStringTypeSupport::get_type_name(), /* Type name */
                        DDS_TOPIC_QOS_DEFAULT,                 /* Topic QoS */
                        NULL,                                  /* Listener  */
                        DDS_STATUS_MASK_NONE);
    if (topic == NULL) {
        cerr << "Unable to create topic." << endl;
        goto clean_exit;
    }
    
    /* Create the data writer using the default publisher */
    data_reader = participant->create_datareader(
                        topic,
                        DDS_DATAREADER_QOS_DEFAULT,    /* QoS */
                        &listener,                      /* Listener */
                        DDS_DATA_AVAILABLE_STATUS);
    if (data_reader == NULL) {
        cerr << "Unable to create data reader." << endl;
        goto clean_exit;
    }
    
    /* --- Sleep During Asynchronous Reception ---------------------------- */

    /* This thread sleeps forever. When a sample is received, RTI Data
     * Distribution Service will call the on_data_available_callback function.
     */
    cout << "Ready to read data." << endl;
    cout << "Press CTRL+C to terminate." << endl;
    for (;;) {
#ifdef RTI_WIN32
        Sleep(2000);
#else
        sleep(2);
#endif
        if(shutdown_flag){
            break;
        }
    }
    
    /* --- Clean Up ------------------------------------------------------- */

    main_result = 0;
clean_exit:
    cout << "Exiting..." << endl;
    if (participant != NULL) {
        retcode = participant->delete_contained_entities();
        if (retcode != DDS_RETCODE_OK) {
            cerr << "Deletion failed." << endl;
            main_result = 1;
        }
        retcode = DDSDomainParticipantFactory::get_instance()->
                        delete_participant(participant);
        if (retcode != DDS_RETCODE_OK) {
            cerr << "Deletion failed." << endl;
            main_result = 1;
        }
    }

    return main_result;
}


/* This method gets called back by DDS when one or more data samples have been
 * received.
 */
void HelloListener::on_data_available(DDSDataReader *reader) {
    DDSStringDataReader * string_reader = NULL;
    char                  sample[MAX_STRING_SIZE]; 
    DDS_SampleInfo        info;
    DDS_ReturnCode_t      retcode;

    /* Perform a safe type-cast from a generic data reader into a
     * specific data reader for the type "DDS::String"
     */
    string_reader = DDSStringDataReader::narrow(reader);
    if (string_reader == NULL) {
        /* In this specific case, this will never fail */
        cerr << "DDSStringDataReader::narrow failed." << endl;
        return;
    }
    
    /* Loop until there are messages available in the queue */
    char *ptr_sample = &sample[0];
    for(;;) {
        retcode = string_reader->take_next_sample(
                            ptr_sample,
                            info);
        if (retcode == DDS_RETCODE_NO_DATA) {
            /* No more samples */
            break;
        } else if (retcode != DDS_RETCODE_OK) {
            cerr << "Unable to take data from data reader, error "
                 << retcode << endl;
            return;
        }
        if (info.valid_data) {
            // Valid (this isn't just a lifecycle sample): print it
            cout << sample << endl;
            if(strlen(sample) == 0){
                shutdown_flag = DDS_BOOLEAN_TRUE;
            }
        }
    }
}








