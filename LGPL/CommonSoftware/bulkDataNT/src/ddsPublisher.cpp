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

int main() {
    DDSDomainParticipant *  participant = NULL;
    DDSTopic *              topic = NULL;
    DDSDataWriter *         data_writer = NULL;
    DDSStringDataWriter *   string_writer = NULL;
    DDS_ReturnCode_t        retcode;
    char                    sample[MAX_STRING_SIZE];
    int                     main_result = 1; /* error by default */
    
    /* --- Set Up --------------------------------------------------------- */

    NDDSConfigLogger::get_instance()->set_verbosity_by_category(
                        NDDS_CONFIG_LOG_CATEGORY_API,
                        NDDS_CONFIG_LOG_VERBOSITY_STATUS_ALL);
	/* NDDSConfigLogger::get_instance()->set_verbosity_by_category(
						NDDS_CONFIG_LOG_CATEGORY_ENTITIES,
						NDDS_CONFIG_LOG_VERBOSITY_STATUS_ALL);
	NDDSConfigLogger::get_instance()->set_verbosity_by_category(
    					NDDS_CONFIG_LOG_CATEGORY_COMMUNICATION,
    					NDDS_CONFIG_LOG_VERBOSITY_STATUS_ALL);
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
    data_writer = participant->create_datawriter(
                                topic,
                                DDS_DATAWRITER_QOS_DEFAULT,     /* QoS */
                                NULL,                           /* Listener */
                                DDS_STATUS_MASK_NONE);
    if (data_writer == NULL) {
        cerr << "Unable to create data writer." << endl;
        goto clean_exit;
    }

    /* Perform a safe type-cast from a generic data writer into a
     * specific data writer for the type "DDS::String"
     */
    string_writer = DDSStringDataWriter::narrow(data_writer);
    if (string_writer == NULL) {
        /* In this specific case, this will never fail */
        cerr << "DDS_StringDataWriter_narrow failed." << endl;
        goto clean_exit;
    }

    /* --- Write Data ----------------------------------------------------- */

    cout << "Ready to write data." << endl;
    cout << "When the subscriber is ready, you can start writing." << endl;
    cout << "Press CTRL+C to terminate or enter an empty line to do a clean shutdown."
         << endl
         << endl;
    /* RTI provides APIs for detecting when data readers and data writers
     * join the network. You can also configure data durability so that data
     * readers can receive data that were written before they started.
     * However, for the sake of keeping this example as simple as possible,
     * it asks you to wait for both sides to start before continuing.
     */

    for (;;) {
        cout << "Please type a message> ";
        if (!cin.getline(sample, MAX_STRING_SIZE-1)) {
            break;
        }
        retcode = string_writer->write(
                            sample,
                            DDS_HANDLE_NIL);
        if (retcode != DDS_RETCODE_OK) {
            cerr << "Write failed: " << retcode << endl;
        }
        if (strlen(sample) == 0) {
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

