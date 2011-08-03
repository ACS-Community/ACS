//#include "bulkDataNTReceiverFlow.h"
#include "bulkDataNTReceiverStream.h"
#include "bulkDataNTCallback.h"
#include <iostream>
#include <ace/Get_Opt.h>
#include <ace/Tokenizer_T.h>

using namespace std;

class  TestCB:  public BulkDataCallback
{
public:
	int cbStart(unsigned char* userParam_p, unsigned  int size)
	{
		std::cout << "cbStart: got " << size << " :";
		for(unsigned int i=0; i<size; i++)
		{
			std::cout <<  *(char*)(userParam_p+i);
		}
		std::cout << std::endl;
		return 0;
	}

	int cbReceive(unsigned char* data, unsigned  int size)
	{
		std::cout << "cbReceive: got " << size << " :";
/*		for(unsigned int i=0; i<frame_p->length(); i++)
		{
			std::cout <<  *(char*)(frame_p->base()+i);
		}
	*/	std::cout << std::endl;
		return 0;
	}

	int cbStop()
	{
		std::cout << "cbStop" << std::endl;
		return 0;
	}

};

void print_usage(char *argv[]) {
	cout << "Usage: " << argv[0] << " [-s streamName] -f flow1Name[,flow2Name,flow3Name...]" << endl;
	exit(1);
}

int main(int argc, char *argv[])
{

	char c;
	char *streamName = "DefaultStream";
	list<char *> flows;

	// Parse the args
	ACE_Get_Opt get_opts (argc, argv, "s:f:");
	while(( c = get_opts()) != -1 ) {

		switch(c) {
			case 's':
				streamName = get_opts.opt_arg();
				break;

			case 'f':
				ACE_Tokenizer tok(get_opts.opt_arg());
				tok.delimiter(',');
				for(char *p = tok.next(); p; p = tok.next())
					flows.push_back(p);

				break;
		}

	}

	if( flows.size() == 0 )
		print_usage(argv);

	if( !strcmp("DefaultStream", streamName) )
		cerr << "Warning: using default stream name \"DefaultStream\"" << endl;

	LoggingProxy m_logger(0, 0, 31, 0);
	LoggingProxy::init (&m_logger);
	ACS_CHECK_LOGGER;

	AcsBulkdata::BulkDataNTReceiverStream<TestCB> receiverStream(streamName);

	list<char *>::iterator it;
	for(it = flows.begin(); it != flows.end(); it++) {
		receiverStream.createFlow((*it));
	}

	std::cout << "press a key to end.." << std::endl;
	getchar();

}
