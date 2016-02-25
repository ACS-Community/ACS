/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2011
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: bulkDataNTGenSender.cpp,v 1.17 2013/03/16 21:01:30 rtobar Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/
#include "bulkDataNTSenderFlow.h"
#include <iostream>
#include <ctime>
#include <string>
#include <ace/Get_Opt.h>
#include <ace/Tokenizer_T.h>
#include <stdlib.h>
#include <time.h>
#include "BDNTSenderSimulatorFlow.h"

using namespace AcsBulkdata;
using namespace std;

void print_usage(char *argv[]) {
	cout << "Usage: " << argv[0] << ":" << endl;
	cout << "\t[-s] \t streamName. Default: 'DefaultStream'" << endl;
	cout << "\t[-f] \t flow1Name[,flow2Name,flow3Name...]: names of the flows" << endl;
	cout << "\t[-b] \t size1[,size2,size...]: data size to send in each flow in bytes (default 65000)" << endl;
	cout << "\t[-l] \t number of loops (default 1)" << endl;
	cout << "\t[-d] \t number of seconds to wait between loops (default 10)" << endl;
	cout << "\t[-g] \t hh:mm:ss send data starting at hh:mm:ss" << endl;
	cout << "\t[-t] \t send frame timeout in sec. Default: 5.0" << endl;
	cout << "\t[-a] \t ACK timeout in sec. Default: 5.0" << endl;
	cout << "\t[-o] \t throttling in MBytes/sec. Default: 0.0 (no throttling)" << endl;
	cout << "\t[-r] \t Recreate streams/flows between iterations (default false)" << endl;
	cout << "\t[-q] \t qosFileName: load the QoS from qosFileName XML file (use default if not set)" << endl;
	cout << "\t[-x] \t qosLibName: load the QoS library from qosLibName file (use default if not set)" << endl;

	cout << "EXAMPLE:" << endl;
	cout << argv[0] << "-s sName -f fname1,fname2 -b 650000,564123 -g 12:37:30 -l 2 -d 3" << endl << endl;
	exit(1);
}

/**
 * Sleep until the current time matches with the passed HH:MM:SS
 *
 * @return false in case of error parsing the parameter
 *         true other wise
 */
bool sleepUntil(char* startTime) {
	std::string str(startTime);
	// Check the format of the passed string
	if (std::count(str.begin(), str.end(), ':')!=2) {
		cerr << "Malformed start date: expected HH:MM:SS got " << str <<endl;
		return false;
	}
	// Get hours minutes and seconds for the param
	int posFirst=str.find(":");
	int posLast=str.rfind(":");
	string hr = str.substr(0, posFirst);
	if (hr.size()==0 || hr.size()>2) {
		cerr << "Malformed start date (HH): expected HH:MM:SS got " << str <<endl;
		return false;
	}
	string min = str.substr(posFirst+1, posLast-posFirst-1);
	if (min.size()==0 || min.size()>2) {
		cerr << "Malformed start date (MM): expected HH:MM:SS got " << str <<endl;
		return false;
	}
	string sec = str.substr(posLast+1);
	if (sec.size()==0 || sec.size()>2) {
		cerr << "Malformed start date (SS): expected HH:MM:SS got " << str <<endl;
		return false;
	}
	char* errors;
	long int h = strtol(hr.c_str(), &errors, 10);
	if (errors==hr.c_str()) {
		cerr << "Malformed start date (HH): expected HH:MM:SS got " << str <<endl;
		return false;
	}
	long int m = strtol(min.c_str(), &errors, 10);
	if (errors==min.c_str()) {
		cerr << "Malformed start date (MM): expected HH:MM:SS got " << str <<endl;
		return false;
	}
	long int s = strtol(sec.c_str(), &errors, 10);
	if (errors==sec.c_str()) {
		cerr << "Malformed start date (SS): expected HH:MM:SS got " << str <<endl;
		return false;
	}
	if (h<0 || m< 0 || s<0 || h>23 || m>59 || s>59) {
		cerr << "Malformed start date expected [0-23]:[0-59]:[0-59] got " << str <<endl;
		return false;
	}


	time_t t = time(0);
	struct tm* requestedTime  = gmtime(&t);
	requestedTime -> tm_hour = h;
	requestedTime -> tm_min = m;
	requestedTime -> tm_sec = s;
	time_t requestedTimeInSecs = mktime(requestedTime);
	time_t actualTimeInSecs = time (NULL);

	if ((requestedTimeInSecs-actualTimeInSecs)<=0) {
		cerr << "Start time is in the past!" << endl;
		exit(1);
	}
	cout << "Going to sleep till " << h << ":" << m << ":" << s <<" (" << (requestedTimeInSecs-actualTimeInSecs) << " secs)" << endl ;
	sleep(requestedTimeInSecs-actualTimeInSecs);

	time_t nowTime = time(0);
	struct tm* now = gmtime(&nowTime);
	cout << "Waked up at " << now->tm_hour << ':' << now->tm_min << ':' << now->tm_sec << endl;

	return true;

}

/**
 * Remove and delete all the BDNTSenderSimulatorFlow from the passed vector
 */
void cleanSimSenders(vector<BDNTSenderSimulatorFlow*>& simSenders) {
	for (unsigned int t=0; t<simSenders.size(); t++) {
			delete simSenders[t]->getFlow();
			delete simSenders[t];
		}
	simSenders.clear();
}

/**
 * Build the flows from the passed stream.
 * The flows are added to the vector of the flows
 *
 * @param flowNames The names of the flows
 * @param stream The stream to create the flows
 * @param simSenders The vector of BDNTSenderSimulatorFlow that owns the flows
 * @param cfg: BDNT configuration
 * @param bytesToSend: the number of bytes to send from each flow
 * @param startBarrier: the barrier to start the sending
 * @param doneBarrier: the barrier to signal the termination of the sending
 * @param logger: the logger
 * @param If true removes and deletes all the BDNTSenderSimulatorFlow from simSenders
 */
void buildFlows(
		vector<std::string>fNames,
		BulkDataNTSenderStream& stream,
		vector<BDNTSenderSimulatorFlow*>& simSenders,
		SenderFlowConfiguration& cfg,
		vector<int> bytesToSend,
		ACE_Barrier** startBarrier,
		ACE_Barrier** doneBarrier,
		LoggingProxy& logger,
		bool clean) {
	if (clean) {
		if (*startBarrier!=NULL) {
			(*startBarrier)->shutdown();
			delete *startBarrier;
			*startBarrier=NULL;
		}
		if (*doneBarrier!=NULL) {
			(*doneBarrier)->shutdown();
			delete *doneBarrier;
			*doneBarrier=NULL;
		}
		cleanSimSenders(simSenders);
	}
	if (*startBarrier==NULL) {
		*startBarrier= new ACE_Barrier(fNames.size()+1,"BDNT simulator start send barrier");
	}
	if (*doneBarrier==NULL) {
		*doneBarrier = new ACE_Barrier(fNames.size()+1,"BDNT simulator done send barrier");
	}
	for(unsigned int t=0; t<fNames.size(); t++) {
		//std::cout << cfg.getQosProfile() << std::endl;
		BulkDataNTSenderFlow* flow = stream.createFlow(fNames[t].c_str(), cfg);
		BDNTSenderSimulatorFlow* senderFlow = new  BDNTSenderSimulatorFlow(stream.getName().c_str(),fNames[t].c_str(),bytesToSend[t],*(*startBarrier),*(*doneBarrier),logger,flow);
		simSenders.push_back(senderFlow);
	}
}

int main(int argc, char *argv[])
{

	bool recreate=false;
	double sendTimeout=5.0, ACKtimeout=5.0;
	ACE_Time_Value start_time, elapsed_time;
	string streamName = "DefaultStream";

	/**
	 * The name of the XML file with the QoS definition
	 */
	std::string qosFileName="";

	/**
	 * The name of the QoS library
	 */
	std::string qosLibFileName="";

	// Set if the process must start to send data at the passed time hh:mm:ss
	char *startAt = 0;

	unsigned int dataSize=65000;
	double throttling=0.0;

	// The delay between iterations in seconds
	int delay =10;

	// The name fo the flows
	vector<std::string> flowNames;

	// The number of bytes to sent through each flow (default is dataSize)
	vector<int> flowDataSize;

	unsigned int numOfIterations = 1;

	// Parse the args
    ACE_Get_Opt get_opts (argc, argv, "g:s:f:b:l:d:t:a:o:q:x:rh");
    char c;
    while((c = get_opts()) != -1 ) {
    	switch(c) {
    	case 'h':
		{
			print_usage(argv);
			break;
		}
		case 'l':
		{
			numOfIterations = atoi(get_opts.opt_arg());
			break;
		}
    	case 't':
    	{
    		sendTimeout = atof(get_opts.opt_arg());
    		break;
    	}
    	case 'a':
    	{
    		ACKtimeout = atof(get_opts.opt_arg());
    		break;
    	}
    	case 'o':
    	{
    		throttling = atof(get_opts.opt_arg());
    		break;
    	}
    	case 's':
    	{
    		streamName = get_opts.opt_arg();
    		break;
    	}
    	case 'd':
		{
			delay = atoi(get_opts.opt_arg());
			break;
		}
    	case 'r':
		{
			recreate = true;
			break;
		}
    	case 'f':
    	{
    		ACE_Tokenizer tok(get_opts.opt_arg());
    		tok.delimiter_replace(',', 0);
    		for(char *p = tok.next(); p; p = tok.next()) {
    			std::string flowName(p);
    			flowNames.push_back(flowName);
    		}
    		break;
    	}
    	case 'b':
    	{
    		ACE_Tokenizer tok(get_opts.opt_arg());
    		tok.delimiter_replace(',', 0);
    		for(char *p = tok.next(); p; p = tok.next()) {
    			flowDataSize.push_back(atoi(p));
    		}
    		break;
    	}
    	case 'g':
		{
			startAt = strdup(get_opts.opt_arg());
			break;
		}
    	case 'q':
		{
			qosFileName = get_opts.opt_arg();
			break;
		}
    	case 'x':
		{
			qosLibFileName = get_opts.opt_arg();
			break;
		}
    	default:
    	{
    		cerr << "Unrecognized option/switch in command line" << endl;
    		print_usage(argv);
    		break;
    	}
    	}
    }//while

    if( flowNames.size() == 0 ) {
    	cerr << "No flows defined" << endl;
    	print_usage(argv);
    }

    // Check if the user set all the sizes of data to send for each of the flows
    // If there are less data sizes then flows then fill with default value
    if (flowDataSize.size()>flowNames.size()) {
    	cerr << "Data size and number of flows mismatch" << endl;
    	print_usage(argv);
    }
    while (flowDataSize.size()<flowNames.size()) {
    	flowDataSize.push_back(dataSize);
    }

    // Dump the received params before starting the real computation
    cout << endl << "Computation will begin at " << startAt << endl;
    cout << "Will send data through " << flowNames.size() << " flows of the " << streamName << " stream:" << endl;
    for (unsigned int t=0; t<flowNames.size(); t++) {
    	cout << "\t" << flowDataSize[t] << " bytes through flow " << flowNames[t] << endl;
    }
    cout << "Will run " << numOfIterations << " iterations " << " with a delay of " << delay << " seconds" << endl;
    cout << "ACK timeout " << ACKtimeout << endl;
    cout << "Send timeout " << sendTimeout << endl;
    cout << "Throttling " << throttling << endl;
    if (!qosFileName.empty()) cout << "Load QoS settings from " <<  qosFileName << endl;
    if (!qosLibFileName.empty()) cout << "Load QoS library from " <<  qosLibFileName << endl;
    if (recreate) cout << "Recreate streams/flows and each iteration" << endl << endl;

	LoggingProxy m_logger(0, 0, 31, 0);
	LoggingProxy::init (&m_logger);
    ACS_CHECK_LOGGER;

	vector<BDNTSenderSimulatorFlow*> bdntSenderSimFlow;
	// first we need a stream
	SenderStreamConfiguration scfg;
	BulkDataNTSenderStream senderStream(streamName.c_str(), scfg);

	// Configure the stream
	SenderFlowConfiguration cfg;
	cfg.setACKsTimeout(ACKtimeout);
	cfg.setSendFrameTimeout(sendTimeout);
	cfg.setThrottling(throttling);
	if (!qosFileName.empty()) {
		cfg.setQosProfile(qosFileName);
	}
	if (!qosLibFileName.empty()) {
		cfg.setQosLibrary(qosLibFileName);
	}

	// The barrier to start all the sending at the same time
	// There is one thread for each flow pluse the one of the main
	ACE_Barrier* theStartBarrier=NULL;

	// The barrier to notify that all the sending have been completed
	// There is one thread for each flow pluse the one of the main
	ACE_Barrier* theDoneBarrier=NULL;

	// Build the flows and the associated BDNTSenderSimulatorFlow's
	if (!recreate) {
		buildFlows(flowNames,senderStream,bdntSenderSimFlow,cfg,flowDataSize,&theStartBarrier,&theDoneBarrier,m_logger,true);
	}

	sleepUntil(startAt);

	// first startSend
	for(unsigned int n=1; n<=numOfIterations; n++)
	{
		// Start all the thread i.e. all the sending
		std::cout << "\nIteration: " << n << " of " << numOfIterations << std::endl;
		if (recreate) {
			buildFlows(flowNames,senderStream,bdntSenderSimFlow,cfg,flowDataSize,&theStartBarrier,&theDoneBarrier,m_logger,true);
		}
		theStartBarrier->wait();
		ACE_Time_Value totSendTime = ACE_OS::gettimeofday();
		// Waiting for the sending to terminate from all the threads
		theDoneBarrier->wait();
		totSendTime = ACE_OS::gettimeofday() - totSendTime;

		// Print some statistics
		cout << "All the flows sent the data to the receivers" << endl;
		unsigned int totBytesSent=0;
		for (unsigned int t=0; t<bdntSenderSimFlow.size(); t++) {
			cout << "\tTransfer rate for " << bdntSenderSimFlow[t]->getName() << ": "<< bdntSenderSimFlow[t]->getThrouhgput()<< "MBytes/sec" << endl;
			totBytesSent += bdntSenderSimFlow[t]->getSize();
		}
		double send_time = (totSendTime.sec()+( totSendTime.usec() / 1000000. ));
		double totalThroughput=(totBytesSent/(1024.0*1024.0))/send_time;
		cout << "Total transfer rate "<< totalThroughput << "MBytes/sec" << endl << endl;

		if (n<numOfIterations) {
			cout << "Waiting " << delay << " secs before next iteration..." << endl;
			sleep(delay);
		} else {
			cout << "All iterations executed" << endl;
		}

	}

	// Terminate all the threads
	theStartBarrier->shutdown();
	theDoneBarrier->shutdown();

	cleanSimSenders(bdntSenderSimFlow);

    delete theStartBarrier;
    delete theDoneBarrier;

    m_logger.flush();
};//main
