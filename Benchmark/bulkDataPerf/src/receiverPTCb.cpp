#include "receiverPTCb.h"


ACE_CString ReceiverPTCb::testName_m="Result";

ReceiverPTCb::ReceiverPTCb() : count1_m(0)
{
//    ACS_TRACE("ReceiverPTCb::ReceiverPTCb"); 
    //ACE_Time_Value waitPeriod_m;
    //waitPeriod_m.set(1L, 0L);
    //setSleepTime(waitPeriod_m);

    dstats.reserve(10000);
}


ReceiverPTCb::~ReceiverPTCb()
{
//    ACS_TRACE("ReceiverPTCb::~ReceiverPTCb"); 
}


int
ReceiverPTCb::cbStart(ACE_Message_Block * userParam_p)
{
//    ACS_TRACE("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXReceiverPTCb::cbStart"); 
    //ACS_SHORT_LOG((LM_INFO, "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXReceiverPTCb::cbStart"));

//    cout << "JJJJJJJJJJJJJJJJJJJJJJJJJ" << endl;
    count = 0;
    size = 0;
    start = 1;



/*
    if(flowNumber_m == 1)
	{
	ACS_SHORT_LOG((LM_INFO, "RECEIVERPT flowname 1: %s", flowname_m.c_str()));
	ACS_SHORT_LOG((LM_INFO, "RECEIVERPT length param flowname 1: %d", userParam_p->length()));

	count1_m = 0;
	}
*/

    return 0;
}


int
ReceiverPTCb::cbReceive(ACE_Message_Block * frame_p)
{
    //ACS_TRACE("ReceiverPTCb::cbReceive"); 

    //   cout << "KKKKKKKKKKKKKKKKKK " << frame_p->length() << endl;

    count++;
    double dtime;

    if (start)
	{
	start_time = ACE_OS::gettimeofday();
	//new_time = ACE_OS::gettimeofday();
	//el_timer.start ();


	start = 0;
	}
    else 
	{
	ACE_Time_Value elapsed_time = ACE_OS::gettimeofday() - start_time;
	dtime = elapsed_time.sec() + ( elapsed_time.usec() / 1000000. );

	//stats.push_back(elapsed_time.usec());
	dstats.push_back(dtime);
	
	start_time = ACE_OS::gettimeofday();
	}
    
    size += frame_p->length();

/*
    if(flowNumber_m == 1)
	{
        ACS_SHORT_LOG((LM_INFO, "RECEIVERPT flowname 1: %s", flowname_m.c_str()));
        ACS_SHORT_LOG((LM_INFO, "RECEIVERPT length data flowname 1: %d", frame_p->length()));

	//ACE_OS::sleep(10);

	count1_m += frame_p->length();
	}
*/

    return 0;
}


int
ReceiverPTCb::cbStop()
{

//    ACS_TRACE("YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY ReceiverPTCb::cbStop"); 
    //ACS_SHORT_LOG((LM_INFO,"YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY ReceiverPTCb::cbStop")); 

    //ACE_Time_Value elapsed_time = ACE_OS::gettimeofday() - new_time;

    //cout << "PRIMA DI CALCOLO " << elapsed_time.sec() + ( elapsed_time.usec() / 1000000. ) << endl;

    dump_stats();

    //el_timer.stop ();
    //ACE_Time_Value etime;
    //el_timer.elapsed_time (etime);
    //cout << "Elapsed_time in msec DOPO IL CALCOLO " << etime.msec () << endl;


/*
    if(flowNumber_m == 1)
	ACS_SHORT_LOG((LM_INFO, "RECEIVERPT flow 1 total length: %d", count1_m)); 
*/

    return 0;
}

double
ReceiverPTCb::stats_avg()
{
  double sum = 0;
  for (size_t i = 0; i < dstats.size(); i++)
    {
      sum += dstats[i];
    }
  return sum/dstats.size();
}


double
ReceiverPTCb::sum_frame()
{

  double sum = 0.;
  for (size_t i = 0; i < dstats.size(); i++)
    {
      sum += dstats[i];
    }
  return sum;
}


void
ReceiverPTCb::dump_stats (void)
{
 
//  ACS_SHORT_LOG((LM_INFO,"Dumping Stats in file StatsReceiverPT.dat ....."));
 
  FILE* stats_file = ACE_OS::fopen (testName_m.c_str(), "w");
  if (stats_file == 0)
      {
      ACE_ERROR ((LM_ERROR, "StatsReceiverPT.dat cannot be opened \n"));
      }

  // first dump what the caller has to say.
  ACE_OS::fprintf (stats_file, "Average Inter-Frame Arrival Time = %f sec\n",stats_avg ());
  ACE_OS::fprintf (stats_file, "Amount transmitted = %ld bytes\n",size);
  ACE_OS::fprintf (stats_file, "Total time = %f sec\n",(sum_frame()));
  //ACE_OS::fprintf (stats_file, "Total time = %f sec\n",(sum_frame()/1000000.));

  double tmp = sum_frame();

  if (tmp != 0.)
      { 
	ACS_SHORT_LOG((LM_INFO, "Transfer rate for test %s = %f Mbits/sec ", testName_m.c_str() ,(size/tmp)/(1024.*1024.)*8.));
      ACE_OS::fprintf (stats_file, "Transfer rate = %f Mbits/sec \n",  (size/tmp)/(1024.*1024.)*8. );
      }
  else 
      {
      ACE_OS::fprintf (stats_file, "Could not calculate transfer rate. Total amount of time = 0\n");
      }

  ACE_OS::fprintf (stats_file, "Single frame arrival time (sec):\n");

  for (size_t i = 0; i < dstats.size(); i++)
      ACE_OS::fprintf (stats_file, "%f\n",dstats[i]);
  
  ACE_OS::fclose (stats_file);  
  
  ACS_SHORT_LOG((LM_INFO,"Done!"));     

}

