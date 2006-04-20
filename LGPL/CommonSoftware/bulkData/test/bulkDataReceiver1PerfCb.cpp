#include "bulkDataReceiver1PerfCb.h"


BulkDataReceiver1PerfCb::BulkDataReceiver1PerfCb() : count1_m(0)
{
    //ACE_Time_Value waitPeriod_m;
    //waitPeriod_m.set(1L, 0L);
    //setSleepTime(waitPeriod_m);

    dstats.reserve(10000);
}


BulkDataReceiver1PerfCb::~BulkDataReceiver1PerfCb()
{
//    ACS_TRACE("BulkDataReceiver1PerfCb::~BulkDataReceiver1PerfCb"); 
}


int
BulkDataReceiver1PerfCb::cbStart(ACE_Message_Block * userParam_p)
{
    count = 0;
    size = 0;
    start = 1;

/*
    if(flowNumber_m == 1)
	{
	ACS_SHORT_LOG((LM_INFO, "RECEIVER3 flowname 1: %s", flowname_m.c_str()));
	ACS_SHORT_LOG((LM_INFO, "RECEIVER3 length param flowname 1: %d", userParam_p->length()));

	count1_m = 0;
	}
*/

    return 0;
}


int
BulkDataReceiver1PerfCb::cbReceive(ACE_Message_Block * frame_p)
{

    count++;
    double dtime;

    if (start)
	{
	start_time = ACE_OS::gettimeofday();
	start = 0;
	}
    else 
	{
	ACE_Time_Value elapsed_time = ACE_OS::gettimeofday() - start_time;
	dtime = elapsed_time.sec() + ( elapsed_time.usec() / 1000000. );

	dstats.push_back(dtime);
	
	start_time = ACE_OS::gettimeofday();
	}
    
    size += frame_p->length();

/*
    if(flowNumber_m == 1)
	{
        ACS_SHORT_LOG((LM_INFO, "RECEIVER3 flowname 1: %s", flowname_m.c_str()));
        ACS_SHORT_LOG((LM_INFO, "RECEIVER3 length data flowname 1: %d", frame_p->length()));

	//ACE_OS::sleep(10);

	count1_m += frame_p->length();
	}
*/

    return 0;
}


int
BulkDataReceiver1PerfCb::cbStop()
{
    dump_stats();

    return 0;
}

double
BulkDataReceiver1PerfCb::stats_avg()
{
  double sum = 0;
  for (size_t i = 0; i < dstats.size(); i++)
    {
      sum += dstats[i];
    }
  return sum/dstats.size();
}


double
BulkDataReceiver1PerfCb::sum_frame()
{

  double sum = 0.;
  for (size_t i = 0; i < dstats.size(); i++)
    {
      sum += dstats[i];
    }
  return sum;
}


void
BulkDataReceiver1PerfCb::dump_stats (void)
{
 
//  ACS_SHORT_LOG((LM_INFO,"Dumping Stats in file StatsBulkDataReceiver1Perf.dat ....."));
 
  FILE* stats_file = ACE_OS::fopen ("StatsReceiver1.dat", "w");
  if (stats_file == 0)
      {
      ACE_ERROR ((LM_ERROR, "StatsReceiver1.dat cannot be opened \n"));
      }

  // first dump what the caller has to say.
  ACE_OS::fprintf (stats_file, "Average Inter-Frame Arrival Time = %f sec\n",stats_avg ());
  ACE_OS::fprintf (stats_file, "Amount transmitted = %ld bytes\n",size);
  ACE_OS::fprintf (stats_file, "Total time = %f sec\n",(sum_frame()));
  //ACE_OS::fprintf (stats_file, "Total time = %f sec\n",(sum_frame()/1000000.));

  double tmp = sum_frame();

  if (tmp != 0.)
      { 
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

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BulkDataReceiver1PerfImpl<BulkDataReceiver1PerfCb>)
/* ----------------------------------------------------------------*/

