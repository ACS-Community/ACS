#ifndef _BULKDATA_RECEIVER1_PERF_CB_H
#define _BULKDATA_RECEIVER1_PERF_CB_H

#include "bulkDataCallback.h"
#include "bulkDataReceiver1PerfImpl.h"
#include "ace/High_Res_Timer.h"

class BulkDataReceiver1PerfCb : public BulkDataCallback
{

 public:
  BulkDataReceiver1PerfCb();

  ~BulkDataReceiver1PerfCb();

  virtual int cbStart(ACE_Message_Block * userParam_p = 0);

  virtual int cbReceive(ACE_Message_Block * frame_p);

  virtual int cbStop();

  private:
    
    //statistics
    void dump_stats();
    double stats_avg();
    double sum_frame();
    
    ACE_Time_Value start_time;
    std::vector<double> dstats;
    int start;
    long count;
    long size;
    CORBA::ULong count1_m;
};



#endif
