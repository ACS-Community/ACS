#ifndef _RECEIVER_PT_CB_H
#define _RECEIVER_PT_CB_H

#include "bulkDataCallback.h"
//#include "receiverPTImpl.h"
#include "ace/High_Res_Timer.h"

class ReceiverPTCb : public BulkDataCallback
{

 public:
  ReceiverPTCb();

  ~ReceiverPTCb();

  virtual int cbStart(ACE_Message_Block * userParam_p = 0);

  virtual int cbReceive(ACE_Message_Block * frame_p);

  virtual int cbStop();

  private:
    
//statistics
    void dump_stats();
    double stats_avg();
    double sum_frame();
    
    ACE_Time_Value start_time;

    //ACE_Time_Value new_time;

    //ACE_High_Res_Timer el_timer;

    //vector<long> stats;
    std::vector<double> dstats;
    
    int start;
    long count;
    long size;
    CORBA::ULong count1_m;
    CORBA::ULong count2_m;

    CORBA::ULong count_tot;

    CORBA::ULong cLoop;

 public:
    // to be done in differnt way !!!
    static ACE_CString testName_m;
};



#endif
