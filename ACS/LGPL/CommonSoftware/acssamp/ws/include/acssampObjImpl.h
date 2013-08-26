#ifndef _ACSAMP_OBJ_IMPL_H
#define _ACSAMP_OBJ_IMPL_H 
/*******************************************************************************
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *
 *
 * "@(#) $Id: acssampObjImpl.h,v 1.30 2008/10/07 06:41:54 cparedes Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * oat       07/04/03  created 
 */

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "acssampImpl.h"
#include "acssampS.h"
#include <acsncSimpleSupplier.h>
#include <ace/Message_Queue.h>

#include <sstream>


#define ACS_SAMP_C class T, class T_var, class Tval
#define ACS_SAMP_TL T, T_var, Tval


/** @file acssampObjImpl.h
 *  Header file for the sampling object. It contains all method dealing 
 *  with sampling setup, start, stop etc. of a user-defined property.
 *  The collected, buffered data, are then sent to the notification channel.
 *  
 */

//forward declaration
template<ACS_SAMP_C>
class ACSSampObjImpl;

template<ACS_SAMP_C>
class SamplingThread : public ACS::Thread
{
  public:
    SamplingThread(const ACE_CString& name,
		   ACSSampObjImpl<ACS_SAMP_TL> *sampObj,
		   const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
		   const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime) :
	ACS::Thread(name, responseTime, sleepTime),
	sampObj_p(sampObj) {}

    ~SamplingThread() {}

    virtual void runLoop()
	{
	    if ( sampObj_p->isInDestructState() ) exit();
//	    ACS_DEBUG("acssamp::ACSSampObjImpl::sampThreadWorker","sampling ...");		    
	    // perform the sampling
	    sampObj_p->doSamp();
	}
		
  private:

    ACSSampObjImpl<ACS_SAMP_TL> *sampObj_p;   
};


template<ACS_SAMP_C>
class SamplingThreadFlush : public ACS::Thread
{
  public:
    SamplingThreadFlush(const ACE_CString& name, 
			ACSSampObjImpl<ACS_SAMP_TL> *sampObj,
			const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
			const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime) :
	ACS::Thread(name, responseTime, sleepTime),
	sampObj_p(sampObj) {}

    ~SamplingThreadFlush() {}

    virtual void runLoop()
	{
	    ACS_DEBUG_PARAM("acssamp::ACSSampObjImpl::sampThreadFlush","Starting thread %s",getName().c_str());  
      
	    if (sampObj_p->isInDestructState()) exit();
	    //  ACS_DEBUG("acssamp::ACSSampObjImpl::sampThreadWorker","flushing on the NC ...");
	    sampObj_p->flushSamp();
	}
		
  private:

    ACSSampObjImpl<ACS_SAMP_TL> *sampObj_p;   
};




/** @class ACSSampObjImpl
 *  This class implements all methods used to sample a specific
 *  property. In particular, once a new sampling object is
 *  initialized, with user-defined parameters (like the sampling rate,
 *  the report rate etc.) it allows to start/stop/pause/continue
 *  the sampling. Moreover it expose methods to change these
 *  parametrs on-the-fly. All buffered data are delivered automatically
 *  to the notification channel; trough it a client could retrieve them,
 *  and subsequently plot them or perform the necessary kind of analysis.
 *
 *  This class is a template. This because the type of the property
 *  could be RWdouble, RWlong etc.
 *
 *  This class is meant to be used trough the factory class ACSSampImpl.
 */

template <ACS_SAMP_C>
class ACSSampObjImpl: public virtual POA_acssamp::SampObj,
                      public virtual PortableServer::RefCountServantBase
{
  
  public:
    /**
     * Constructor
     * It construct the sampling object, using the following parameters:
     * @param _cobName   name of the component, which property is to be sampled
     *                   (e.g.<i>LAMP1 </i>)
     * @param _propertyName   name of the property to be sampled (e.g. <i>brightness </i>)
     * @param _sampFrequency  sampling frequency: period between two consecutive
     *                      sampling
     *                      <i>(units are 100ns; e.g. 1000000 means 0.1 sec i.e. 10 samples per second) </i>
     * @param _sampReportRate    number of second the process should buffer 
     *                           before actually sending (notifying) data
     *                           <i>(units are 100ns; e.g. 10000000 means collect data
     *                           for 1 second)</i>
     * @param _m_cob  BACIComponent of the factory object.
     * @param _genProperty  reference to a generic property
     * @param _sampPtr pointer to the factory object. It is used to store created sampling
     *                 object in an internal (member of factory object) list; when the
     *                 the sampling objects are destroyed, the corresponding entry in the list 
     *                 is cleared. When the base factory is destroyed, it checks
     *                 the list and in case takes care to
     *                 clean-up correctly all the existing sampling objects (still not removed
     *                 from the list).
     *
     */


    ACSSampObjImpl(const ACE_CString& _cobName, const ACE_CString& _propertyName, 
		   ACS::TimeInterval _sampFrequency, ACS::TimeInterval _sampReportRate, baci::BACIComponent *_m_cob, ACS::Property_var _genProperty, ACSSampImpl * _sampPtr);

    /**
     * Destructor
     */
    virtual ~ACSSampObjImpl();


    /**
     * Starts the sampling.
     * Implementation of IDL start() interface.
     
     * @return void
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */     
    virtual void start ();

    /**
     * Stops the sampling.
     * Implementation of IDL stop() interface.
     
     * @return void
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */ 
    virtual void stop ();
    
    /**
     * Destroy the sampling object.
     * Implementation of IDL destroy() interface (inherited from the ACS:Subscription interface).
     
     * @return void
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */ 
    virtual void destroy();

    /**
     * Suspend the sampling.
     * Implementation of IDL suspend() interface (inherited from the ACS:Subscription interface).
     
     * @return void
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */ 
    virtual void suspend();

    /**
     * Resume the sampling.
     * Implementation of IDL resume() interface (inherited from the ACS:Subscription interface).
     
     * @return void
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */ 
    virtual void resume();


    /** 
     * Sets the sampling frequency of an already activated 
     * sampling object.
     *
     *
     * @param sFrequency   a new sampling frequency
     *                     (units are 100ns; e.g. 1000000 means 10 sample 
     *                      per second)
     
     * @return void
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    virtual void setFrequency (ACS::TimeInterval sFrequency);
  

    /**      
     * Gets the sampling frequency of an already activated 
     * sampling object.
     *
     * @param sFrequency   the current sampling frequency
     *                     (units are 100ns; e.g. 1000000 means 10 sample 
     *                      per second)
     
     * @return void
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    virtual void getFrequency (ACS::TimeInterval_out sFrequency);



    /**
     * Sets the report rate of an already activated 
     * sampling object.
     *
     * @param sFrequency   a new report rate
     *                     (units are 100ns; e.g. 10000000 means collect data
     *                      for 1 second) 
     
     * @return void
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    virtual void setRate (ACS::TimeInterval rRate);
  


    /**
     * Gets the report rate of an already activated 
     * sampling object.
     *
     * @param sFrequency   the current report rate report rate
     *                     (units are 100ns; e.g. 10000000 means collect data
     *                      for 1 second) 
     * @return void
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    virtual void getRate (ACS::TimeInterval_out rRate);



    /**
     * Initialize (activate) the sampling object, with parameters
     * passed in the constructor. 
     *
     * @throw ACSErrTypeCommon::OutOfBoundsExImpl
     * @throw ACSErrTypeCommon::MemoryFaultExImpl
     * @throw ACSErrTypeCommon::CORBAProblemExImpl
     * @throw ACSErrTypeCommon::CouldntCreateObjectExImpl 
     * @return void
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    virtual void initialize( );

    /**
     * Do the sampling. Retrieves data from the property using
     * get_sync(). Data are stored in an internal array used as a buffer. 
     *
     * @return void
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    virtual void doSamp();


    /**
     * Flush data from the internal array to the notification channel.
     *
     * @return void
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    virtual void flushSamp();


    /**
     * Internal method to retrieve the sampling frequency.
     *
     * @return sampFrequency
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    ACS::TimeInterval getSampFrequency() const { return sampFrequency; }

    /**
     * Internal method to set the sampling frequency.
     *
     * @param _sampFrequency.
     * @return void
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    void setSampFrequency(const ACS::TimeInterval& _sampFrequency);

    /**
     * Internal method to retrieve the report rate.
     *
     * @return sampReportRate
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    ACS::TimeInterval getReportRate() const { return sampReportRate; }

    /**
     * Internal method to set the report rate.
     *
     * @param _sampReportRate.
     * @return void
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    void setReportRate(const ACS::TimeInterval& _sampReportRate);

    /**
     * Internal method to get the CORBA reference to the newly activated
     * CORBA object (i.e. this sampling object).
     *
     * @return reference_p  reference to the CORBA object.
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    CORBA::Object_ptr getCORBAReference() const { return reference_p; }


    /**
     * Is the channel name of the notification channel onto which the data
     * are delivered. The name is composed by concatenation of
     * the following string: 
     * NC_ <i>Component name_</i> <i>Property name_</i>
     * <i>SampRate_</i> <i>ReportRate_</i>
     * (e.g. <i>NC_LAMP1_brightness_1000000_10000000</i>)
     *
     
     * @return channelName.
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    virtual char * getChannelName();
    

    /** 
     * Data structure delivered to the notification channel.
     * Every sample is composed by two values: a timestamp (time when
     * the sample occured) and a value.
     */
    struct SampData {
	ACS::Time timeStamp; 
	Tval val;
    };


    /**
     * Internal thread used to determine when the sampling object is
     * to be destroyed.
     *
     * @return state  1 if is in a destruction state.
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    bool isInDestructState() const { return inDestructState; };

  private:
  
  
    //ACE_CString name;  
    ACE_CString cobName; 
    ACE_CString propertyName;
    ACE_CString sampObjName;
    ACE_CString sampChannelName;

    ACS::TimeInterval sampFrequency;
    ACS::TimeInterval sampReportRate;

    bool inDestructState;

    /** BACI Component instance
     *  This is a reference to the factory CORBA object
     */
    baci::BACIComponent *cob_p;

    /**
     *  controlLoop_p is only started once the sampling object is initialized,
     *  through the call of method start. It determines the ticks, when the sampling 
     *  actually occurs; the thread is destroyed when the method destroy is called.
     */
    SamplingThread<ACS_SAMP_TL> *controlLoop_p;

    /**
     *  flush_p is only started once the sampling object is initialized,
     *  through the call of method start. It determines the time, when the sampling 
     *  are actually flushed on the NC; the thread is destroyed 
     *  when the method destroy is called.
     */
    SamplingThreadFlush<ACS_SAMP_TL> *flush_p;
 
    //ACS::ThreadManager *threadManager_p;

    ACS::Property_var genProperty_p;

    T_var propToSamp_p;

    /**
     *  Internal buffer containing all data, before the delivering to the
     *  notification channel.
     */
    ACE_Message_Queue<ACE_SYNCH> *mq_p;

//  nc::SimpleSupplier<acssamp::SampObj::SampDataBlock> *sampSupplier_p;

    nc::SimpleSupplier *sampSupplier_p;

    CORBA::Object_ptr reference_p;

    ACSSampImpl *samp_p;
};



/**
 *  The actual implementation of this class (it is a template!)
 */
#include "acssampObjImpl.i"


#endif /*!_ACSAMP_OBJ_IMPL_H*/






