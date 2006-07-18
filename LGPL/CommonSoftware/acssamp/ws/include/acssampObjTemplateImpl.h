/*******************************************************************************
 * ALMA - Atacama Large Millimiter Array
 * (c) European Southern Observatory, 2003 
 *
 *This library is free software; you can redistribute it and/or
 *modify it under the terms of the GNU Lesser General Public
 *License as published by the Free Software Foundation; either
 *version 2.1 of the License, or (at your option) any later version.
 *
 *This library is distributed in the hope that it will be useful,
 *but WITHOUT ANY WARRANTY; without even the implied warranty of
 *MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *Lesser General Public License for more details.
 *
 *You should have received a copy of the GNU Lesser General Public
 *License along with this library; if not, write to the Free Software
 *Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *
 * "@(#) $Id: acssampObjTemplateImpl.h,v 1.30 2006/07/18 17:20:56 dfugate Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * oat  2003-04-29  created 
 */

/************************************************************************
 *   NAME
 *------------------------------------------------------------------------
 */

/** @file acssampObjTemplateImpl.h
 *  Source file for sampling object implementatation (it is a template class).  
 */

#include <baciDB.h>

using namespace baci;
using namespace maci;
using namespace std;
using namespace ACSErrTypeCommon;

// sampling thread

template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::sampThreadWorker(void *param_p)
{
    if(!param_p)
	{
	return;
	}

    ACS_TRACE("ACSSamp::ACSSampObjImpl::sampThreadWorker");
    
    BACIThreadParameter *baciParameter_p = (BACIThreadParameter *)param_p;
    BACIThread *myself_p = baciParameter_p->getBACIThread();
     
    // Variables have to be passed explicitly 
    ACSSampObjImpl *samp_p = (ACSSampObjImpl *)baciParameter_p->getParameter();
    
    if (BACIThread::InitThread) 
	{
	BACIThread::InitThread(myself_p->getName().c_str());
	}

    ACS_DEBUG_PARAM("ACSSamp::ACSSampObjImpl::sampThreadWorker","Starting thread %s",myself_p->getName().c_str());  
    ACS_SHORT_LOG((LM_INFO,"ACSSamp Starting thread %s",myself_p->getName().c_str()));
  

    while(myself_p->check() && !samp_p->isInDestructState())
	{

	if(!myself_p->isSuspended() && !samp_p->isInDestructState())
	    {
	
	    //  ACS_DEBUG("ACSSamp::ACSSampObjImpl::sampThreadWorker","sampling ...");

            // perform the sampling
	    samp_p->doSamp();

	    myself_p->sleep();
	    }

	}
    if (BACIThread::DoneThread) 
	{
	BACIThread::DoneThread();
	}

    delete baciParameter_p;
    myself_p->setStopped();
    
}



// flushing thread
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::sampThreadFlush(void *param_pf)
{
    if(!param_pf)
	{
	return;
	}

    ACS_TRACE("ACSSamp::ACSSampObjImpl::sampThreadFlush");
    
    BACIThreadParameter *baciParameter_pf = (BACIThreadParameter *)param_pf;
    BACIThread *myself_pf = baciParameter_pf->getBACIThread();
     
    // Variables have to be passed explicitly 
    ACSSampObjImpl *samp_pf = (ACSSampObjImpl *)baciParameter_pf->getParameter();
    
    if (BACIThread::InitThread) 
	{
	BACIThread::InitThread(myself_pf->getName().c_str());
	}

    ACS_DEBUG_PARAM("ACSSamp::ACSSampObjImpl::sampThreadFlush","Starting thread %s",myself_pf->getName().c_str());  
      
    while(myself_pf->check() && !samp_pf->isInDestructState())
	{

	if(!myself_pf->isSuspended() && !samp_pf->isInDestructState())
	    {
	
	    
	    //  ACS_DEBUG("ACSSamp::ACSSampObjImpl::sampThreadWorker","flushing on the NC ...");
	      samp_pf->flushSamp();
	      myself_pf->sleep();

	    }

	}
    if (BACIThread::DoneThread) 
	{
	BACIThread::DoneThread();
	}

    delete baciParameter_pf;
    myself_pf->setStopped();
    
}




//
// ACSSampObjImpl Constructor
//
template <ACS_SAMP_C>
ACSSampObjImpl<ACS_SAMP_TL>::ACSSampObjImpl(const ACE_CString& _cobName,
					    const ACE_CString& _propertyName, 
					    TimeInterval _sampFrequency, TimeInterval _sampReportRate,
					    BACIComponent *_m_cob, ACS::Property_var _genProperty, 
					    ACSSampImpl * _sampPtr) :
    cobName(_cobName),propertyName(_propertyName),sampFrequency(_sampFrequency),
    sampReportRate(_sampReportRate),m_cob(_m_cob), genProperty(_genProperty),sampPtr(_sampPtr)

{

    ACS_TRACE("ACSSamp::ACSSampObjImpl::ACSSampObjImpl");
    
    inDestructState=false;
    
    ostringstream os;
    os << sampFrequency << "_" << sampReportRate;
    
    // this will be the name of the sampling object
    sampObjName = cobName+"_"+propertyName+"_"+ACE_CString(os.str().c_str());

    // this string will be the NC channel name
    sampChannelName="NC_"+sampObjName;
  
    // internal members initialization
    m_controlLoop_p=BACIThread::NullBACIThread;
    m_flush_p=BACIThread::NullBACIThread;
    m_reference=CORBA::Object::_nil();
    threadManager=NULL;
    m_SampSupplier_p=NULL;

}


//
// ACSSampObjImpl Destructor
//
template <ACS_SAMP_C>
ACSSampObjImpl<ACS_SAMP_TL>::~ACSSampObjImpl()
{

    ACS_TRACE("ACSSamp::ACSSampObjImpl::~ACSSampObjImpl");

    // most of the things are handled in the destroy method

    // set destruction flag
    inDestructState = true;
    
    // stop threads
    // m_cob->stopAllThreads();

    if (threadManager)
	delete threadManager;

    // clean-up associated with internal buffer
	if (mq_)
	    delete mq_;
    
    // if the object is correctly destroyed, it will be removed from the
    // ACSSampImpl internal list
    sampPtr->removeComponentfromList(m_reference);

    ACS_SHORT_LOG((LM_INFO,"ACSSamp deleting object %s",sampObjName.c_str()));

}



//
// ACSSampObjImpl initialize method. It is called from the factory
// object and initializes the sampling object with all user-defined data.
// If no exceptions are thrown, then the object is ready to start sampling.
//
template <ACS_SAMP_C> 
void ACSSampObjImpl<ACS_SAMP_TL>::initialize()
    throw (CORBA::SystemException, OutOfBoundsExImpl,MemoryFaultExImpl,
	   CORBAProblemExImpl,CouldntCreateObjectExImpl)
{

    ACS_TRACE("ACSSamp::ACSSampObjImpl::initialize");

    try
	{

	if (sampFrequency < 1 || sampReportRate < 1)
	    {
	    ACS_SHORT_LOG((LM_INFO,"frequency or polling interval must be greater then 1"));
	    OutOfBoundsExImpl err = 
		OutOfBoundsExImpl(__FILE__,__LINE__,"ACSSampObjImpl::initialize");
	    err.addData("Frequency low limit 1",sampFrequency);
	    err.addData("Report rate low limit 1",sampReportRate);
	    throw err;
	    }

       m_SampSupplier_p = new nc::SimpleSupplier(sampChannelName.c_str(), 0);
//	m_SampSupplier_p = new nc::SimpleSupplier<ACSSamp::SampObj::SampDataBlockSeq>(sampNames);
	if (!m_SampSupplier_p) 
	    {
	    ACS_SHORT_LOG((LM_INFO,"Failed to activate notification channel"));
	    MemoryFaultExImpl err = MemoryFaultExImpl(__FILE__,__LINE__,"ACSSampObjImpl::initialize");
	    err.addData("Notification channel","not created");
	    throw err;
	    }

	threadManager = new BACIThreadManager();
	if (!threadManager) 
	    {
	    ACS_SHORT_LOG((LM_INFO,"Failed to activate thread manager"));
	    MemoryFaultExImpl err = 
		MemoryFaultExImpl(__FILE__,__LINE__,"ACSSampObjImpl::initialize");
	    err.addData("Thread Manager","not created");
	    throw err;
	    }

	if( !(mq_ = new ACE_Message_Queue<ACE_SYNCH>(100000,1000) ))
	    {
	    ACS_SHORT_LOG((LM_INFO,"Failed to activate message queue"));
	    MemoryFaultExImpl err = 
		MemoryFaultExImpl(__FILE__,__LINE__,"ACSSampObjImpl::initialize");
	    err.addData("Message queue","not created");
	    throw err;
	    }
      
	propToSamp = T::_narrow(genProperty.in());
	if (CORBA::is_nil(propToSamp.in()))
	    {
	    ACS_SHORT_LOG((LM_INFO,"Failed to obtain property reference"));
	    CORBAProblemExImpl err = 
		CORBAProblemExImpl(__FILE__,__LINE__,"ACSSampObjImpl::initialize");
	    throw err;
	    }


 	ACS_DEBUG_PARAM("ACSSamp::ACSSampObjImpl::initialize","activating CORBA object  %s",
			sampObjName.c_str());      

	m_reference = BACI_CORBA::ActivateCORBAObject(this,sampObjName.c_str());
	if (CORBA::is_nil(m_reference))
	    {
	    ACS_SHORT_LOG((LM_INFO,"XXXX Failed to activate CORBA object"));
	    CouldntCreateObjectExImpl err =
		CouldntCreateObjectExImpl(__FILE__,__LINE__,"ACSSampObjImpl::initialize");
	    err.addData("Not created",sampObjName.c_str());
	    throw err;
	    }

	ACS_SHORT_LOG((LM_INFO,"CORBA object  %s activated",sampObjName.c_str()));
	//_remove_ref();

	}
    // we catch everything and just rethrow
    catch(...)
	{
	if (threadManager)
	    delete threadManager;	

        // clean-up associated with NC
	if (m_SampSupplier_p)
	    {
	    m_SampSupplier_p->disconnect();
	    m_SampSupplier_p=NULL;
	    }

        // clean-up associated with internal buffer
	if (mq_)
	    delete mq_;

 	throw;
	}
 

	// the object is correctly created; it will be inserted in
        // the ACSSampImpl internal list of active objects
	sampPtr->addComponenttoList(m_reference);
    
}


// implementation of  start() method
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::start ()
    throw (CORBA::SystemException)
{

    ACS_TRACE("ACSSamp::ACSSampObjImpl::start");

    //  ACS_SHORT_LOG((LM_INFO,"::SampObjImpl::start %s", m_cob->getName()));
    
    DBConnector::writeCommand(m_cob->getName(), "start", getStringifiedTimeStamp());
    

    const TimeInterval responseTime=1*1000*1000*10;    // 1s

    ACE_CString sampThreadName = sampObjName + "_thread";
    ACE_CString flushThreadName = sampObjName + "_f_thread";

// starting sampling thread    
    if(!m_controlLoop_p)
	{
	m_controlLoop_p = threadManager->create(sampThreadName.c_str(), (void *)sampThreadWorker, (void *)this, responseTime, sampFrequency );
	ACS_DEBUG("ACSSamp::ACSSampObjImpl::start","thread created");
	}
    else
	{
	m_controlLoop_p->resume();
	ACS_DEBUG("ACSSamp::ACSSampObjImpl::start","thread resumed");
	}


// starting flushing thread
    if(!m_flush_p)
        {
	m_flush_p = threadManager->create(flushThreadName.c_str(), (void *)sampThreadFlush, (void *)this, responseTime, sampReportRate );
	ACS_DEBUG("ACSSamp::ACSSampObjImpl::start","flush thread created");
	}
    else
	{
	m_flush_p->resume();
	ACS_DEBUG("ACSSamp::ACSSampObjImpl::start","flush thread resumed");
	}
}



// implementation of  stop() method
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::stop ()
    throw (CORBA::SystemException)
{

    ACS_TRACE("ACSSamp::ACSSampObjImpl::stop");
    
    DBConnector::writeCommand(m_cob->getName(), "stop", getStringifiedTimeStamp());
    
    if( m_controlLoop_p )
	{
	m_controlLoop_p->stop();
	ACS_DEBUG("ACSSamp::ACSSampObjImpl::start","thread stopped");
	}

    if( m_flush_p )
	{
	m_flush_p->stop();
	ACS_DEBUG("ACSSamp::ACSSampObjImpl::stop","flush thread stopped");
	}
    
}



// implementation of  suspend() method
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::suspend ()
    throw (CORBA::SystemException)
{

    ACS_TRACE("ACSSamp::ACSSampObjImpl::suspend");

    if( m_controlLoop_p )
	{
	m_controlLoop_p->suspend();
	ACS_DEBUG("ACSSamp::ACSSampObjImpl::suspend","thread suspended");
	}
    if( m_flush_p )
	{
	m_flush_p->suspend();
	ACS_DEBUG("ACSSamp::ACSSampObjImpl::suspend","flush thread suspended");
	}

}


// implementation of  resume() method
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::resume ()
    throw (CORBA::SystemException)
{

    ACS_TRACE("ACSSamp::ACSSampObjImpl::resume");

    if( m_controlLoop_p )
	{
	m_controlLoop_p->resume();
	ACS_DEBUG("ACSSamp::ACSSampObjImpl::resume","thread resumed");
	}
    if( m_flush_p )
	{
	m_flush_p->resume();
	ACS_DEBUG("ACSSamp::ACSSampObjImpl::resume","flush thread resumed");
	}
   
}



// implementation of  destroy() method
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::destroy()
    throw (CORBA::SystemException)
{

    ACS_TRACE("ACSSamp::ACSSampObjImpl::destroy");

    if (inDestructState) return;
    inDestructState = true;

    if( m_controlLoop_p )
	{
	m_controlLoop_p->terminate();
	ACS_DEBUG_PARAM("ACSSamp::ACSSampObjImpl::destroy","thread %s destroyed",m_controlLoop_p->getName().c_str()); 
	}

    if( m_flush_p )
	{
	m_flush_p->terminate();
	ACS_DEBUG_PARAM("ACSSamp::ACSSampObjImpl::destroy","thread %s destroyed",m_flush_p->getName().c_str());
	}

    // clean-up associated with NC
    if (m_SampSupplier_p)
	{
	m_SampSupplier_p->disconnect();
	m_SampSupplier_p=0;
	}

   
    if (!CORBA::is_nil(m_reference))
	{
	// this calls delete on this object, so DO NOT use any of its variables anymore
	if (!BACI_CORBA::DestroyCORBAObject(m_reference))
	    {
	    ACS_LOG(LM_RUNTIME_CONTEXT, "ACSSamp::ACSSampObjImpl::destroy",
		    (LM_ERROR, "Failed to destroy CORBA object '%s', m_controlLoop_p->getName()"));
	    }
	else
	    {
	    _remove_ref();
	    //delete this;
	    }
	}

    //delete this;
    //this->_remove_ref();
}




// implementation of  internal setSampFrequency() method
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::setSampFrequency(const TimeInterval& _sampFrequency)
{
    sampFrequency=_sampFrequency;
}


// implementation of  internal setReportRate() method
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::setReportRate(const TimeInterval& _sampReportRate)
{
    sampReportRate=_sampReportRate;
}


// implementation of  interface method set_sampFrequency
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::set_sampFrequency (ACS::TimeInterval sFrequency
						     )
    throw (CORBA::SystemException)
{
    setSampFrequency(sFrequency);
}


// implementation of  interface method get_sampFrequency
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::get_sampFrequency (ACS::TimeInterval_out sFrequency
						     )
    throw (CORBA::SystemException)
{
    sFrequency=getSampFrequency();
}


// implementation of  interface method set_ReportRate
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::set_reportRate (ACS::TimeInterval sRate
						  )
    throw (CORBA::SystemException)
{
    setReportRate(sRate);
}


// implementation of  interface method get_reportRate
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::get_reportRate (ACS::TimeInterval_out sRate
						  )
    throw (CORBA::SystemException)
{
    sRate=getReportRate();
}


// implementation of  interface method getChanelName. Returns
// the notification channel name, to be used when connecting a client
template <ACS_SAMP_C>
char * ACSSampObjImpl<ACS_SAMP_TL>::getChannelName ()
    throw (CORBA::SystemException)
{

    return CORBA::string_dup(sampChannelName.c_str());
}




// implementation of method doSamp(). It actually perform the sampling
// by calling the property get_sync() function. Data are stored in
// a temporary buffer.
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::doSamp()
{
    
    sampData currentVal;

    if ( propToSamp.ptr() != T::_nil())
	{
	/*
	 * Get the current value of the property
	 */
	ACSErr::Completion_var completion;
	currentVal.val = propToSamp->get_sync(completion.out());
	currentVal.timeStamp = getTimeStamp();

	// ACS_SHORT_LOG((LM_INFO,"Time: %u",currentVal.timeStamp));
	// ACS_SHORT_LOG((LM_INFO,"Value: %f",currentVal.val));

	// this constructor manages the memory automatically
	ACE_Message_Block *mb = new ACE_Message_Block(sizeof(currentVal));
	if(mb == NULL)
	    {
	    ACS_SHORT_LOG((LM_INFO,"ACSSampObjImpl::doSamp error allocating ACE_Message_Block"));
	    }

	mb->copy((const char *) & currentVal, sizeof(currentVal));

	if(mq_->enqueue_prio(mb) == -1)
	    {
	    ACS_SHORT_LOG((LM_INFO,"ACSSampObjImpl::doSamp error enqueueing ACE_Message_Block"));
	    }

	}
}


// implementation of method flushSamp(). It actually flushes all collected
// data to the NC and clears the internal buffer.
template <ACS_SAMP_C>
void  ACSSampObjImpl<ACS_SAMP_TL>::flushSamp()
{

    if (mq_->is_empty())
	{
	return;
	}

    //ACS_SHORT_LOG((LM_INFO,"Deque (0 = not empty) %d",mq_->is_empty()));
 
    sampData data;
    ACE_Message_Block *mbf;

    size_t len = mq_->message_length()/sizeof(data);

    //ACS_SHORT_LOG((LM_INFO,"ACSSampObjImpl::flushSamp message len=%d",len));

    unsigned int index = 0;
    
    ACSSamp::SampObj::SampDataBlockSeq_var theSeq = new ACSSamp::SampObj::SampDataBlockSeq(len);
    theSeq->length(len);
    
    while (len-- > 0)
	{

        // extract data from the queue and store it in the sequence 
	mq_->dequeue_head(mbf);
	ACE_OS::memmove((char *) &data,mbf->rd_ptr(),sizeof(data));
	mbf->rd_ptr(sizeof(data));
     
//	cout << "VVVVVVVVVVVVVVVVV " << data.val << "   " << data.timeStamp << endl;

	theSeq[index].sampTime=data.timeStamp;
	theSeq[index].sampVal <<= data.val;
	index++;

	}
  
    mbf->release();

    m_SampSupplier_p->publishData(theSeq.in());
 
}


  
/*___oOo___*/

