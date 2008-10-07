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
 * "@(#) $Id: acssampObjImpl.i,v 1.10 2008/10/07 06:41:54 cparedes Exp $"
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


//
// ACSSampObjImpl Constructor
//
template <ACS_SAMP_C>
ACSSampObjImpl<ACS_SAMP_TL>::ACSSampObjImpl(const ACE_CString& _cobName,
					    const ACE_CString& _propertyName, 
					    ACS::TimeInterval _sampFrequency, ACS::TimeInterval _sampReportRate,
					    baci::BACIComponent *_m_cob, ACS::Property_var _genProperty, 
					    ACSSampImpl * _sampPtr) :
    cobName(_cobName),propertyName(_propertyName),sampFrequency(_sampFrequency),
    sampReportRate(_sampReportRate),cob_p(_m_cob), genProperty_p(_genProperty),samp_p(_sampPtr)

{
    ACS_TRACE("acssamp::ACSSampObjImpl::ACSSampObjImpl");
    
    inDestructState = false;
    
    std::ostringstream os;
    os << sampFrequency << "_" << sampReportRate;
    
    // this will be the name of the sampling object
    sampObjName = cobName+"_" + propertyName + "_" + ACE_CString(os.str().c_str());

    // this string will be the NC channel name
    sampChannelName="NC_" + sampObjName;
   
    // internal members initialization
    controlLoop_p = 0;
    flush_p = 0;
    reference_p = CORBA::Object::_nil();
    //threadManager_p = 0;
    sampSupplier_p = 0;
}


//
// ACSSampObjImpl Destructor
//
template <ACS_SAMP_C>
ACSSampObjImpl<ACS_SAMP_TL>::~ACSSampObjImpl()
{
    ACS_TRACE("acssamp::ACSSampObjImpl::~ACSSampObjImpl");

    // most of the things are handled in the destroy method

    // set destruction flag
    inDestructState = true;
    
    // stop threads
    // cob_p->stopAllThreads();

    //if (threadManager_p)
    //delete threadManager_p;

    if(controlLoop_p)
	delete controlLoop_p;

    if(flush_p)
	delete flush_p;

    // clean-up associated with internal buffer
    if (mq_p)
	delete mq_p;
    
    // if the object is correctly destroyed, it will be removed from the
    // ACSSampImpl internal list
    samp_p->removeComponentfromList(reference_p);

    ACS_SHORT_LOG((LM_INFO,"ACSSamp deleting object %s",sampObjName.c_str()));
}



//
// ACSSampObjImpl initialize method. It is called from the factory
// object and initializes the sampling object with all user-defined data.
// If no exceptions are thrown, then the object is ready to start sampling.
//
template <ACS_SAMP_C> 
void ACSSampObjImpl<ACS_SAMP_TL>::initialize()
{
    ACS_TRACE("acssamp::ACSSampObjImpl::initialize");

    try
	{
	if (sampFrequency < 1 || sampReportRate < 1)
	    {
	    ACS_SHORT_LOG((LM_INFO,"frequency or polling interval must be greater then 1"));
	    ACSErrTypeCommon::OutOfBoundsExImpl err = 
		ACSErrTypeCommon::OutOfBoundsExImpl(__FILE__,__LINE__,"ACSSampObjImpl::initialize");
	    err.addData("Frequency low limit 1",sampFrequency);
	    err.addData("Report rate low limit 1",sampReportRate);
	    throw err;
	    }

	sampSupplier_p = new nc::SimpleSupplier(sampChannelName.c_str(), 0);
//	sampSupplier_p = new nc::SimpleSupplier<acssamp::SampObj::SampDataBlockSeq>(sampNames);
	if (!sampSupplier_p) 
	    {
	    ACS_SHORT_LOG((LM_INFO,"Failed to activate notification channel"));
	    ACSErrTypeCommon::MemoryFaultExImpl err = ACSErrTypeCommon::MemoryFaultExImpl(__FILE__,__LINE__,"ACSSampObjImpl::initialize");
	    err.addData("Notification channel","not created");
	    throw err;
	    }

	/*threadManager_p = new ACS::ThreadManager();
	if (!threadManager_p) 
	    {
	    ACS_SHORT_LOG((LM_INFO,"Failed to activate thread manager"));
	    MemoryFaultExImpl err = 
		MemoryFaultExImpl(__FILE__,__LINE__,"ACSSampObjImpl::initialize");
	    err.addData("Thread Manager","not created");
	    throw err;
	    }*/

	if( !(mq_p = new ACE_Message_Queue<ACE_SYNCH>(100000,1000) ))
	    {
	    ACS_SHORT_LOG((LM_INFO,"Failed to activate message queue"));
	    ACSErrTypeCommon::MemoryFaultExImpl err = 
		ACSErrTypeCommon::MemoryFaultExImpl(__FILE__,__LINE__,"ACSSampObjImpl::initialize");
	    err.addData("Message queue","not created");
	    throw err;
	    }
      
	propToSamp_p = T::_narrow(genProperty_p.in());
	if (CORBA::is_nil(propToSamp_p.in()))
	    {
	    ACS_SHORT_LOG((LM_INFO,"Failed to obtain property reference"));
	    ACSErrTypeCommon::CORBAProblemExImpl err = 
		ACSErrTypeCommon::CORBAProblemExImpl(__FILE__,__LINE__,"ACSSampObjImpl::initialize");
	    throw err;
	    }


 	ACS_DEBUG_PARAM("acssamp::ACSSampObjImpl::initialize","activating CORBA object  %s",
			sampObjName.c_str());      

	reference_p = BACI_CORBA::ActivateCORBAObject(this,sampObjName.c_str());
	if (CORBA::is_nil(reference_p))
	    {
	    ACS_SHORT_LOG((LM_INFO,"XXXX Failed to activate CORBA object"));
	    ACSErrTypeCommon::CouldntCreateObjectExImpl err =
		ACSErrTypeCommon::CouldntCreateObjectExImpl(__FILE__,__LINE__,"ACSSampObjImpl::initialize");
	    err.addData("Not created",sampObjName.c_str());
	    throw err;
	    }

	ACS_SHORT_LOG((LM_INFO,"CORBA object  %s activated",sampObjName.c_str()));
	//_remove_ref();

	}
    // we catch everything and just rethrow
    catch(...)
	{
	//if (threadManager_p)
	//delete threadManager_p;	

	if(controlLoop_p)
	    delete controlLoop_p;
	
	if(flush_p)
	    delete flush_p;

        // clean-up associated with NC
	if (sampSupplier_p)
	    {
	    sampSupplier_p->disconnect();
	    sampSupplier_p = 0;
	    }

        // clean-up associated with internal buffer
	if (mq_p)
	    delete mq_p;

 	throw;
	}
 
    // the object is correctly created; it will be inserted in
    // the ACSSampImpl internal list of active objects
    samp_p->addComponenttoList(reference_p);   
}


// implementation of  start() method
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::start ()
{
    ACS_TRACE("acssamp::ACSSampObjImpl::start");

    //  ACS_SHORT_LOG((LM_INFO,"::SampObjImpl::start %s", cob_p->getName()));
    
    baci::DBConnector::writeCommand(cob_p->getName(), "start", getStringifiedTimeStamp());
    
    const ACS::TimeInterval responseTime=1*1000*1000*1;    // 0.1s

    ACE_CString sampThreadName = sampObjName + "_thread";

    ACE_CString flushThreadName = sampObjName + "_f_thread";
   
    ACSSampObjImpl<ACS_SAMP_TL> *selfPtr = this;

// starting sampling thread    
    if(!controlLoop_p)
	{
	controlLoop_p = new SamplingThread<ACS_SAMP_TL>(sampThreadName, selfPtr, responseTime, sampFrequency);

	//controlLoop_p = threadManager_p->create<SamplingThread<ACS_SAMP_TL>, ACSSampObjImpl<ACS_SAMP_TL> *>(sampThreadName.c_str(), selfPtr, responseTime, sampFrequency);
	ACS_DEBUG("acssamp::ACSSampObjImpl::start","thread created");
	controlLoop_p->resume();
	}
    else
	{
	controlLoop_p->resume();
	ACS_DEBUG("acssamp::ACSSampObjImpl::start","thread resumed");
	}


// starting flushing thread
    if(!flush_p)
        {
	flush_p = new SamplingThreadFlush<ACS_SAMP_TL>(flushThreadName, selfPtr, responseTime, sampFrequency);

	//flush_p = threadManager_p->create<SamplingThreadFlush<ACS_SAMP_TL>, ACSSampObjImpl<ACS_SAMP_TL> *>(flushThreadName.c_str(), selfPtr, responseTime, sampReportRate);
	ACS_DEBUG("acssamp::ACSSampObjImpl::start","flush thread created");
	flush_p->resume();
	}
    else
	{
	ACS_DEBUG("acssamp::ACSSampObjImpl::start","flush thread resumed");
	flush_p->resume();
	}
}


// implementation of  stop() method
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::stop ()
{

    ACS_TRACE("acssamp::ACSSampObjImpl::stop");
    
    baci::DBConnector::writeCommand(cob_p->getName(), "stop", getStringifiedTimeStamp());
    
    if( controlLoop_p )
	{
	controlLoop_p->stop();
	ACS_DEBUG("acssamp::ACSSampObjImpl::start","thread stopped");
	}

    if( flush_p )
	{
	flush_p->stop();
	ACS_DEBUG("acssamp::ACSSampObjImpl::stop","flush thread stopped");
	}
    
}



// implementation of  suspend() method
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::suspend ()
{

    ACS_TRACE("acssamp::ACSSampObjImpl::suspend");

    if( controlLoop_p )
	{
	controlLoop_p->suspend();
	ACS_DEBUG("acssamp::ACSSampObjImpl::suspend","thread suspended");
	}
    if( flush_p )
	{
	flush_p->suspend();
	ACS_DEBUG("acssamp::ACSSampObjImpl::suspend","flush thread suspended");
	}

}


// implementation of  resume() method
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::resume ()
{

    ACS_TRACE("acssamp::ACSSampObjImpl::resume");

    if( controlLoop_p )
	{
	controlLoop_p->resume();
	ACS_DEBUG("acssamp::ACSSampObjImpl::resume","thread resumed");
	}
    if( flush_p )
	{
	flush_p->resume();
	ACS_DEBUG("acssamp::ACSSampObjImpl::resume","flush thread resumed");
	}
   
}



// implementation of  destroy() method
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::destroy()
{

    ACS_TRACE("acssamp::ACSSampObjImpl::destroy");

    if (inDestructState) return;
    inDestructState = true;

    if( controlLoop_p )
	{
	controlLoop_p->terminate();
	ACS_DEBUG_PARAM("acssamp::ACSSampObjImpl::destroy","thread %s destroyed",controlLoop_p->getName().c_str()); 
	}

    if( flush_p )
	{
	flush_p->terminate();
	ACS_DEBUG_PARAM("acssamp::ACSSampObjImpl::destroy","thread %s destroyed",flush_p->getName().c_str());
	}

    // clean-up associated with NC
    if (sampSupplier_p)
	{
	sampSupplier_p->disconnect();
	sampSupplier_p = 0;
	}

   
    if (!CORBA::is_nil(reference_p))
	{
	// this calls delete on this object, so DO NOT use any of its variables anymore
	if (!BACI_CORBA::DestroyCORBAObject(reference_p))
	    {
	    ACS_LOG(LM_RUNTIME_CONTEXT, "acssamp::ACSSampObjImpl::destroy",
		    (LM_ERROR, "Failed to destroy CORBA object '%s', controlLoop_p->getName()"));
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
void ACSSampObjImpl<ACS_SAMP_TL>::setSampFrequency(const ACS::TimeInterval& _sampFrequency)
{
    sampFrequency=_sampFrequency;
}


// implementation of  internal setReportRate() method
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::setReportRate(const ACS::TimeInterval& _sampReportRate)
{
    sampReportRate = _sampReportRate;
}


// implementation of  interface method setFrequency
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::setFrequency (ACS::TimeInterval sFrequency)
{
    setSampFrequency(sFrequency);
}


// implementation of  interface method getFrequency
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::getFrequency (ACS::TimeInterval_out sFrequency)
{
    sFrequency = getSampFrequency();
}


// implementation of  interface method setRate
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::setRate (ACS::TimeInterval sRate)
{
    setReportRate(sRate);
}


// implementation of  interface method getRate
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::getRate (ACS::TimeInterval_out sRate)
{
    sRate = getReportRate();
}


// implementation of  interface method getChanelName. Returns
// the notification channel name, to be used when connecting a client
template <ACS_SAMP_C>
char * ACSSampObjImpl<ACS_SAMP_TL>::getChannelName ()
{

    return CORBA::string_dup(sampChannelName.c_str());
}




// implementation of method doSamp(). It actually perform the sampling
// by calling the property get_sync() function. Data are stored in
// a temporary buffer.
template <ACS_SAMP_C>
void ACSSampObjImpl<ACS_SAMP_TL>::doSamp()
{  
    SampData currentVal;

    if ( propToSamp_p.ptr() != T::_nil())
	{
	/*
	 * Get the current value of the property
	 */
	ACSErr::Completion_var completion;
	currentVal.val = propToSamp_p->get_sync(completion.out());
	//currentVal.timeStamp = getTimeStamp();
	CompletionImpl co = completion;
	currentVal.timeStamp = co.getTimeStamp();

	// ACS_SHORT_LOG((LM_INFO,"Time: %u",currentVal.timeStamp));
	// ACS_SHORT_LOG((LM_INFO,"Value: %f",currentVal.val));

	// this constructor manages the memory automatically
	ACE_Message_Block *mb = new ACE_Message_Block(sizeof(currentVal));
	if(mb == NULL)
	    {
	    ACS_SHORT_LOG((LM_INFO,"ACSSampObjImpl::doSamp error allocating ACE_Message_Block"));
	    }

	mb->copy((const char *) & currentVal, sizeof(currentVal));

	if(mq_p->enqueue_prio(mb) == -1)
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
    if (mq_p->is_empty())
	{
	return;
	}

    //ACS_SHORT_LOG((LM_INFO,"Deque (0 = not empty) %d",mq_p->is_empty()));
 
    SampData data;
    ACE_Message_Block *mbf;

    size_t len = mq_p->message_length()/sizeof(data);

    //ACS_SHORT_LOG((LM_INFO,"ACSSampObjImpl::flushSamp message len=%d",len));

    unsigned int index = 0;
    
    acssamp::SampObj::SampDataBlockSeq_var theSeq = new acssamp::SampObj::SampDataBlockSeq(len);
    theSeq->length(len);
    
    while (len-- > 0)
	{

        // extract data from the queue and store it in the sequence 
	mq_p->dequeue_head(mbf);
	ACE_OS::memmove((char *) &data,mbf->rd_ptr(),sizeof(data));
	mbf->rd_ptr(sizeof(data));
     
 	//cout << "VVVVVVVVVVVVVVVVV " << data.val << "   " << data.timeStamp << endl;

	theSeq[index].sampTime=data.timeStamp;
	theSeq[index].sampVal <<= data.val;
	index++;

	}
  
    mbf->release();

    sampSupplier_p->publishData(theSeq.in());
}


  
/*___oOo___*/

