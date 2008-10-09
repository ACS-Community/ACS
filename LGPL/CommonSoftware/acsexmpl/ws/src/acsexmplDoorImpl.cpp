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
* "@(#) $Id: acsexmplDoorImpl.cpp,v 1.116 2008/10/09 08:41:11 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni 2004-04-06 Use the smart pointer for the properties
* gchiozzi 2003-04-09 Replaced writeXXXX with write
* david 2002-08-08 converted defines in DoorDefines.h into const static int's located in this file
* bgustafs 2002-04-11 Modified for VxWorks
* almamgr 2002-04-07 Removed poa parameter from call to ConstructorEpilogue()
* blopez   2002-04-05 writeLong changed to writeString at version initialization
* gchiozzi 2002-04-04 Replaced set_sync() with getDevIO()->write<T>()
* blopez  2002-04-04  Modified for ACSDO usage
* blopez  2002-03-27  Header removed
* blopez  2002-03-11  Created
*/

/** @file acsexmplMountImpl.cpp
 *  Source file for Mount implementation.  
 */

#include <baciDB.h>
#include <acsexmplDoorImpl.h>
#include <ACSErrTypeCommon.h>

#ifndef MAKE_VXWORKS
ACE_RCSID(acsexmpl, acsexmplDoorImpl, "$Id: acsexmplDoorImpl.cpp,v 1.116 2008/10/09 08:41:11 cparedes Exp $")
#else
static char *rcsId="$Id: acsexmplDoorImpl.cpp,v 1.116 2008/10/09 08:41:11 cparedes Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);
#endif

using namespace baci;

// Asynchronous actions
const static int OPEN_ACTION    = 0;
const static int CLOSE_ACTION   = 1;

// Door states
const static int DOOR_UNDEFINED = 0;
const static int DOOR_OPEN      = 1; 
const static int DOOR_CLOSED    = 2;
const static int DOOR_HALTED    = 3;
const static int DOOR_OPENING   = 4;
const static int DOOR_CLOSING   = 5;

// Control loop time interval [sec]
const static int CTRL_INTERVAL  = 1;

// Control loop step size [units]
const static int CTRL_STEP      = 5;

void DoorThread::onStart()
{
    ACS_SHORT_LOG((LM_INFO, "Starting Door control loop.."));
}

// main loop for the DoorThread, which updates the Door's position
void DoorThread::runLoop()
{
	// Control loop
	ACS::Time timestamp;

	// TBD: Error/Exception handling
	ACSErr::Completion_var completion;

	// Get current door position
	CORBA::Double currentPosition = door_p->m_position_sp->get_sync(completion.out());

	// Get new position
	CORBA::Double newPosition = door_p->m_ref_position_sp->get_sync(completion.out());

	if (newPosition != currentPosition) 
	{
 		// Simulated control
 		ACS_SHORT_LOG((LM_INFO, "Moving %s ...", door_p->getComponent()->getName()));
 		if (currentPosition < newPosition) 
 		{
	 		// Set state to CLOSING
	 		door_p->m_substate_sp->getDevIO()->write(DOOR_CLOSING, timestamp); 
	 		currentPosition += CTRL_STEP;
	 		if (currentPosition > newPosition) 
	 		{
				 currentPosition = newPosition;
	 		}
	 		door_p->m_position_sp->getDevIO()->write(currentPosition, timestamp);
	 		ACS_SHORT_LOG((LM_INFO, "Current position = %f", currentPosition ));         
 		} 
 		else 
 		{
	 		// Set state to OPENING
	 		door_p->m_substate_sp->getDevIO()->write(DOOR_OPENING, timestamp);
	 		currentPosition -= CTRL_STEP;
	 		if (currentPosition < newPosition) 
	 		{
		 		currentPosition = newPosition;
	 		}
	 		door_p->m_position_sp->getDevIO()->write(currentPosition, timestamp);
	 		ACS_SHORT_LOG((LM_INFO, "Current position = %f", currentPosition ));
 		} 
	} 
	else 
	{
 		// Define and set the state
 		CORBA::Double maxPosition = door_p->m_ref_position_sp->max_value();
 		CORBA::Double minPosition = door_p->m_ref_position_sp->min_value();

 		if (currentPosition == minPosition) 
		{
			door_p->m_substate_sp->getDevIO()->write(DOOR_OPEN, timestamp);
		} 
		else if (currentPosition == maxPosition) 
		{
			door_p->m_substate_sp->getDevIO()->write(DOOR_CLOSED, timestamp);
		}  
		else 
		{
			door_p->m_substate_sp->getDevIO()->write(DOOR_HALTED, timestamp);
		}
	}
}

void DoorThread::onStop()
{
    ACS_SHORT_LOG((LM_INFO, "Stopping Door control loop."));
}

//
// Door Constructor
//
Door::Door(const ACE_CString& name,
	   maci::ContainerServices * containerServices):
    CharacteristicComponentImpl(name,containerServices),
    m_ref_position_sp(new RWdouble(name+":ref_position", getComponent()),this),
    m_position_sp(new ROdouble(name+":position", getComponent()),this),
    m_substate_sp(new ROlong(name+":substate", getComponent()),this),
    m_version_sp(new ROstring(name+":version", getComponent()),this)
{
    ACS_TRACE("::Door::Door");
}

void
Door::execute()
{
  ACS_SHORT_LOG((LM_INFO,"Door::execute"));

  // Used just as parameters to getDevIO()->write()
  ACS::Time timestamp;
  
  // Set current version  

#ifndef MAKE_VXWORKS
  m_version_sp->getDevIO()->write(rcsid_acsexmpl_acsexmplDoorImpl, timestamp);
#else
  m_version_sp->getDevIO()->write(rcsId, timestamp);
#endif
  // Set current substate
  int st = DOOR_UNDEFINED;
  m_substate_sp->getDevIO()->write(st, timestamp);
  
  // Initialize control loop thread
  Door * selfPtr = this;
  m_doorThread_p = getContainerServices()->getThreadManager()->create<DoorThread, Door*>
                 ("doorControl", // Name of the thread
                   selfPtr); // pass a ptr to this as parameter to thread so it can call back to us
  m_doorThread_p->resume();
  
  ACS_SHORT_LOG((LM_INFO,"doorControl thread spawned.")); 

  ACS_SHORT_LOG((LM_INFO,"door::COMPSTATE_OPERATIONAL"));
}

// Door Destructor
Door::~Door()
{  
    ACS_TRACE("::Door::~Door");

    ACS_SHORT_LOG((LM_INFO,"Door::~Door destroying thread"));
    getContainerServices()->getThreadManager()->destroy(m_doorThread_p);
    // cleanUp() will have been already called by the container
    // and all threads should have been stopped already.
}

// Door check-substate method
// Checks for the device's substate. If the device is busy it returns
// en error. (Note: the purpose of this function is to show how to handle
// local error information.)
void 
Door::checkSubstate (CompletionImpl *&error_p) 
{
    try 
	{
	
	// Get substate
	ACS_TRACE("::Door::checkSubstate");
        ACSErr::Completion_var completion;
        CORBA::Long substate = m_substate_sp->get_sync(completion.out());
        if ( (substate == DOOR_OPENING) || (substate == DOOR_CLOSING) ) 
	    {
            error_p = new ACSErrTypeCommon::FileNotFoundCompletion(__FILE__, __LINE__, "checkSubstate", ACSErr::Error);
            return;
	    }
	}
    catch(...) 
	{
	ACS_SHORT_LOG((LM_ERROR,"::Door::checkSubstate"));
	}
    
    error_p = new ACSErrTypeOK::ACSErrOKCompletion();//no error
}

/* --------------- [ Action implementator interface ] -------------- */
ActionRequest
Door::invokeAction (int function,
		    BACIComponent *cob_p, 
		    const int &callbackID, 
		    const CBDescIn &descIn, 
		    BACIValue *value_p, 
		    Completion &completion, 
		    CBDescOut &descOut) 
{
    switch (function)
        {
        case OPEN_ACTION:
	  {
	  return openAction(cob_p, callbackID, descIn, value_p, completion, descOut);
	  }
        case CLOSE_ACTION:
	  {
	  return closeAction(cob_p, callbackID, descIn, value_p, completion, descOut);
          }
	default:
	  {
	  return reqDestroy;
          }
	}
}

/// Implementation of async. open() method
ActionRequest 
Door::openAction (BACIComponent *cob_p, 
		  const int &callbackID,
		  const CBDescIn &descIn, 
		  BACIValue *value_p,
		  Completion &completion, 
		  CBDescOut &descOut)
{
    ACE_UNUSED_ARG(descOut);
    ACS_DEBUG_PARAM("::Door::openAction", "%s", getComponent()->getName());
    
    ACS::Time timestamp;
    CompletionImpl *error_p = 0;

    DBConnector::writeCommand(getComponent()->getName(), "open", getStringifiedTimeStamp());
    
    // Set new ref_position_p
    
    try 
	{
	
        // Check for state
      
        checkSubstate(error_p);
	
        if (error_p->isErrorFree() == true) 
	    {
	       CORBA::Double minPosition = m_ref_position_sp->min_value();
	       m_ref_position_sp->getDevIO()->write(minPosition, timestamp);
	    
            // Set completion for OK
	       completion = ACSErrTypeOK::ACSErrOKCompletion();
	       delete error_p; // Memory management: object has to be released.
	    } 
	else 
	    {
	    //here memory for error_p is deleted automatically
	    completion = ACSErrTypeCommon::CouldntPerformActionCompletion(error_p, __FILE__, __LINE__, "::Door::closeAction");
	    }

	}
    catch(...) 
	{
	delete error_p;
        ACS_SHORT_LOG((LM_ERROR, "::Door::openAction"));
	completion = ACSErrTypeCommon::CouldntPerformActionCompletion(__FILE__, __LINE__, "::Door::closeAction");
	}
 
    // Note: when finished "return reqInvokeDone", otherwise 
    // "return reqInvokeWorking" and set "descOut.estimated_timeout".
    return reqInvokeDone;
}


/// implementation of async. close() method
ActionRequest 
Door::closeAction (BACIComponent *cob_p, 
		   const int &callbackID,
		   const CBDescIn &descIn, 
		   BACIValue *value_p,
		   Completion &completion, 
		   CBDescOut &descOut)
{
    ACE_UNUSED_ARG(descOut);
    ACS_DEBUG_PARAM("::Door::closeAction", "%s", getComponent()->getName());
    
    ACS::Time timestamp;
    CompletionImpl *error_p = 0;

    DBConnector::writeCommand(getComponent()->getName(), "close", getStringifiedTimeStamp());
    
    // Set new ref_position_p
    
    try 
	{
        // Check for state
        checkSubstate(error_p);
	
        if (error_p->isErrorFree() == true) 
	    {
            CORBA::Double maxPosition = m_ref_position_sp->max_value();
	        
            m_ref_position_sp->getDevIO()->write(maxPosition, timestamp);
	    
            // Set completion for OK
            completion = ACSErrTypeOK::ACSErrOKCompletion();
	    delete error_p;
	    } 
	else 
	    {
            // Set completion for ERROR
	    completion = ACSErrTypeCommon::CouldntPerformActionCompletion(error_p,
									  __FILE__, 
									  __LINE__,
									  "::Door::closeAction");
	    }
	}
    catch(...)
	{
	delete error_p;
        ACS_SHORT_LOG((LM_ERROR,"::Door::closeAction"));
	completion = ACSErrTypeCommon::CouldntPerformActionCompletion(__FILE__, __LINE__, "::Door::closeAction");
	}
    
    // Note: when finished "return reqInvokeDone", otherwise 
    // "return reqInvokeWorking" and set "descOut.estimated_timeout".
    return reqInvokeDone;
}

/* --------------------- [ CORBA interface ] ----------------------*/
void
Door::open (ACS::CBvoid_ptr cb,
	    const ACS::CBDescIn &desc)
{
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, OPEN_ACTION);  
}

void
Door::close (ACS::CBvoid_ptr cb,
	     const ACS::CBDescIn &desc)
{
    getComponent()->registerAction(BACIValue::type_null, cb, desc, this, CLOSE_ACTION);
}

void
Door::move (CORBA::Double pos)
{
    // Set new ref_position_p
    try 
	{
        ACS::Time timestamp;
	
        // Check for state
        CompletionImpl *error_p = 0;
        checkSubstate(error_p);
        if (error_p->isErrorFree() == false) 
	    {
	    delete error_p;    // Memory management: object has to be released.
            // Throw an exception
            // Note: the File-Not-Found error has been chosen arbitrarily
	    THROW_ACS_EXCEPTION(ACSErr::ACSErrTypeCommon, ACSErrTypeCommon::FileNotFound, "::Door::move");
	    }
        delete error_p;    // Memory management: object has to be released.

	try
	    {
	    m_ref_position_sp->getDevIO()->write(pos, timestamp);
	    }
	catch(ACSErr::ACSbaseExImpl &ex)
	    {
            // Throw an Out-Of-Bounds exception
	    throw ACSErrTypeCommon::OutOfBoundsExImpl(ex, __FILE__, __LINE__, "::Door::move");
	    }
	}
    catch(...)
	{
	//Note: throwing the exception here does not work since set_sync 
	//      does not throw any exception.   
	ACS_SHORT_LOG((LM_ERROR,"::Door::move"));
	}
}


ACS::RWdouble_ptr
Door::ref_position ()
{
    if (m_ref_position_sp == 0)
	{
	return ACS::RWdouble::_nil();
	}
    
    ACS::RWdouble_var prop = ACS::RWdouble::_narrow(m_ref_position_sp->getCORBAReference());
    return prop._retn();
}

ACS::ROdouble_ptr
Door::position ()
{
    if (m_position_sp == 0)
	{
	return ACS::ROdouble::_nil();
	}
    
    ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_position_sp->getCORBAReference());
    return prop._retn();
}

ACS::ROlong_ptr
Door::substate ()
{
    if (m_substate_sp == 0)
	{
	return ACS::ROlong::_nil();
	}
    
    ACS::ROlong_var prop = ACS::ROlong::_narrow(m_substate_sp->getCORBAReference());
    return prop._retn();
}

ACS::ROstring_ptr
Door::version ()
{
    if (m_version_sp == 0)
	{
        return ACS::ROstring::_nil();
	}
    
    ACS::ROstring_var prop = ACS::ROstring::_narrow(m_version_sp->getCORBAReference());
    return prop._retn();
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(Door)
/* ----------------------------------------------------------------*/



