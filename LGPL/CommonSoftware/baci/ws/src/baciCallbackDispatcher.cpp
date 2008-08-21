/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration),
*    All rights reserved
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
* "@(#) $Id: baciCallbackDispatcher.cpp,v 1.105 2008/08/21 15:30:52 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almamgr 2002-02-05 Removed ## concatenation characters in macros.
* almamgr 2002-02-05 created
* msekoran  2001/03/06  created
*/
#include <baci.h>


#define WORKING_CALLBACK(ty)                                                           \
	case ( BACIValue::type_##ty ):                                                 \
          {                                                                            \
	    ACS::CB##ty##_var ccb = ACS::CB##ty::_unchecked_narrow(cb_p);             \
                                                                         \
	    if (CORBA::is_nil(ccb.ptr()))                                         \
		{ callback_p->failed(); return false; }                                  \
	    ccb->working(value.ty##Value(), completion, descOut);       \
                                                                         \
	    break;                                                                     \
	  }

bool baci::BACIComponent::dispatchCallback(int callbackID,
					   const BACIValue& value,
					   CBDescOut& descOut,
					   const Completion& completion,
					   const BACIMonitor * archiver)
{

  //ACS_TRACE("baci::BACIComponent::dispatchCallback");

  BACICallback* callback_p = getCallback(callbackID);
  if (callback_p==0) return false;

  // set id_tag
  descOut.id_tag = callback_p->getDescIn().id_tag;

  Callback_ptr cb_p = callback_p->getCallback();
  if (CORBA::is_nil(cb_p)==true)
    if (archiver != 0)
      {
		  // error free
		  if( completion.previousError.length() == 0 )
		  {
			ACS_ARCHIVE_PRIORITY(this->getName(),
				   archiver->getName(),
				   value.getArchiveType(),
				   value,
				   archiver->getPriority());
			return true;
		  }
		  else
		  {
		  baciErrTypeProperty::ArchiveMonitorProblemCompletion c(completion,
									 __FILE__,
									 __LINE__,
									 "baci::BACIComponent::dispatchCallback");
		  c.log();
		  return true;
		  }//if-else
      }
    else
      {
	callback_p->failed();
	return false;
      }

  BACIValue::Type type = callback_p->getType();
/*
  ACS_LOG(0, "baci::BACIComponent::dispatchCallback",
    (LM_DEBUG, "Calling working() for id_tag: %d, type: %s", descOut.id_tag, BACIValue::typeName[type].c_str()));
*/


  try
    {
      switch (type)
	{
	case (BACIValue::type_null) :
	  {
	    CBvoid_var ccb = CBvoid::_unchecked_narrow(cb_p);

	    if (CORBA::is_nil(ccb.ptr()))
		{ callback_p->failed(); return false; }
	    ccb->working(completion, descOut);

	    break;
	  }

/// User defined

WORKING_CALLBACK(double)
WORKING_CALLBACK(float)
WORKING_CALLBACK(long)
WORKING_CALLBACK(longLong)
WORKING_CALLBACK(uLongLong)
//tbdeleted WORKING_CALLBACK(pattern)
WORKING_CALLBACK(string)
WORKING_CALLBACK(doubleSeq)
WORKING_CALLBACK(floatSeq)
WORKING_CALLBACK(longSeq)
WORKING_CALLBACK(stringSeq)

	default:
	  {
	    ACS_LOG(LM_RUNTIME_CONTEXT, "baci::BACIComponent::dispatchCallback",
					(LM_ERROR, "Unsupported type %d", type));
            callback_p->failed();
            return false;
	  }
	}

    }
  catch(...)
    {
      callback_p->failed();
      return false;
    }

  callback_p->succeeded();
  return true;
}

#undef WORKING_CALLBACK

/* ---------------------------------------------------------------------------------- */

#define DONE_CALLBACK(ty)                                                              \
	case ( BACIValue::type_##ty ):                                                 \
          {                                                                            \
	    CB##ty##_var ccb = CB##ty::_unchecked_narrow(cb_p);             \
                                                                         \
	    if (CORBA::is_nil(ccb.ptr()))                                         \
		{ callback_p->failed(); return false; }                                  \
	    ccb->done(value.ty##Value(), completion, descOut);          \
                                                                         \
	    break;                                                                     \
	  }

bool baci::BACIComponent::finishCallback(int callbackID,
					 const BACIValue& value,
					 CBDescOut& descOut,
					 const Completion& completion)
{
  //ACS_TRACE("baci::BACIComponent::finishCallback");

  BACICallback* callback_p = getCallback(callbackID);
  if (callback_p==0)
      {
      return false;
      }

  // set id_tag
  descOut.id_tag = callback_p->getDescIn().id_tag;

  Callback_ptr cb_p = callback_p->getCallback();
  if (CORBA::is_nil(cb_p)==true)
    {
      callback_p->failed();
      return false;
    }

  BACIValue::Type type = callback_p->getType();
/*
  ACS_LOG(0, "baci::BACIComponent::finishCallback",
    (LM_DEBUG, "Calling done() for id_tag: %d, type: %s", descOut.id_tag, BACIValue::typeName[type].c_str()));
*/


  try
    {
      switch (type)
	{
	case (BACIValue::type_null) :
	  {
	    CBvoid_var ccb = CBvoid::_unchecked_narrow(cb_p);

	    if (CORBA::is_nil(ccb.ptr()))
		{ callback_p->failed(); return false; }
	    ccb->done(completion, descOut);

	    break;
	  }

/// User defined

DONE_CALLBACK(double)
DONE_CALLBACK(float)
DONE_CALLBACK(long)
//TBDeleted DONE_CALLBACK(pattern)
DONE_CALLBACK(longLong)
DONE_CALLBACK(uLongLong)
DONE_CALLBACK(string)
DONE_CALLBACK(doubleSeq)
DONE_CALLBACK(floatSeq)
DONE_CALLBACK(longSeq)
DONE_CALLBACK(stringSeq)

	default:
	  {
	    ACS_LOG(LM_RUNTIME_CONTEXT, "baci::BACIComponent::finishCallback",
					(LM_ERROR, "Unsupported type %d", type));
            callback_p->failed();
            return false;
	  }
	}

    }
  catch(...)
    {
      callback_p->failed();
      return false;
    }

  callback_p->succeeded();
  removeCallback(callbackID);
  return true;
}

#undef DONE_CALLBACK

/*___oOo___*/

