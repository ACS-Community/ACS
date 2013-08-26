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
* "@(#) $Id: baciEvent.cpp,v 1.94 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001/03/19  created 
*/

#include <vltPort.h>
#include "logging.h"
#include <algorithm>
#include "baciRecovery.h"
#include "baciEvent.h"

namespace baci {

/* ------------------------------------------------------------------*/
/* --------------------- [ EventDispathcer ] ------------------------*/
/* ------------------------------------------------------------------*/

EventDispatcher::EventDispatcher() : active_m(0), shutdown_m(false)
{
}

EventDispatcher::~EventDispatcher()
{
  ACE_TRACE("baci::EventDispatcher::~EventDispatcher");
  shutdown_m = true;
  destroyEvents();
}

int
EventDispatcher::subscribe(EventStrategy * event)
{
  ACE_TRACE("baci::EventDispatcher::subscribe");

  // BAD_INV_ORDER
  if (shutdown_m==true) 
      {
      return 1;
      }

  ACE_Guard<ACE_Recursive_Thread_Mutex> guard(mutex_m);

  events_m.push_back(event);

  return 0;
}

int
EventDispatcher::unsubscribe(EventStrategy * event)
{
  ACE_TRACE("baci::EventDispatcher::unsubscribe");

  // BAD_INV_ORDER
  if (shutdown_m==true) 
      {
      return 1;
      }

  ACE_Guard<ACE_Recursive_Thread_Mutex> guard(mutex_m);

  EventStrategyVector::iterator i = find(events_m.begin(), events_m.end(), event);
  if (i != events_m.end())
      {
      events_m.erase(i);
      }

  return 0;
}

ACE_Recursive_Thread_Mutex&
EventDispatcher::getMutex()
{
  ACE_TRACE("baci::EventDispatcher::getMutex");
  return mutex_m;
}

EventStrategyVector&
EventDispatcher::getSubscribers()
{
  ACE_TRACE("baci::EventDispatcher::getSubscribers");
  return events_m;
}

void
EventDispatcher::destroyEvents()
{
  // object must be in shutdown mode (no un/subscriptions).

  ACE_TRACE("baci::EventDispatcher::destroyEvents");

  ACE_Guard<ACE_Recursive_Thread_Mutex> guard(mutex_m);

  for (EventStrategyVector::iterator iter = events_m.begin();
	iter != events_m.end(); iter++)
		(*iter)->destroy();
}

 }; 

/*___oOo___*/




