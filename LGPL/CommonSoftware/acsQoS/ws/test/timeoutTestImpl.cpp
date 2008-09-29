/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2004 
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
* "@(#) $Id: timeoutTestImpl.cpp,v 1.4 2008/09/29 09:42:48 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2004-08-24  created 
*/

static char *rcsId="@(#) $Id: timeoutTestImpl.cpp,v 1.4 2008/09/29 09:42:48 cparedes Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "timeoutTestImpl.h"

TimeOutTestImpl::TimeOutTestImpl (CORBA::ORB_ptr orb)
  : orb_ (CORBA::ORB::_duplicate (orb))
{
}

CORBA::Long
TimeOutTestImpl::echo (CORBA::Long x,
                       CORBA::Long msecs
                       ACE_ENV_ARG_DECL_NOT_USED)
{
  ACE_Time_Value tv (msecs / 1000, (msecs % 1000) * 1000);

  // ACS_SHORT_LOG((LM_DEBUG, "server (%P) Sleeping for %d msecs",
  //             tv.msec ()));
  ACE_OS::sleep (tv);

  return x;
}

void TimeOutTestImpl::shutdownOrb()
{
	orb_->shutdown(0);
}
/*___oOo___*/
