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
* "@(#) $Id: baciBACICallback.cpp,v 1.3 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate 2005-01-17 created
*/

#include "baciBACICallback.h"
#include "baci.h"

ACE_RCSID(baci, baci, "$Id: baciBACICallback.cpp,v 1.3 2006/09/01 02:20:54 cparedes Exp $");

namespace baci {

/////////////////////////////////////////////////
// BACICallback
/////////////////////////////////////////////////

const int BACICallback::failureLimitCount_m = 2;      // try 3 times

void BACICallback::failed() 
{
  ACS_TRACE("baci::BACICallback::failed");
  failureCount_m++;
  ACS_DEBUG_PARAM("baci::BACICallback::failed", "FC: %d", failureCount_m);
  if (failureCount_m > failureLimitCount_m) 
      {
      if (doRemoveOnFailure()==true) 
	  {
	  component_mp->removeCallbackAndAction(getID());
	  }
      }
}

 }; 

/*___oOo___*/


