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
* "@(#) $Id: acsQoS.cpp,v 1.4 2006/02/10 20:41:03 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2004-08-26  created 
*/

static char *rcsId="@(#) $Id: acsQoS.cpp,v 1.4 2006/02/10 20:41:03 sharring Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acsQoS.h"

bool acsQoS::init(CORBA::ORB_ptr _orb)
{
    acsQoS::Timeout::init(_orb);  // error handling
    // here we can put init calls for other QoS capabilities, when they are added in the future.
    return true;
}

bool acsQoS::isInitialized()
{
    return Timeout::isInitialized();    // && ....
}


void acsQoS::done()
{
    Timeout::done();
    // here we can put other done calls for other QoS capabilities, when they are added in the future.
}

/*___oOo___*/
