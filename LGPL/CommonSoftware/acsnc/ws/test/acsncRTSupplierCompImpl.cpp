/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) Associated Universities Inc., 2002 
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
* "@(#) $Id: acsncRTSupplierCompImpl.cpp,v 1.13 2008/10/01 03:14:56 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-09-26 added many more comments
* david  25/09/02  created 
*/

static char *rcsId="@(#) $Id: acsncRTSupplierCompImpl.cpp,v 1.13 2008/10/01 03:14:56 cparedes Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acsncRTSupplierCompImpl.h"

using namespace std;
 using namespace baci;
/* ----------------------------------------------------------------*/
RTSupplierCompImpl::RTSupplierCompImpl(const ACE_CString &name,
				       maci::ContainerServices *cs) :
    acscomponent::ACSComponentImpl(name,cs)
{
    m_testSupplier_p = new nc::RTSupplier("blar", this);
}
/* ----------------------------------------------------------------*/
RTSupplierCompImpl::~RTSupplierCompImpl()
{
    m_testSupplier_p->disconnect();
    m_testSupplier_p=0;
}
/* --------------------- [ CORBA interface ] ----------------------*/
void
RTSupplierCompImpl::sendEvents(short numEvents)
{
    acsnc::EventDescription descrip;
    descrip.name = CORBA::string_dup("real-time");
    descrip.timestamp = 101;
    descrip.count = 101;

    for(short i=0; i<numEvents; i++)
	{
	m_testSupplier_p->publishData<acsnc::EventDescription>(descrip);
	ACS_SHORT_LOG((LM_ALERT, "Sent an event via RTSupplier."));
	ACE_OS::sleep(1);
	}
}
/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(RTSupplierCompImpl)
/* ----------------------------------------------------------------*/
/*___oOo___*/
