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
* "@(#) $Id: acsncArchiveConsumerCompImpl.cpp,v 1.3 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-09-26 added many more comments
* david  25/09/02  created 
*/

static char *rcsId="@(#) $Id: acsncArchiveConsumerCompImpl.cpp,v 1.3 2006/09/01 02:20:54 cparedes Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acsncArchiveConsumerCompImpl.h"
#include <ACSErrTypeCommon.h>

 using namespace ACSErrTypeCommon;


using namespace std;
 using namespace baci;

/* ----------------------------------------------------------------*/
ArchiveConsumerCompImpl::ArchiveConsumerCompImpl(const ACE_CString &name,
				   maci::ContainerServices *cs) :
    acscomponent::ACSComponentImpl(name, cs)
{
    m_count = 0;

    m_testConsumer_p = new nc::ArchiveConsumer(this);
    m_testConsumer_p->consumerReady();
}
/* ----------------------------------------------------------------*/
ArchiveConsumerCompImpl::~ArchiveConsumerCompImpl()
{   
    m_testConsumer_p->disconnect();
    m_testConsumer_p = 0;
}
/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(ArchiveConsumerCompImpl)
/* ----------------------------------------------------------------*/
/*___oOo___*/
