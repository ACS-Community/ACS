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
* "@(#) $Id: acscomponentTestImpl.cpp,v 1.12 2012/01/24 01:00:04 tstaig Exp $"
*
* who       when        what
* --------  --------    ----------------------------------------------
* rcirami   2003-09-18  created

*/

#include <vltPort.h>

static char *rcsId="@(#) $Id: acscomponentTestImpl.cpp,v 1.12 2012/01/24 01:00:04 tstaig Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <acscomponentTestImpl.h>

CORBA::ORB_var orb;

//This must be used instead of "using ...." because of VxWorks
 using namespace acscomponent;


ACSComponentTestClassImpl::ACSComponentTestClassImpl(
						     const ACE_CString &name,
						     maci::ContainerServices *cs) :
  ACSComponentImpl(name,cs)
{
  
  ACS_SHORT_LOG((LM_INFO,"::ACSComponentTestClassImpl::ACSComponentTestClassImpl"));
    
}

ACSComponentTestClassImpl::~ACSComponentTestClassImpl()
{
  
  ACS_SHORT_LOG((LM_INFO,"::ACSComponentTestClassImpl::~ACSComponentTestClassImpl"));
  
}

void ACSComponentTestClassImpl::shutdown ()
{
    ACS_SHORT_LOG((LM_INFO, "acscomponentTestImpl Shutdown")); 
    orb->shutdown(1);
 }

/* --------------- [ MACI DLL support functions ] -----------------*/

//MACI_DLL_SUPPORT_FUNCTIONS(ACSComponentTestClassImpl)

/* ----------------------------------------------------------------*/



