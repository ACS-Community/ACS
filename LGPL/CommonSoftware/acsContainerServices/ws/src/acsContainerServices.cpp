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
 *
 * "@(#) $Id: acsContainerServices.cpp,v 1.7 2005/08/08 23:17:05 dfugate Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * acaproni  2005-04-06  created 
 */
 
#include "acsContainerServices.h"
//#include <acsutil.h> 
 
using namespace maci;
 
ContainerServices::ContainerServices(ACE_CString& compName, PortableServer::POA_ptr poa): 
    m_componentName(compName),
    m_poa(PortableServer::POA::_nil()),
    threadManager_m(logger_m)
{
    ACS_CHECK_LOGGER;
    logger_m = getNamedLogger(compName.c_str());
    ACS_TRACE("maci::ContainerServices::ContainerServices");
    // Save reference to the POA
    m_poa = PortableServer::POA::_duplicate(poa);
}

/**
 * Destructor
 */
ContainerServices::~ContainerServices()
{
  ACS_TRACE("maci::ContainerServices::~ContainerServices");
}


static char *rcsId="@(#) $Id: acsContainerServices.cpp,v 1.7 2005/08/08 23:17:05 dfugate Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


/*___oOo___*/
