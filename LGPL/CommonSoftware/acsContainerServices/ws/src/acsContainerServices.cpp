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
 * "@(#) $Id: acsContainerServices.cpp,v 1.10 2007/09/03 05:59:27 cparedes Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * acaproni  2005-04-06  created 
 */
 
#include "acsContainerServices.h"
 
using namespace maci;
 
ContainerServices::ContainerServices(ACE_CString& compName, PortableServer::POA_ptr poa): 
    Logging::Loggable(compName.c_str()),
    m_componentName(compName),
    withCompListener(false),
    m_poa(PortableServer::POA::_nil()),
    threadManager_m(getLogger())
{
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

void ContainerServices::registerComponentListener(ComponentListener* listener) {
     compListener = listener;
     withCompListener = true;
}

void ContainerServices::fireComponentsAvailable (ACE_CString_Vector& compNames) {
    if (!withCompListener || compListener == NULL) {
        return;
    }

     if (compNames.size() > 0) {
         try {
             compListener->componentsAvailable(compNames);
         } catch (...) {
             ACS_SHORT_LOG((LM_INFO, "componentsAvailable implementation of client %s failed", m_componentName.c_str()));
         }
     }
}

void ContainerServices::fireComponentsUnavailable(ACE_CString_Vector& compNames){
         if (!withCompListener || compListener == NULL) {
             return;
         }

         // find out which components are interesting for the client
  /*      //TODO:find a way how to know the used components
         ACE_CString_Vector & interesting;
             
         if (compListener->includeForeignComponents()) {
             interesting = compNames;
         }
         else {
            int i;
             for (i=0; i< (int)compNames.size(); i++) {
                ACE_CString cn = compNames[i];
                 if (m_usedComponentsMap.containsKey(cn) || m_usedNonStickyComponentsMap.containsKey(cn) ) {
                     interesting.add(cn);
                 }
             }
         }
*/
         if (compNames.size() > 0) {
            try {
                 compListener->componentsUnavailable(compNames);
             } catch (...) {
                 ACS_SHORT_LOG((LM_INFO, "componentsUnavailable implementation of client %s failed", m_componentName.c_str()));
             }
         }
 }

static char *rcsId="@(#) $Id: acsContainerServices.cpp,v 1.10 2007/09/03 05:59:27 cparedes Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


/*___oOo___*/
