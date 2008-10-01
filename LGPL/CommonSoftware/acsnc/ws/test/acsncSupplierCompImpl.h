#ifndef _acsnc_supplier_comp_impl_h_
#define _acsnc_supplier_comp_impl_h_
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
* "@(#) $Id: acsncSupplierCompImpl.h,v 1.7 2008/10/01 03:14:56 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-09-26 added more comments
* david  25/09/02  created
*/


#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baci.h>
#include <acscomponentImpl.h>
#include "EventComponentS.h"
#include "acsncSimpleSupplier.h"

 using namespace baci;

/**
 * This class is for testing the notification channel under an activator
 */
class SupplierCompImpl: public virtual acscomponent::ACSComponentImpl,
	      public virtual POA_demo::SupplierComp
{
  public:
    /* ----------------------------------------------------------------*/
    /**
     * Constructor
     * @param poa Poa which will activate this and also all other COBs. 
     * @param name DO's name. This is also the name that will be used to find the
     * configuration data for the DO in the Configuration Database.
     */
    SupplierCompImpl(const ACE_CString& name,
		     maci::ContainerServices *);
    
    /**
     * Destructor
     */
    virtual ~SupplierCompImpl();
    

    /* --------------------- [ CORBA interface ] ----------------------*/
    virtual void 
    sendEvents(short numEvents);
    
  private:
    nc::SimpleSupplier *m_testSupplier_p;
};

#endif
