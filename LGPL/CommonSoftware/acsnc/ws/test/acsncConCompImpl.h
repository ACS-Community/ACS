#ifndef _acsnc_con_comp_impl_h_
#define _acsnc_con_comp_impl_h_
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
* "@(#) $Id: acsncConCompImpl.h,v 1.8 2006/09/01 02:20:54 cparedes Exp $"
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
#include "acsncSimpleConsumer.h"

 using namespace baci;

/**
 * This class is for testing the notification channel under an activator
 */
class ConCompImpl: public virtual acscomponent::ACSComponentImpl,
			public virtual POA_demo::ConComp
{
  public:
    /* ----------------------------------------------------------------*/
    /**
     * Constructor
     * @param poa Poa which will activate this and also all other COBs. 
     * @param name DO's name. This is also the name that will be used to find the
     * configuration data for the DO in the Configuration Database.
     */
    ConCompImpl(const ACE_CString& name,
		     maci::ContainerServices *);
    
    /**
     * Destructor
     */
    virtual ~ConCompImpl();

    virtual void execTest(const char* channelName,CORBA::Boolean autoreconnect);
    virtual void execTestResumeSuspend(const char* channelName);
    virtual void checkCounterGreaterThan(CORBA::Long value);
    virtual void checkCounterLowerThan(CORBA::Long value);

  private:
    //
    int m_count;

    //
    static void myHandlerFunction(acsnc::EventDescription joe, void *handlerParam);

    //
    nc::SimpleConsumer<acsnc::EventDescription> *m_testCon_p;
};

#endif
