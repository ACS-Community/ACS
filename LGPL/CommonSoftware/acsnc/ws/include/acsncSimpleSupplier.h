#ifndef SIMPLE_SUPPLIER_H
#define SIMPLE_SUPPLIER_H
/*    @(#) $Id: acsncSimpleSupplier.h,v 1.19 2009/09/24 23:08:03 javarias Exp $
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
 */

/** @file acsncSimpleSupplier.h
 *  Header file for the Supplier-derived class that should be used from within
 *  components.
 */

#include "acsncSupplier.h"
namespace nc {
/**
 *  SimpleSupplier is used to publish data onto a notification channel
 *  defined by the string passed to SimpleSupplier's constructor.
 *  To publish data onto the channel, simply invoke the publishData method on
 *  the templated type. This particular class blocks until the event is received
 *  on manager's host and supports just about any number of event types for the 
 *  channel it is connected to.
 *
 *  TODO:
 *  - 
 */
class SimpleSupplier : public Supplier
{
  public:
	  /**
		 * Event procesing callback interface definition
		*/
	  template<class T>
		  class EventProcessingCallback{
			  public:
				  virtual void eventDropped(T event){}
				  virtual void eventSent(T event){}
				  virtual void eventStoredInQueue(T event){}
				  virtual ~EventProcessingCallback(){}
		  };
    ///////////////////////////////////////////////////////////////
    /**
     * Constructor. All the work is done in the superclass's constructor.
     * @param channlName The name of the channel events will be published to.
     * @param component A reference to a component is needed for the Event 
     * Description (which is normally hidden from Consumers).
     */
    SimpleSupplier(const char* channelName, acscomponent::ACSComponentImpl* component);
    ///////////////////////////////////////////////////////////////
    /**
     * publishData is the templated method that actually sends the event
     * to the channel. The templated parameter is the ICD event (i.e., IDL
     * struct) to be sent. Really this is the only Supplier method developers 
     * should be invoking from their code.
     * @param data The templated data structure to be published.
	  * @param evProcCallback Pointer to the callback to handle event's fate, by
	  * default is null
     * @throw ACSErrTypeCommon::CORBAProblemEx
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    template <class T> void 
    publishData(T data, EventProcessingCallback<T> *evProcCallback=NULL);

    ///////////////////////////////////////////////////////////////
  protected:
    /**
     * Destructor is protected.
     */
    virtual ~SimpleSupplier();

    /**
     * CORBA Any taken out of the publishData method for a small 
     * performance increase.
     */
    CORBA::Any any_m;

    ///////////////////////////////////////////////////////////////
  private:
    /**
     * ALMA C++ coding standards state copy operators should be disabled.
     */
    void operator=(const SimpleSupplier&);
};
 }; 


#include "acsncSimpleSupplier.i"

#endif
