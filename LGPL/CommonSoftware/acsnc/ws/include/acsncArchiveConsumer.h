#ifndef acsnc_archive_consumer_H
#define acsnc_archive_consumer_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) UNSPECIFIED - FILL IN, 2005 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: acsncArchiveConsumer.h,v 1.11 2009/08/07 19:57:38 javarias Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-04-12  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "acsncConsumer.h"
#include <string>

#include <lokiSmartPtr.h>
#include <loggingBaseLog.h>

namespace nc {
    //------------------------------------------------------------------
    class ArchiveConsumer : public Consumer
    {
      public:
	
	/**
	 * Pure abstract class which defines a receive method to be overridden
	 * by developers.
	 */
	class ArchiveHandler {
	    
	  public:
	    
	    virtual void
	    receive(ACS::Time          timeStamp, 
		    const std::string& device,
		    const std::string& parameter,
		    const CORBA::Any& value) = 0;

            virtual ~ArchiveHandler(){}
	};
	
	///Archive handler smart pointer
	typedef Loki::SmartPtr<ArchiveHandler, 
			       Logging::RefCounted, 
			       Loki::AllowConversion,
			       Loki::NoCheck,
			       Loki::DefaultSPStorage> ArchiveHandlerSmartPtr;
	
	
	//--------------------------------------------------------------
	/** 
	 * Constructor to be used within components.
	 * Consumer will use the Container to get a reference to the Naming
	 * Service.  If a valid reference to the container cannot be obtained, Consumer 
	 * will default to creating it's own ORB (assuming Consumer is run on the same
	 * host as the Naming Service).
	 * @param handler A (static) method which will be invoked each time an
	 *                archive event is received.
	 */
	ArchiveConsumer(ArchiveHandlerSmartPtr handler);
	
	/** 
	 * Constructor to be used within SimpleClient's.
	 * This constructor is provided for API users who create their own ORB that has 
	 * a reference to the Naming Service.
	 * @param orb_mp ORB that <b>has</b> a valid reference to the Naming Service.
	 * @param handler A (static) method which will be invoked each time an
	 *                archive event is received.
	 */
	ArchiveConsumer(CORBA::ORB_ptr orb_p,
			ArchiveHandlerSmartPtr handler);
	
	/** 
	 * Optional constructor - used outside of ACS.
	 * This constructor is very resource intensive (it spawns it's own ORB) and should 
	 * only to be utilized when there is a reason not to use Manager to get at the 
	 * Naming Service.  If argc==0, default parameters (i.e., environment variables) 
	 * specify how to get to the Naming Service. Otherwise, it is assumed argv has
	 * a valid corbaloc to the Naming Service.
	 * @param argc Number of ORB parameters in argv or 0
	 * @param argv ORB params.  Typically something like:<br>
	 *   orbArg[0] = ""<br>.
	 * orbArg[1] = "-ORBInitRef NameService=corbaloc::host:xxxx/NameService"<br>
	 * orbArg[2] = "-ORBDottedDecimalAddresses=1"<br>
	 * @param handler A (static) method which will be invoked each time an
	 *                archive event is received.
	 * @htmlonly
	 Sample Usage:<br>
	 <ul>
	 <li><b><i>Consumer("fridge",0,(char **)0);</b></i> - Generates ORB arguments on the fly from environment variables. 
	 <li><b><i>Consumer("fridge",argc,argv);</b></i> - Uses passed ORB arguments.
	 </ul>
	 @endhtmlonly
	*/
	ArchiveConsumer(int argc, 
			char *argv[],
			ArchiveHandlerSmartPtr handler);
	/** 
	 * Overridden 
	 *  @param publishedEvent The real CORBA event (defined via an IDL definition).
	 *         This structure has little to do with so-called ICD events.
     *  @throw CosEventComm::Disconnected
	 *
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	virtual void 
	push_structured_event(const CosNotification::StructuredEvent &publishedEvent);
	
      protected:
	//--------------------------------------------------------------
	/**
	 * Overridden
	 * @return 
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	const char* 
	getChannelKind();
	
	/**
	 * Overridden
	 * @return 
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	const char*
	getChannelDomain();


	/**
	 * This method returns a constant character pointer to the name of 
	 * the notification service as registered with the CORBA Naming Service.
	 * @return pointer to a constant string. Normally acscommon::NOTIFICATION_FACTORY_NAME
	 */
	const char*
	getNotificationFactoryName()
	    {return acscommon::ARCHIVE_NOTIFICATION_FACTORY_NAME;}

      private:
	//--------------------------------------------------------------
	/**
	 * Method used to subscribe to all types of events on the channel.
	 * @return 
	 * @throw ACSErrTypeCommon::CORBAProblemEx
     *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	 */
	void 
	subscribeAllEvents();

	/**
	 * This function does something with archive events.
	 */
	ArchiveHandlerSmartPtr handler_m;
    };
    //------------------------------------------------------------------
};



#endif /*!acsnc_archive_consumer_H*/
