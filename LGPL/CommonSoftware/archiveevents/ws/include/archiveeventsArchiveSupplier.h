#ifndef archive_event_supplier_H
#define archive_event_supplier_H
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
 * "@(#) $Id: archiveeventsArchiveSupplier.h,v 1.10 2012/01/21 22:48:11 tstaig Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * msekoran  2001-06-17  created
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

/** @file archiveeventsArchiveSupplier.h
 * Provides class declarations for sending events to the ALMA archive.
 */

#include <lokiSingleton.h>
#include <baciC.h>

#include <basencSupplier.h>
#include <ACSErrTypeCORBA.h>
#include <acsncErrType.h>
#include <archiveeventsExport.h>

/**
 * Class designed to send structured events out which will eventually
 * be stored in the ALMA archive. It's important to note a couple of issues
 * about this class:
 * 1. It should not be used directly. Use ArchiveSupplierSingleton instead.
 * 2. Until the init method is invoked, all methods should be considered
 *    unusable.
 */
class ArchiveSupplier : public BaseSupplier
{
  public:
    /**
     * Standard constructor.
     */
    ArchiveSupplier();

    /**
     * Destructor
     */
    virtual ~ArchiveSupplier();

    /**
     * Sends data to the archive.
     * @param priority  Priority of the event. A higher number is
     * equivalent to a greater priority.
     * @param timeStamp Timestamp
     * @param component Name of the component sending this value
     * @param property  Name of the property sending this value
     * @param value     Value which must be some CORBA type.
     * @param container Name of the container. Optional and there's
     * a solid chance this param will be removed entirely in the 
     * future.
	  * @throw acsncErrType::PublishEventFailureExImpl
	  * 		  Cannot publish the event in the Notification Channel
     */
    template <class T> 
    void 
    sendEvent(CORBA::Short priority,
	      ACS::Time timeStamp,
	      const std::string& component,
	      const std::string& property,
	      T value,
	      const std::string& container="")
	{
	    CORBA::Any any;
	    any <<= value;

		 try{
			 this->send_event(priority,
					 timeStamp,
					 component,
					 property,
					 any,
					 container);
		 }
		 catch(ACSErrTypeCORBA::CORBAReferenceNilExImpl& ex1)
		 {
			 acsncErrType::PublishEventFailureExImpl
				 ex2 (__FILE__, __LINE__, "ArchiveSupplier::send_event");
			 ex2.setEventName("archiving_event");
			 ex2.setChannelName(channelName_mp);
			 throw ex2;
		 }
		 catch(ACSErrTypeCORBA::NarrowFailedExImpl& ex1)
		 {
			 acsncErrType::PublishEventFailureExImpl
				 ex2 (__FILE__, __LINE__, "ArchiveSupplier::send_event");
			 ex2.setEventName("archiving_event");
			 ex2.setChannelName(channelName_mp);
			 throw ex2;
		 }
		 catch(ACSErrTypeCORBA::FailedToResolveServiceExImpl& ex1)
		 {
			 acsncErrType::PublishEventFailureExImpl
				 ex2 (__FILE__, __LINE__, "ArchiveSupplier::send_event");
			 ex2.setEventName("archiving_event");
			 ex2.setChannelName(channelName_mp);
			 throw ex2;
		 }
	}
    
    /**
     * Sends data to the archive.
     * @param priority  Priority of the event. A higher number is
     * equivalent to a greater priority.
     * @param timeStamp Timestamp in ACS format
     * @param component Name of the component sending this value
     * @param property  Name of the property sending this value
     * @param value     Value in CORBA any format
     * @param container Name of the container. There's
     * a solid chance this param will be removed entirely in the 
     * future.
	  * @throw acsncErrType::PublishEventFailureExImpl
	  * 		  Cannot publish the event in the Notification Channel
     */
    void 
    send_event(CORBA::Short priority,
	       ACS::Time timeStamp,
	       const std::string& component,
	       const std::string& property,
	       CORBA::Any value,
	       const std::string& container);

  protected:
    
    /**
     * Overridden.
     */
    virtual const char*
    getNotificationFactoryName()
	{return acscommon::ARCHIVE_NOTIFICATION_FACTORY_NAME;}

    /**
     * Overridden.
     */
    virtual const char*
    getChannelDomain() 
	{return acscommon::ALMADOMAIN;}

    /**
     * Overridden.
     */
    virtual const char* 
    getChannelKind() 
	{ return acscommon::NC_KIND; } // see http://ictjira.alma.cl/browse/ICT-494

    /**
     * Overridden.
     */
    virtual const char*
    getEventType()
	{ return ""; }
    
};

/**
 * Typedef defining a singleton ArchiveSupplier object. Users must use this
 * instead of creating instances of ArchiveSupplier on their own!.
 */
#ifdef __CYGWIN__
template class ARCHIVEEVENTS_Export Loki::SingletonHolder<ArchiveSupplier,
               Loki::CreateUsingNew,
               Loki::PhoenixSingleton,
               Loki::SingleThreaded>;
#endif
typedef Loki::SingletonHolder<ArchiveSupplier, 
			      Loki::CreateUsingNew, 
			      Loki::PhoenixSingleton, 
			      Loki::SingleThreaded> ArchiveSupplierSingleton;

#endif
