#ifndef _BULKDATANT_SENDER_IMPL_H
#define _BULKDATANT_SENDER_IMPL_H
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
 * "@(#)"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * oat       28/01/05  created 
 */

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#include <baci.h>
#include <baciCharacteristicComponentImpl.h>
#include <maciHelper.h>
#include <maciContainerServices.h>

#include <bulkDataSenderS.h>
#include <bulkDataReceiverC.h>

#include "bulkDataNTConfigurationParser.h"
#include "bulkDataNTSenderStream.h"


/** @file bulkDataSenderNTImpl.h
 */

/** @defgroup BULKDATASENDERIMPLDOC Bulk Data Sender Impl
 *  @{
 * @htmlonly
 <hr size="2" width="100%">
 <div align="left">
 <h2>Description</h2>
 bulkDataSenderImpl.h implements the BulkDataSender interface
 <br>
 <br>
 <h2>Links</h2>
 <ul>
 <li><a href="classBulkDataSenderImpl.html">Bulk Data Sender Impl Class Reference</a></li>
 </ul>
 </div>
 @endhtmlonly
 * @}
 */

// probably we do not need template
//template<class TSenderCallback=BulkDataSenderDefaultCallback>
class BulkDataNTSenderImpl : public baci::CharacteristicComponentImpl,
			   public virtual POA_bulkdata::BulkDataSender
{    
  public:
    /**
     * Constructor
     * @param poa poa which will activate this and also all other components 
     * @param name component name
     */
	BulkDataNTSenderImpl(const ACE_CString& name,maci::ContainerServices* containerServices);
  
    /**
     * Destructor
     */
    virtual ~BulkDataNTSenderImpl();
  
    virtual void initialize();

    virtual void cleanUp();

    /**
     *  Negotiate and initialize connection with the Sender object.
     *  @param receiver reference of the Receiver Component (currently unused).
     *  @throw ACSBulkDataError::AVConnectErrorEx
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void connect(bulkdata::BulkDataReceiver_ptr receiverObj_p);

    /**
     *  @throw ACSBulkDataError::AVDisconnectErrorEx
    */
    virtual void disconnect();

    /**
     *
     * @param name The stream name
     * @return The sender stream object for the given name
     * @throw StreamNotExistExImpl
     */
    virtual AcsBulkdata::BulkDataNTSenderStream *getSenderStream(const char *name);

    /**
     * Gets the first sender stream stored in the internal stream map.
     *
     * @return The first sender stream saved in the internal map
     * @deprecated You should use getSenderStream(const char *name) instead
     */
    virtual AcsBulkdata::BulkDataNTSenderStream *getSenderStream();

    /** 
     *  Calls the Receiver handle_start() method once the connection is established.
     *  @throw ACSBulkDataError::AVStartSendErrorEx
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void startSend()  =0;

    /**
     *  Sends data to the Receiver calling the receive_frame() method on the Receiver side.
     *  This method must be overriden by the user to send his own data.
     *  @param size buffer size of the sent data.
     *  @throw ACSBulkDataError::AVPaceDataErrorEx
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void paceData()  =0;

    /** 
     *  Calls the Receiver handle_stop() method.
     *  @throw ACSBulkDataError::AVStopSendErrorEx
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void stopSend() =0;



  protected:

    /**
     * Indicates if this component's alma/ branch exposes the new or the old configuration mechanism
     * @return Whether this component is configured using the new or the old configuration mechanism
     */
    virtual bool usesOldConfigurationMechanism();

 
  private:

    // The XML parser object, responsible of retrieving the configuration objects for a given XML document
    AcsBulkdata::BulkDataConfigurationParser *parser_m;

    // Used to store the number of flows to create, as specified with the old config mechanism
    int defaultFlowsCount_m;

    // map<name, stream>
    typedef std::map<std::string, AcsBulkdata::BulkDataNTSenderStream *> StreamMap;

    // Map that stores the stream objects that are actually created
    StreamMap senderStreams_m;

    /**
     * Opens all senders as specified on the CDB (old or new configuration mechanism).
     * This method might be added in the future to the IDL interface
     */
    virtual void openSenders();

    // Creates a new sender stream, given a name
    AcsBulkdata::BulkDataNTSenderStream* createSenderStream(const char *stream_name);

    // Create a new default sender stream
    AcsBulkdata::BulkDataNTSenderStream* createDefaultSenderStream();

    // Close a stream, given an iterator of the stream map
    void closeStream(StreamMap::iterator &it);

};
/*
typedef BulkDataSenderImpl<> BulkDataSenderDefaultImpl;

#include "bulkDataSenderImpl.i"
*/
#endif /* _BULKDATANT_SENDER_IMPL_H */
