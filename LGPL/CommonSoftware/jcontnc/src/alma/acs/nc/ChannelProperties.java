/**
 * ALMA - Atacama Large Millimiter Array Copyright (c) ESO - European Southern Observatory, 2005
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */

/**
 * @author dfugate
 * @version $Id: ChannelProperties.java,v 1.15 2012/02/10 12:54:39 hsommer Exp $
 * @since
 */

/**
 * ChannelProperties is a class which discovers the the quality of service and
 * administrative properties that should be used with any particular channel
 * name.
 */

package alma.acs.nc;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.CORBA.Any;
import org.omg.CosNotification.AnyOrder;
import org.omg.CosNotification.BestEffort;
import org.omg.CosNotification.ConnectionReliability;
import org.omg.CosNotification.DeadlineOrder;
import org.omg.CosNotification.DiscardPolicy;
import org.omg.CosNotification.EventReliability;
import org.omg.CosNotification.FifoOrder;
import org.omg.CosNotification.LifoOrder;
import org.omg.CosNotification.MaxConsumers;
import org.omg.CosNotification.MaxEventsPerConsumer;
import org.omg.CosNotification.MaxQueueLength;
import org.omg.CosNotification.MaxSuppliers;
import org.omg.CosNotification.OrderPolicy;
import org.omg.CosNotification.Persistent;
import org.omg.CosNotification.Priority;
import org.omg.CosNotification.PriorityOrder;
import org.omg.CosNotification.Property;
import org.omg.CosNotification.RejectNewEvents;
import org.omg.CosNotification.StartTimeSupported;
import org.omg.CosNotification.StopTimeSupported;
import org.omg.CosNotification.Timeout;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.cdbErrType.CDBFieldDoesNotExistEx;
import alma.cdbErrType.WrongCDBDataTypeEx;

import com.cosylab.CDB.DAO;

/**
 * ChannelProperties is a class designed to retrieve the various quality of
 * service and administrative properties for a given channel. It is intended to
 * be used primarily by the Helper class within this package.
 *  * <p>
 * @TODO think about joining this class with {@link ChannelInfo}.
 * <p>
 * @TODO: Cache CDB data and only re-read when CDB calls back to us via DALChangeListener mechanism.
 * 
 * @author dfugate
 */
public class ChannelProperties {
	/**
	 * Creates a new instance of ChannelProperties.
	 * 
	 * @param services
	 *           A reference to the ContainerServices. Used to retrieve other
	 *           CORBA references and to access the logger.
	 */
	public ChannelProperties(ContainerServicesBase services) {
		// save a local reference to the container services
		m_services = services;

		// immediately grab a logger
		m_logger = m_services.getLogger();
	}

	// //////////////////////////////////////////////////////////////////////////
	/**
	 * Simple function which returns true if the given channel has an entry in
	 * $ACS_CDB/CDB/MACI/EventChannels/ section of the ACS configuration
	 * database.
	 * 
	 * @param channelName
	 *           is a string referring to the name of a Notification Channel
	 *           registered in the CORBA Naming Service
	 * @return true if
	 *         $ACS_CDB/CDB/MACI/EventChannels/channel_name/channel_name.xml
	 *         exists and is a valid XML. false otherwise.
	 */
	public boolean cdbChannelConfigExists(String channelName) {
		try {
			m_services.getCDB().get_DAO("MACI/Channels/" + channelName);
			return true;
		} catch (Exception e) {
			m_logger.finer("No CDB entry found for '" + channelName + "' channel");
			return false;
		}
	}

	// -----------------------------------------------------------
	
	/**
	 * When some attribute is set within the CDB (currently <code>IntegrationLogs</code> 
	 * defined in <code>EventChannel.xsd</code>, see NC document), a log is published 
	 * each time an event is sent or received. 
	 * For performance reasons, this should be used very carefully to say the least.
	 * 
	 * @param channelName
	 *           Name of the channel.
	 * @return True if event publishing/receiving should be traced by log messages.
	 * @throws AcsJException
	 * @since ACS 6.0.1 (formerly called "getIntegrationLogs")
	 */
	public boolean isTraceEventsEnabled(String channelName) throws AcsJException {
		// use this object to get at channel information from the CDB
		DAO tempDAO = null;
		try {
			tempDAO = m_services.getCDB().get_DAO_Servant("MACI/Channels/" + channelName);
		} catch (alma.cdbErrType.CDBRecordDoesNotExistEx e) {
			return false;
		} catch (alma.cdbErrType.CDBXMLErrorEx e) {
			m_logger.log(Level.SEVERE, "Bad CDB entry found for '" + channelName + "' channel", e);
			throw new alma.ACSErrTypeCommon.wrappers.AcsJUnknownEx(e);
		} catch (AcsJContainerServicesEx e) {
			m_logger.log(Level.SEVERE, "CDB unavailable", e);
			throw new alma.ACSErrTypeCommon.wrappers.AcsJNoResourcesEx(e);
		}

		try {
			if (tempDAO.get_string("IntegrationLogs").equals("false")) {
				return false;
			}
			return true;
		} catch (WrongCDBDataTypeEx e) {
			m_logger.log(Level.SEVERE, "Wrong type of data for IntegrationLogs", e);
			throw new alma.ACSErrTypeCommon.wrappers.AcsJNoResourcesEx(e);
		} catch (CDBFieldDoesNotExistEx e) {
			m_logger.log(Level.SEVERE, "Field does not exist for IntegrationLogs", e);
			throw new alma.ACSErrTypeCommon.wrappers.AcsJNoResourcesEx(e);
		}
	}

	// //////////////////////////////////////////////////////////////////////////
	/**
	 * Given a channel name that exists in the ACS CDB
	 * ($ACS_CDB/CDB/MACI/Channels/channelName/channelName.xml), this function
	 * returns the channels administrative properties in their CORBA format.
	 * 
	 * @param channelName
	 *           name of the channel found in $ACS_CDB/CDB/MACI/Channels
	 * @return channel's admin properties
	 * @throws AcsJException
	 *            if the channel's CDB entry is corrupted in any way
	 */
	public Property[] getCDBAdminProps(String channelName) throws AcsJException {
		// use this object to get at channel information from the CDB
		DAO tempDAO = null;
		try {
			tempDAO = m_services.getCDB().get_DAO_Servant("MACI/Channels/" + channelName);
		} catch (alma.cdbErrType.CDBXMLErrorEx e) {
			m_logger.log(Level.WARNING, "Bad CDB entry found for '" + channelName + "' channel");
			throw new alma.ACSErrTypeCommon.wrappers.AcsJUnknownEx(e);
		} catch (alma.cdbErrType.CDBRecordDoesNotExistEx e) {
			m_logger.log(Level.WARNING, "No CDB entry found for '" + channelName + "' channel");
			throw new alma.ACSErrTypeCommon.wrappers.AcsJFileNotFoundEx(e);
		} catch (AcsJContainerServicesEx e) {
			m_logger.log(Level.WARNING, "CDB unavailable");
			throw new alma.ACSErrTypeCommon.wrappers.AcsJNoResourcesEx(e);
		}

		// MaxQueueLength - TAO 1.1 had a queque of ~5 events. TAO 1.3 allows an
		// infinite amount of events to be stored. 20 seems like
		// a reasonable default.
		Any maxQLAny = m_services.getAdvancedContainerServices().getAny();
		try {
			maxQLAny.insert_long(tempDAO.get_long("MaxQueueLength"));
		} catch (Exception e) {
			m_logger.log(Level.SEVERE, "Bad CDB entry datatype found for '" + channelName + "' channel", e);
			throw new alma.ACSErrTypeCommon.wrappers.AcsJTypeNotSupportedEx(e);
		}
		Property maxQL = new Property(MaxQueueLength.value, maxQLAny);

		// MaxConsumers - ///////////////////////////////////////////////////////
		Any maxConsumersAny = m_services.getAdvancedContainerServices().getAny();
		try {
			maxConsumersAny.insert_long(tempDAO.get_long("MaxConsumers"));
		} catch (Exception e) {
			m_logger.log(Level.SEVERE, "Bad CDB entry datatype found for '" + channelName + "' channel", e);
			throw new alma.ACSErrTypeCommon.wrappers.AcsJTypeNotSupportedEx(e);
		}
		Property maxConsumers = new Property(MaxConsumers.value, maxConsumersAny);

		// MaxSuppliers - ///////////////////////////////////////////////////////
		Any maxSuppliersAny = m_services.getAdvancedContainerServices().getAny();
		try {
			maxSuppliersAny.insert_long(tempDAO.get_long("MaxSuppliers"));
		} catch (Exception e) {
			m_logger.log(Level.SEVERE, "Bad CDB entry datatype found for '" + channelName + "' channel", e);
			throw new alma.ACSErrTypeCommon.wrappers.AcsJTypeNotSupportedEx(e);
		}
		Property maxSuppliers = new Property(MaxSuppliers.value, maxSuppliersAny);

		// RejectNewEvents - if the queque is full, suppliers get exceptions when
		// trying to push events onto the channel.
		Any rejectNEAny = m_services.getAdvancedContainerServices().getAny();
		boolean tBool = true;
		try {
			if (tempDAO.get_string("RejectNewEvents").equals("false")) {
				tBool = false;
			}
		} catch (Exception e) {
			m_logger.log(Level.SEVERE, "Bad CDB entry datatype found for '" + channelName + "' channel", e);
			throw new alma.ACSErrTypeCommon.wrappers.AcsJTypeNotSupportedEx(e);
		}
		rejectNEAny.insert_boolean(tBool);
		Property rejectNE = new Property(RejectNewEvents.value, rejectNEAny);

		Property[] adminProps = { maxQL, maxConsumers, maxSuppliers, rejectNE };
		return adminProps;
	}

	// //////////////////////////////////////////////////////////////////////////
	/**
	 * Given a channel name that exists in the ACS CDB
	 * ($ACS_CDB/CDB/MACI/Channels/channelName/channelName.xml), this function
	 * returns the channel's quality of service properties in their CORBA format.
	 * <p>
	 * The schema for this channel configuration is <code>urn:schemas-cosylab-com:EventChannel:1.0</code>.
	 * 
	 * @param channelName
	 *           name of the channel found in $ACS_CDB/CDB/MACI/Channels
	 * @return channel's quality of service properties
	 * @throws AcsJException
	 *            if the channel's CDB entry is corrupted in any way
	 */
	public Property[] getCDBQoSProps(String channelName) throws alma.acs.exceptions.AcsJException {
		// use this object to get at channel information from the CDB
		DAO tempDAO = null;
		try {
			tempDAO = m_services.getCDB().get_DAO_Servant("MACI/Channels/" + channelName);
		} catch (alma.cdbErrType.CDBXMLErrorEx e) {
			m_logger.log(Level.WARNING, "Bad CDB entry found for '" + channelName + "' channel");
			throw new alma.ACSErrTypeCommon.wrappers.AcsJUnknownEx(e);
		} catch (alma.cdbErrType.CDBRecordDoesNotExistEx e) {
			m_logger.log(Level.WARNING, "No CDB entry found for '" + channelName + "' channel");
			throw new alma.ACSErrTypeCommon.wrappers.AcsJFileNotFoundEx(e);
		} catch (AcsJContainerServicesEx e) {
			m_logger.log(Level.WARNING, "CDB unavailable", e);
			throw new alma.ACSErrTypeCommon.wrappers.AcsJNoResourcesEx(e);
		}

		// EventReliability - ///////////////////////////////////////////////
		Any eventRelAny = m_services.getAdvancedContainerServices().getAny();
		short eventRelVal = Persistent.value;
		try {
			if (tempDAO.get_string(EventReliability.value).equals("BestEffort")) {
				eventRelVal = BestEffort.value;
			}
		} catch (Exception e) {
			m_logger.log(Level.SEVERE, "Bad CDB entry datatype found for '" + channelName + "' channel", e);
			throw new alma.ACSErrTypeCommon.wrappers.AcsJTypeNotSupportedEx(e);
		}
		eventRelAny.insert_short(eventRelVal);
		Property eventRel = new Property(EventReliability.value, eventRelAny);

		// ConnectionReliability - ///////////////////////////////////////////////
		Any connectRelAny = m_services.getAdvancedContainerServices().getAny();
		short connectRelVal = Persistent.value;
		try {
			if (tempDAO.get_string(ConnectionReliability.value).equals("BestEffort")) {
				connectRelVal = BestEffort.value;
			}
		} catch (Exception e) {
			m_logger.log(Level.SEVERE, "Bad CDB entry datatype found for '" + channelName + "' channel", e);
			throw new alma.ACSErrTypeCommon.wrappers.AcsJTypeNotSupportedEx(e);
		}

		connectRelAny.insert_short(Persistent.value);
		Property connectRel = new Property(ConnectionReliability.value, connectRelAny);
		//@TODO do something with this connectRel, e.g. check why it's commented out at the end of this method!
		
		// Priority - ///////////////////////////////////////////////
		Any priorityAny = m_services.getAdvancedContainerServices().getAny();
		try {
			priorityAny.insert_short((short) tempDAO.get_long(Priority.value));
		} catch (Exception e) {
			m_logger.log(Level.SEVERE, "Bad CDB entry datatype found for '" + channelName + "' channel", e);
			throw new alma.ACSErrTypeCommon.wrappers.AcsJTypeNotSupportedEx(e);
		}
		Property priority = new Property(Priority.value, priorityAny);

		// Timeout - ///////////////////////////////////////////////
		Any timeoutAny = m_services.getAdvancedContainerServices().getAny();
		try {
			org.omg.TimeBase.TimeTHelper.insert(timeoutAny, tempDAO.get_long(Timeout.value));
		} catch (Exception e) {
			m_logger.log(Level.SEVERE, "Bad CDB entry datatype found for '" + channelName + "' channel", e);
			throw new alma.ACSErrTypeCommon.wrappers.AcsJTypeNotSupportedEx(e);
		}
		Property timeout = new Property(Timeout.value, timeoutAny);

		// OrderPolicy - ///////////////////////////////////////////////
		Any orderPolAny = m_services.getAdvancedContainerServices().getAny();
		short orderPolicyVal;

		try {
			if (tempDAO.get_string(OrderPolicy.value).equals("AnyOrder")) {
				orderPolicyVal = AnyOrder.value;
			} else if (tempDAO.get_string(OrderPolicy.value).equals("FifoOrder")) {
				orderPolicyVal = FifoOrder.value;
			} else if (tempDAO.get_string(OrderPolicy.value).equals("PriorityOrder")) {
				orderPolicyVal = PriorityOrder.value;
			} else if (tempDAO.get_string(OrderPolicy.value).equals("DeadlineOrder")) {
				orderPolicyVal = DeadlineOrder.value;
			} else {
				m_logger.log(Level.SEVERE, "Bad CDB entry datatype found for '" + channelName + "' channel");
				throw new Exception("No value found for order policy.");
			}
		} catch (Exception e) {
			m_logger.log(Level.SEVERE, "Bad CDB entry datatype found for '" + channelName + "' channel", e);
			throw new alma.ACSErrTypeCommon.wrappers.AcsJTypeNotSupportedEx(e);
		}

		orderPolAny.insert_short(orderPolicyVal);

		Property orderPol = new Property(OrderPolicy.value, orderPolAny);

		// DiscardPolicy - ///////////////////////////////////////////////
		Any discardPolAny = m_services.getAdvancedContainerServices().getAny();
		short discardPolicyVal;

		try {
			if (tempDAO.get_string(DiscardPolicy.value).equals("AnyOrder")) {
				discardPolicyVal = AnyOrder.value;
			} else if (tempDAO.get_string(DiscardPolicy.value).equals("FifoOrder")) {
				discardPolicyVal = FifoOrder.value;
			} else if (tempDAO.get_string(DiscardPolicy.value).equals("PriorityOrder")) {
				discardPolicyVal = PriorityOrder.value;
			} else if (tempDAO.get_string(DiscardPolicy.value).equals("DeadlineOrder")) {
				discardPolicyVal = DeadlineOrder.value;
			} else if (tempDAO.get_string(DiscardPolicy.value).equals("LifoOrder")) {
				discardPolicyVal = LifoOrder.value;
			} else {
				m_logger.log(Level.SEVERE, "Bad CDB entry datatype found for '" + channelName + "' channel");
				throw new Exception("No value found for discard policy.");
			}
		} catch (Exception e) {
			m_logger.log(Level.SEVERE, "Bad CDB entry datatype found for '" + channelName + "' channel", e);
			throw new alma.ACSErrTypeCommon.wrappers.AcsJTypeNotSupportedEx(e);
		}

		discardPolAny.insert_short(discardPolicyVal);
		Property discardPol = new Property(DiscardPolicy.value, discardPolAny);

		// StartTimeSupported - ///////////////////////////////////////////////
		Any startTSAny = m_services.getAdvancedContainerServices().getAny();
		boolean startTSVal = true;
		try {
			if (tempDAO.get_string(StartTimeSupported.value).equals("false")) {
				startTSVal = false;
			}
		} catch (Exception e) {
			m_logger.log(Level.SEVERE, "Bad CDB entry datatype found for '" + channelName + "' channel", e);
			throw new alma.ACSErrTypeCommon.wrappers.AcsJTypeNotSupportedEx(e);
		}

		startTSAny.insert_boolean(startTSVal);
		Property startTS = new Property(StartTimeSupported.value, startTSAny);

		// StopTimeSupported - ///////////////////////////////////////////////
		Any stopTSAny = m_services.getAdvancedContainerServices().getAny();
		boolean stopTSVal = true;
		try {
			if (tempDAO.get_string(StopTimeSupported.value).equals("false")) {
				stopTSVal = false;
			}
		} catch (Exception e) {
			m_logger.log(Level.SEVERE, "Bad CDB entry datatype found for '" + channelName + "' channel", e);
			throw new alma.ACSErrTypeCommon.wrappers.AcsJTypeNotSupportedEx(e);
		}

		stopTSAny.insert_boolean(stopTSVal);
		Property stopTS = new Property(StopTimeSupported.value, stopTSAny);

		// MaxEventsPerConsumer - ///////////////////////////////////////////////
		Any MEPCAny = m_services.getAdvancedContainerServices().getAny();
		try {
			MEPCAny.insert_long(tempDAO.get_long(MaxEventsPerConsumer.value));
		} catch (Exception e) {
			m_logger.log(Level.SEVERE, "Bad CDB entry datatype found for '" + channelName + "' channel", e);
			throw new alma.ACSErrTypeCommon.wrappers.AcsJTypeNotSupportedEx(e);
		}
		Property MEPC = new Property(MaxEventsPerConsumer.value, MEPCAny);

		Property[] qosProps = { // eventRel,
		// connectRel,
				priority, timeout, orderPol, discardPol,
				// startTS,
				// stopTS,
				MEPC };
		return qosProps;

	}

	// //////////////////////////////////////////////////////////////////////////
	/**
	 * Override this method in a subclass to specify your own quality of service
	 * properties for the channel. Only useful if the channel has not already
	 * been registered with the naming service.
	 * 
	 * @param channelName
	 *           Name of the channel we are looking for.
	 * 
	 * @return A property array consisting of the quality of service
	 *         specifications.
	 * @throws AcsJException
	 *            Thrown if a CORBA Any cannot be created.
	 */
	protected Property[] configQofS(String channelName) throws AcsJException {

		// if there's a CDB entry for this channel...
		if (cdbChannelConfigExists(channelName) == true) {
			// ...return that CDB entry
			m_logger.finer("Using CDB for '" + channelName + "' channel's Q of S properties");
			return getCDBQoSProps(channelName);
		}

		Property[] initial_QOS = {};
		return initial_QOS;
	}

	// //////////////////////////////////////////////////////////////////////////
	/**
	 * Override this method in a subclass to specify your own administrative
	 * properties for the channel. Only useful if the channel has not already
	 * been registered with the naming service.
	 * 
	 * @param channelName
	 *           Name of the channel we are looking for
	 * 
	 * @return A property array consisting of the administrative specifications.
	 * @throws AcsJException
	 *            Thrown if a CORBA Any cannot be created.
	 */
	protected Property[] configAdminProps(String channelName) throws AcsJException {
		// if there's a CDB entry for this channel...
		if (cdbChannelConfigExists(channelName) == true) {
			// ...return that CDB entry
			m_logger.finer("Using CDB for '" + channelName + "' channel's admin properties");
			return getCDBAdminProps(channelName);
		}

		Property[] initialAdmin = {};
		return initialAdmin;
	}

	/**
	 * Access to the component's name along with the logging service.
	 */
	private final ContainerServicesBase m_services;

	/**
	 * Standard logger
	 */
	private final Logger m_logger;
}
