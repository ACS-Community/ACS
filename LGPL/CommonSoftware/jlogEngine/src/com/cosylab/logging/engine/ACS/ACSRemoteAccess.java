/*
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
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package com.cosylab.logging.engine.ACS;

import java.util.logging.Level;

import org.jacorb.orb.acs.AcsORBProfiler;
import org.jacorb.orb.acs.AcsProfilingORB;
import org.omg.CORBA.ORB;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNotifyChannelAdmin.InterFilterGroupOperator;
import org.omg.CosNotifyFilter.ConstraintExp;
import org.omg.CosNotifyFilter.ConstraintInfo;
import org.omg.CosNotifyFilter.Filter;
import org.omg.CosNotifyFilter.FilterFactory;
import org.omg.CosNotifyFilter.InvalidConstraint;
import org.omg.CosNotifyFilter.InvalidGrammar;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.PortableServer.POAManager;

import com.cosylab.logging.engine.FiltersVector;
import com.cosylab.logging.engine.RemoteAccess;

import gov.sandia.NotifyMonitoringExt.ConsumerAdmin;
import gov.sandia.NotifyMonitoringExt.ConsumerAdminHelper;
import gov.sandia.NotifyMonitoringExt.EventChannel;
import gov.sandia.NotifyMonitoringExt.EventChannelHelper;

import si.ijs.maci.Manager;

import alma.acs.logging.AcsLogger;
import alma.acs.profiling.orb.AcsORBProfilerImplBase;
import alma.acscommon.ACS_NC_DOMAIN_LOGGING;
import alma.acscommon.LOGGING_CHANNEL_NAME;
import alma.acscommon.LOGGING_CHANNEL_XML_NAME;
import alma.acscommon.NAMESERVICE_BINDING_NC_DOMAIN_SEPARATOR;
import alma.acscommon.NAMING_SERVICE_NAME;
import alma.maciErrType.CannotGetComponentEx;
import alma.maciErrType.ComponentConfigurationNotFoundEx;
import alma.maciErrType.ComponentNotAlreadyActivatedEx;
import alma.maciErrType.NoPermissionEx;

/**
 * This class implements methods for declaring the naming service 
 * and the notification channel to which the client is subscribed.
 * 
 * Creation date: (10/30/2001 3:48:32 PM)
 * @author: 
 */
public final class ACSRemoteAccess implements RemoteAccess {
	public static final String MANAGER_PROPERTY = System.getProperty("ACS.manager");
	
	public static final String NAME_SERVICE = NAMING_SERVICE_NAME.value;
	public static final String LOGGING_XML_CHANNEL = LOGGING_CHANNEL_XML_NAME.value;
	public static final String LOGGING_BIN_CHANNEL = LOGGING_CHANNEL_NAME.value;
	
	private boolean isInitialized = false;
	private ORB orb;
    private boolean isExternalORB;
	private EventChannel eventChannel = null;
	private ConsumerAdmin consumerAdmin = null;
	private ACSStructuredPushConsumer acsSPS = null;
	
	// The object to dispatch messages to the listeners
	private ACSListenersDispatcher listenersDispatcher = null;
	
	// The engine filters 
	private FiltersVector filters = null;
	
	// The object to send new logs to
	private ACSLogRetrieval logRetrieval;
	
	private final AcsLogger logger;
	
	/**
	 * ACSRemoteAccss constructor comment.
	 * 
	 * @param listeners The object to send messages to the listeners
	 */
	public ACSRemoteAccess(ACSListenersDispatcher listeners, ACSLogRetrieval retrieval, AcsLogger logger) {
		if (listeners==null) {
			throw new IllegalArgumentException("The object to dispatch messages to listeners can't be null");
		}
		if (retrieval==null) {
			throw new IllegalArgumentException("The ACSLogRetrieval can't be null");
		}
		if (logger==null) {
			throw new IllegalArgumentException("The logger can't be null");
		}
		listenersDispatcher=listeners;
		logRetrieval=retrieval;
		this.logger = logger;
	}
	
	/**
	 * This method can not be used in the current implementation of ACS logging
	 * But it will be useful for archiving.
	 */
	private ConstraintExp[] createConstraints() {
		final int numConstraints = 2;
		String[] constraintStrings = {
			"$upper == 'A'",
			"$lower == 'a'"
		};
		// ... and so on
		// Orbacus Notify book, page 64 (programming example).
		return null;
	}
	
	/**
	 * Creates a consumer admin on the server, that is used only by this jlog instance.
	 * @see #consumerAdmin
	 */
	private boolean createConsumerAdmin() {
		listenersDispatcher.publishReport("Creating Consumer Admin...");
		try {
			InterFilterGroupOperator ifgo = org.omg.CosNotifyChannelAdmin.InterFilterGroupOperator.OR_OP;
			org.omg.CORBA.IntHolder adminID = new org.omg.CORBA.IntHolder();
			
			consumerAdmin = ConsumerAdminHelper.narrow(
					eventChannel.new_for_consumers(ifgo, adminID)
				);
		} catch (Exception e) {
			listenersDispatcher.publishReport("Exception occurred when creating Consumer Admin.");
			System.out.println("Exception in ACSRemoteAccess::createConsumerAdmin(): " + e);
			return false;
		}
		listenersDispatcher.publishReport("Consumer Admin created.");
		return true;
	}
	
	/**
	 * This method can not be used in the current implementation of ACS logging
	 * But it will be useful for archiving.
	 */
	private void createFilter() {
		// create filter
		if (eventChannel == null) return;
		FilterFactory filterFactory = eventChannel.default_filter_factory();
	
		Filter filter = null;
		try {
			filter = filterFactory.create_filter("EXTENDED_TCL");
		}
		catch (InvalidGrammar e) {
			System.out.println("Invalid grammar in ACSRemoteAccess::createFilter(): " + e);
		}
		
		// create constaints
		try {
			ConstraintInfo[] info = filter.add_constraints(createConstraints());
		} catch (InvalidConstraint e) {
			System.out.println("Invalid constraint in ACSRemoteAccess::createFilter(): " + e);
		}
	
		// add constraints to filter
	}
	
	private boolean createStructuredPushConsumer() {
		listenersDispatcher.publishReport("Initializing Structured Push Consumer...");
		acsSPS = new ACSStructuredPushConsumer(
				this,
				listenersDispatcher,
				logRetrieval);
		if (!acsSPS.isInitialized) return false;
		
		acsSPS.connect();
		if (!acsSPS.isConnected) return false;
		
		acsSPS.setupEvents();
		if (!acsSPS.isEventSetup) return false;
		
		listenersDispatcher.publishReport("Structured Push Consumer initialized.");
		return true;
	}
	
	/**
	 * destroy method comment. Not implemented yet.
	 */
	public synchronized void destroy() {
		if (orb==null) {
			return;
		}
		if (acsSPS!=null) {
			acsSPS.destroy();
		}
	//	consumerAdmin.destroy();
        if (!isExternalORB && orb!=null) {
            orb.shutdown(true);
            orb.destroy();
            orb=null;
        }
	}
	
	public ConsumerAdmin getConsumerAdmin() {
		return consumerAdmin;
	}
	
	public ORB getORB() {
		return orb;
	}
	
	/**
	 * Initialize the connection.
	 * 
	 * @param theORB The ORB. 
	 *               If it is null then a new CORBA connection is initialized.
	 * @param manager A reference to the Manager
	 *                If it is null a reference is built by reading the properties.
	 */
	public void initialize(ORB theORB, Manager manager) {
        isExternalORB = (theORB != null);        
		this.orb=theORB;

		if (orb==null) { 
			listenersDispatcher.publishReport("Initializing CORBA...");
		
			// ORB stanza
			java.util.Properties orbprops = java.lang.System.getProperties();
		
			orb = ORB.init(new String[0], orbprops);
		
			// POA stanza -- use RootPOA
			POA rootPOA = null;
			try
			{
				rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
		
			} catch (org.omg.CORBA.ORBPackage.InvalidName in)
			{
				throw new IllegalStateException("Cannot resolve RootPOA: " + in);
			}
			POAManager poaManager = rootPOA.the_POAManager();
			try
			{
				poaManager.activate();
			} catch (Exception e)
			{
				throw new IllegalStateException("POAManager activation failed." + e);
			}
			
			// setup ORB profiling
			try {
				if (orb instanceof AcsProfilingORB) {
					AcsORBProfiler profiler = new AcsORBProfilerImplBase(logger);
					((AcsProfilingORB)orb).registerAcsORBProfiler(profiler);
					logger.finer("Orb profiling set up, using class " + AcsORBProfilerImplBase.class.getName());
				}
			} catch (Throwable th) {
				logger.log(Level.WARNING, "Failed to setup ORB profiling.", th);
			}
		
			// end of CORBA stanza
			listenersDispatcher.publishReport("CORBA initialized.");
		}
		
		Manager maciManager=manager;
		
		if (maciManager==null) {
			maciManager = resolveManagerReference();
			if (maciManager == null) {
				return;  // HSO: Ale, should this not throw an exception?
			}
		} 
	
		NamingContext namingContext = resolveNamingServiceContext(maciManager);
		if (namingContext == null) {
			return;
		}
		
		if (!resolveNotifyChannel(LOGGING_XML_CHANNEL, namingContext)) {
			return;
		}
			
		boolean isConsumerAdminCreated = createConsumerAdmin();
		if (!isConsumerAdminCreated) {
			return;
		}
		
		isInitialized = createStructuredPushConsumer();
	}
	
	/**
	 * isInitialized method comment.
	 */
	public boolean isInitialized() {
		return isInitialized;
	}
	
	/**
	 * 
	 * @return true if the consumer is suspended
	 */
	public boolean isSupended() {
		return acsSPS.isSuspended();
	}
	
	private si.ijs.maci.Manager resolveManagerReference() {
		listenersDispatcher.publishReport("Resolving " + MANAGER_PROPERTY + " manager reference...");
		org.omg.CORBA.Object obj = null;
		si.ijs.maci.Manager manager;
		try {
			if (MANAGER_PROPERTY == null) throw new IllegalStateException("Manager system property ACS.manager is null.");
			obj = orb.string_to_object(MANAGER_PROPERTY);
			//if (obj == null) throw new IllegalStateException("Could not resolve Manager reference from the ACS.manager system property (" + MANAGER_PROPERTY + ").");
			manager = si.ijs.maci.ManagerHelper.narrow(obj);
		} catch (Exception e) {
			listenersDispatcher.publishReport("Exception occurred when resolving manager reference.");
			System.out.println("Exception in ACSRemoteAccess::resolveManagerReference(): " + e);
			return null;
		}
		
		listenersDispatcher.publishReport("Manager reference resolved.");
		
		return manager;
	}
	
	private NamingContext resolveNamingServiceContext(si.ijs.maci.Manager manager) {
		listenersDispatcher.publishReport("Resolving Naming Service...");
		org.omg.CORBA.Object nameService = null;
		try
		    {
		    nameService = manager.get_service(0, NAME_SERVICE, false);
		    }
		catch(NoPermissionEx e)
		    {
		    throw new IllegalStateException("Failed to obtaine NameService from the manager. No permission");
		    }
		catch(CannotGetComponentEx e)
		    {
		    throw new IllegalStateException("Failed to obtain NameService from the manager.");
		    }
		catch(ComponentNotAlreadyActivatedEx e)
		    {
		    throw new IllegalStateException("Failed to  obtaine NameService from the manager.");
		    }
		catch(ComponentConfigurationNotFoundEx e)
		    {
		    throw new IllegalStateException("Failed to  obtaine NameService from the manager.");
		    }
	
		NamingContext namingContext = null;
		try {
			namingContext = org.omg.CosNaming.NamingContextHelper.narrow(nameService);
		} catch (Exception e) {
			listenersDispatcher.publishReport("Exception occurred when narrowing Naming Service Context from the Naming Service.");
			System.out.println("Exception in resloveNamingServiceContext(): " + e);
			return null;
		}
		
		listenersDispatcher.publishReport("Naming Service resolved.");
		return namingContext;
	}
	
	private boolean resolveNotifyChannel(String channelName, NamingContext namingContext) {
		// Cannot use jcontnc Helper for this, due to module order...
		String channelWithDomain = channelName + NAMESERVICE_BINDING_NC_DOMAIN_SEPARATOR.value + ACS_NC_DOMAIN_LOGGING.value;
		
		listenersDispatcher.publishReport("Resolving channel \"" + channelWithDomain + "\" from Notify Service...");
		try {
			NameComponent[] nc = new NameComponent[1];
			nc[0] = new NameComponent(channelWithDomain, alma.acscommon.NC_KIND.value);
			
			org.omg.CORBA.Object obj = namingContext.resolve(nc);
	
			eventChannel = EventChannelHelper.narrow(obj);
			
		} catch (Exception e) {
			listenersDispatcher.publishReport("Exception occurred when obtaining channel \"" + channelWithDomain + "\" from the Notify Service.");
			System.out.println("ACSRemoteAccess::Exception in resolveNotifyChannel(): " + e);
			return false;
		}
		listenersDispatcher.publishReport("Channel \"" + channelWithDomain + "\" resolved.");
		return true;
	}

	public boolean isConnected() {
		if (acsSPS==null) {
			return false;
		}
		return acsSPS.isConnected();
	}

	/**
	 * Suspend the notification of the incoming logs
	 * @see LCEngine
	 * @param suspended If true suspend the notification of new logs
	 */
	public void setSuspended(boolean suspended) {
		if (acsSPS!=null) {
			acsSPS.setSupended(suspended);
		}
	}
	
	/**
	 * Close the threads and free all the resources
	 * @param sync If it is true wait the termination of the threads before returning
	 */
	public void close(boolean sync) {
		if (acsSPS!=null) {
			acsSPS.close(sync);
		}
	}

}
