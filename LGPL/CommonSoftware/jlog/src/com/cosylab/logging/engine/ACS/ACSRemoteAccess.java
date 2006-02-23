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

import com.cosylab.logging.engine.RemoteAccess;
import com.cosylab.logging.LCEngine;

// for POA initialization
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.PortableServer.POAManager;

import org.omg.CORBA.ORB;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNotifyChannelAdmin.*;
import org.omg.CosNotifyFilter.*;
import org.omg.CosNaming.NamingContext;

/**
 * This class implements methods for declaring the naming service 
 * and the notification channel to which the client is subscribed.
 * 
 * Creation date: (10/30/2001 3:48:32 PM)
 * @author: 
 */
public final class ACSRemoteAccess implements RemoteAccess {
	public static final String MANAGER_PROPERTY = System.getProperty("ACS.manager");
	
	public static final String NAME_SERVICE = "NameService";
	public static final String LOGGING_CHANNEL = "LoggingChannel";
	
	private boolean isInitialized = false;
	private LCEngine engine = null;
	private ORB orb = null;
	private POA rootPOA = null;
//	private NamingContext namingContext = null;
	private EventChannel eventChannel = null;
	private ConsumerAdmin consumerAdmin = null;
	private ACSStructuredPushConsumer acsSPS = null;
	
	private Thread orbThread = null;
/**
 * ACSRemoteAccss constructor comment.
 */
public ACSRemoteAccess(LCEngine engine) {
	this.engine = engine;
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
private boolean createConsumerAdmin() {
	engine.reportStatus("Creating Consumer Admin...");
	try {
		//consumerAdmin = eventChannel.default_consumer_admin();

		// msekoran
		InterFilterGroupOperator ifgo = org.omg.CosNotifyChannelAdmin.InterFilterGroupOperator.OR_OP;
		org.omg.CORBA.IntHolder adminID = new org.omg.CORBA.IntHolder();
		
		consumerAdmin = eventChannel.new_for_consumers(ifgo, adminID);
	} catch (Exception e) {
		engine.reportStatus("Exception occurred when creating Consumer Admin.");
		System.out.println("Exception in ACSRemoteAccess::createConsumerAdmin(): " + e);
		return false;
	}
	engine.reportStatus("Consumer Admin created.");
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
	engine.reportStatus("Initializing Structured Push Consumer...");
	acsSPS = new ACSStructuredPushConsumer(this);
	if (!acsSPS.isInitialized) return false;

	acsSPS.connect();
	if (!acsSPS.isConnected) return false;
	
	acsSPS.setupEvents();
	if (!acsSPS.isEventSetup) return false;
	
	engine.reportStatus("Structured Push Consumer initialized.");
	return true;
}
/**
 * destroy method comment. Not implemented yet.
 */
public void destroy() {
//	System.out.println(">>> Before destroy.");
	acsSPS.destroy();
//	consumerAdmin.destroy();
	getORB().shutdown(false);
//	System.out.println(">>> After destroy.");
}
public ConsumerAdmin getConsumerAdmin() {
	return consumerAdmin;
}
/**
 * Returns the LCEngine.
 * Creation date: (11/2/2001 4:07:16 PM)
 * @return cosylab.logging.LCEngine
 */
public LCEngine getEngine() {
	return engine;
}
public ORB getORB() {
	return orb;
}
public POA getPOA() {
	return rootPOA;
}
/**
 * initialize method comment.
 */
public void initialize() {
	// System.out.println("Manger system property: " + System.getProperty("ACS.manager"));
	// CORBA stanza

	engine.reportStatus("Initializing CORBA...");

	// ORB stanza
	java.util.Properties orbprops = java.lang.System.getProperties();

	// to make code completely independed, properties have to be set using JVM -D mechanism
	
	// ORBacus
	//orbprops.put("org.omg.CORBA.ORBClass", "com.ooc.CORBA.ORB");
	//orbprops.put("org.omg.CORBA.ORBSingletonClass", "com.ooc.CORBA.ORBSingleton");
	
	// JacORB
	//orbprops.put("org.omg.CORBA.ORBClass", "org.jacorb.orb.ORB");
	//orbprops.put("org.omg.CORBA.ORBSingletonClass", "org.jacorb.orb.ORBSingleton");
	
	// Java JDK
	// none
	
	orb = org.omg.CORBA.ORB.init(new String[0], orbprops);

	// POA stanza -- use RootPOA
	rootPOA = null;
	try
	{
		rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));

	} catch (org.omg.CORBA.ORBPackage.InvalidName in)
	{
		throw new IllegalStateException("Cannot resolve RootPOA: " + in);
	}
	POAManager manager = rootPOA.the_POAManager();
	try
	{
		manager.activate();
		orbThread = new Thread();
		orbThread.setDaemon(true);
		orbThread.start();
	} catch (Exception e)
	{
		throw new IllegalStateException("POAManager activation failed." + e);
	}

	// end of CORBA stanza
	engine.reportStatus("CORBA initialized.");
	
	si.ijs.maci.Manager maciManager = resolveManagerReference();
	if (maciManager == null) return;

	NamingContext namingContext = resolveNamingServiceContext(maciManager);
	if (namingContext == null) return;
	
	boolean isNotifyResolved = resolveNotifyChannel(LOGGING_CHANNEL, namingContext);
	if (!isNotifyResolved) return;
		
	boolean isConsumerAdminCreated = createConsumerAdmin();
	if (!isConsumerAdminCreated) return;
	
	isInitialized = createStructuredPushConsumer();
}
/**
 * isInitialized method comment.
 */
public boolean isInitialized() {
	return isInitialized;
}
private si.ijs.maci.Manager resolveManagerReference() {
	engine.reportStatus("Resolving " + MANAGER_PROPERTY + " manager reference...");
	org.omg.CORBA.Object obj = null;
	si.ijs.maci.Manager manager;
	try {
		if (MANAGER_PROPERTY == null) throw new IllegalStateException("Manager system property ACS.manager is null.");
		obj = orb.string_to_object(MANAGER_PROPERTY);
		//if (obj == null) throw new IllegalStateException("Could not resolve Manager reference from the ACS.manager system property (" + MANAGER_PROPERTY + ").");
		manager = si.ijs.maci.ManagerHelper.narrow(obj);
	} catch (Exception e) {
		engine.reportStatus("Exception occurred when resolving manager reference.");
		System.out.println("Exception in ACSRemoteAccess::resolveManagerReference(): " + e);
		return null;
	}
	
	engine.reportStatus("Manager reference resolved.");
	
	return manager;
}
private NamingContext resolveNamingServiceContext(si.ijs.maci.Manager manager) {
	engine.reportStatus("Resolving Naming Service...");
	org.omg.CORBA.IntHolder holder = new org.omg.CORBA.IntHolder();
		
	org.omg.CORBA.Object nameService = manager.get_service(0, NAME_SERVICE, false, holder);
	if (nameService == null) throw new IllegalStateException("NameService obtained from the manager is null.");

	NamingContext namingContext = null;
	try {
		namingContext = org.omg.CosNaming.NamingContextHelper.narrow(nameService);
	} catch (Exception e) {
		engine.reportStatus("Exception occurred when narrowing Naming Service Context from the Naming Service.");
		System.out.println("Exception in resloveNamingServiceContext(): " + e);
		return null;
	}
	
	engine.reportStatus("Naming Service resolved.");
	return namingContext;
}
private boolean resolveNotifyChannel(String channelName, NamingContext namingContext) {
	engine.reportStatus("Resolving channel \"" + channelName + "\" from Notify Service...");
	try {
		NameComponent[] nc = new NameComponent[1];
		nc[0] = new NameComponent(channelName, "");
		
		org.omg.CORBA.Object obj = namingContext.resolve(nc);

		eventChannel = org.omg.CosNotifyChannelAdmin.EventChannelHelper.narrow(obj);
		
	} catch (Exception e) {
		engine.reportStatus("Exception occurred when obtaining channel \"" + channelName + "\" from the Notify Service.");
		System.out.println("ACSRemoteAccess::Exception in resolveNotifyChannel(): " + e);
		return false;
	}
	engine.reportStatus("Channel \"" + channelName + "\" resolved.");
	return true;
}

public boolean isConnected() {
	if (acsSPS==null) {
		return false;
	}
	return acsSPS.isConnected();
}

}
