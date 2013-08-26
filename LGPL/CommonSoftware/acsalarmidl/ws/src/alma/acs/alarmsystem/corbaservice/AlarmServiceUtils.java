/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
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
package alma.acs.alarmsystem.corbaservice;

import org.omg.PortableServer.POAManager;
import org.omg.CORBA.ORB;
import org.omg.CORBA.Object;
import org.omg.CORBA.ORBPackage.InvalidName;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNaming.NamingContextExtHelper;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;

//import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;
import alma.alarmsystem.AlarmService;
import alma.alarmsystem.AlarmServiceHelper;

import alma.acs.util.ACSPorts;

/**
 * An helper class with a set of useful methods.
 * <P>
 * Some of the methods of this class can be used through a script.
 * <P>
 * AlarmServiceUtils needs a {@link ORB} that can be passed in the constructor
 * directly or through an instance of {@link ContainerServicesBase}.
 * <BR>
 * If the empty constructor is used, a new {@link ORB} is instantiated.
 * 
 * @author acaproni
 *
 */
public class AlarmServiceUtils {
	
	/**
	 * The main method
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		System.out.println("HERE");
		AcsLogger log=ClientLogManager.getAcsLogManager().getLoggerForApplication("AlarmServiceUtils", true);
		log.log(AcsLogLevel.DEBUG,"Started");
		for (String opt: args) {
			if (opt.compareToIgnoreCase("-h")==0 || opt.compareToIgnoreCase("--help")==0) {
				printUsage();
				System.exit(0);
			}
			if (opt.compareToIgnoreCase("-s")==0 || opt.compareToIgnoreCase("--shutdown")==0) {
				AlarmServiceUtils utils = new AlarmServiceUtils(log);
				AlarmService alarmService=null;
				try {
					alarmService=utils.getAlarmService();
				} catch (Throwable t) {
					log.log(AcsLogLevel.ERROR,"Error getting the alarm service",t);
					t.printStackTrace(System.err);
					utils.shutdownORB();
					System.exit(-1);
				}
				log.log(AcsLogLevel.DEBUG,"Got a reference to the alarm service");
				try {
					log.log(AcsLogLevel.DEBUG,"Stopping the alarm service");
					alarmService.shutdown();
					log.log(AcsLogLevel.DEBUG,"Alarm service stopped");
				} catch (Throwable t) {
					log.log(AcsLogLevel.ERROR,"Error shutting down the alarm service",t);
					t.printStackTrace(System.err);
					utils.shutdownORB();
					System.exit(-1);
				}
				utils.shutdownORB();
				log.log(AcsLogLevel.DEBUG,"Done");
			}
		}
	}
	
	public static void printUsage() {
		System.out.println("USAGE: "+AlarmServiceUtils.class.getName()+" [-h||--help] [-s|--shutdown]");
		System.out.println("\t-h|--help:     print this message and exit");
		System.out.println("\t-s|--shutdown: shuts down the alarm service");
	}
	
	/**
	 * The ORB
	 */
	private final ORB orb;
	
	/**
	 * The logger
	 */
	private final AcsLogger logger;
	
	/**
	 * Constructor
	 * 
	 * @param orb The ORB
	 * @param theLogger The logger
	 */
	public AlarmServiceUtils(ORB orb, AcsLogger theLogger) {
		if (orb==null) {
			throw new IllegalArgumentException("The ORB can't be null");
		}
		if (theLogger==null) {
			throw new IllegalArgumentException("The Logger can't be null");
		}
		this.orb=orb;
		this.logger=theLogger;
	}
	
//	/**
//	 * Constructor
//	 * 
//	 * @param containerServices The container services
//	 */
//	public AlarmServiceUtils(ContainerServicesBase containerServices) {
//		if (containerServices==null) {
//			throw new IllegalArgumentException("ContainerServices can't be null");
//		}
//		orb=containerServices.getAdvancedContainerServices().getORB();
//		logger=containerServices.getLogger();
//	}
	
	/**
	 * Constructor
	 * 
	 * @param theLogger The logger; if <code>null</code> a new logger is instantiated
	 */
	public AlarmServiceUtils(AcsLogger theLogger) {
		if (theLogger==null) {
			logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(AlarmServiceUtils.class.getName(), true);
		} else {
			logger=theLogger;
		}
		orb=initORB();
	}
	
	/**
	 * Get the {@link NamingContext}
	 * 
	 * @return The {@link NamingContext}
	 */
	private NamingContext getNamingContext() throws InvalidName {
		org.omg.CORBA.Object obj = orb.resolve_initial_references("NameService");
		NamingContext context = NamingContextExtHelper.narrow(obj);
		if (context==null) {
			throw new NullPointerException("Got a null NamingContext");
		}
		return context;
	}
	
	/**
	 * Get a reference to the {@link AlarmService}.
	 * 
	 * @return The {@link AlarmService} 
	 */
	public AlarmService getAlarmService() throws Exception {
		String name=alma.alarmsystem.AlarmServiceName.value;
		try {
			// try naming service first
			NamingContext ns = getNamingContext();
			NameComponent[] nameComponent= new NameComponent[1];
			nameComponent[0]=new NameComponent(name,"");
			Object alarmObj = ns.resolve(nameComponent);
			return AlarmServiceHelper.narrow(alarmObj);
		} catch (Throwable th) {
			logger.info("Failed to obtain alarm service reference from naming service, trying corbaloc...");
		}

		String corbaloc = "corbaloc::" + ACSPorts.getIP() + ":" + ACSPorts.getAlarmServicePort() + "/" + name;
		Object alarmObj = orb.string_to_object(corbaloc);
		return AlarmServiceHelper.narrow(alarmObj);
	}
	
	/**
	 * Return the type of the alarm service in use
	 * 
	 * @return <code>true</code> if the alarm system implementation is ACS,
	 * 			<code>false</code> otherwise
	 */
	public boolean getAlarmServiceType() throws Exception {
		AlarmService alService = getAlarmService();
		return alService.isACSAlarmService();
	}
	
	/**
	 * Get a reference to the {@link CERNAlarmService}.
	 * 
	 * @return The {@link CERNAlarmService} 
	 */
	public AlarmService getAcsAlarmService() throws Exception {
		AlarmService alService=getAlarmService();
		if (!alService.isACSAlarmService()) {
			throw new Exception("The CERN implementation of the alarm service is in use");
		}
		return alService;
	}

	/**
	 * Instantiate the ORB
	 * 
	 * @return the ORB
	 */
	private ORB initORB() {
		// ORB stanza
		java.util.Properties orbprops = java.lang.System.getProperties();
	
		ORB theORB = ORB.init(new String[0], orbprops);
	
		// POA stanza -- use RootPOA
		POA rootPOA = null;
		try
		{
			rootPOA = POAHelper.narrow(theORB.resolve_initial_references("RootPOA"));
	
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
		return theORB;
	}
	
	/**
	 * Shutdown the orb.
	 * <P>
	 * This method must be called if a new ORB is created by an object of this
	 * class.
	 */
	public void shutdownORB() {
		orb.shutdown(true);
	}
}

