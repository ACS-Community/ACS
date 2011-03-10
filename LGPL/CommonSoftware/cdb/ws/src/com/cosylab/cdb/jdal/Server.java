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
 * @author dragan
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */
package com.cosylab.cdb.jdal;

import java.io.FileWriter;
import java.io.PrintWriter;
import java.util.Properties;
import java.util.StringTokenizer;
import java.util.logging.Logger;

import org.omg.CORBA.NO_RESOURCES;
import org.omg.CORBA.ORB;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNaming.NamingContextHelper;
import org.omg.PortableServer.IdAssignmentPolicyValue;
import org.omg.PortableServer.LifespanPolicyValue;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.PortableServer.Servant;

import com.cosylab.CDB.JDAL;
import com.cosylab.CDB.JDALHelper;
import com.cosylab.CDB.WJDALOperations;
import com.cosylab.CDB.WJDALPOATie;

import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.ClientLogManager;
import alma.acs.monitoring.SimpleCallInterceptor;
import alma.acs.util.ACSPorts;

public class Server {
	
	public static final String CDB_LOGGER_NAME = "CDB";
	
	public static final String LOG_CDB_CALLS_PROPERTYNAME = "alma.acs.cdb.log_corba_calls";
	
	public static void main (String[] args) {
		new Server().run(args);
	}

	private JDAL jdal; 
	
	public void run(String args[]) {
		String iorFileName = null;
		final Logger sharedLogger = ClientLogManager.getAcsLogManager().getLoggerForApplication(CDB_LOGGER_NAME, true);
		try {
		Properties properties = System.getProperties();
			boolean useJacORB = false; // default is JDK ORB
			int portNumber = Integer.parseInt(ACSPorts.getCDBPort());
			for (int i = 0; i < args.length; i++) {
				if (args[i].equals("-OAport") || args[i].equals("-OAPort")) {
					if (i < args.length - 1) {
						portNumber = Integer.valueOf(args[++i]).intValue();
					}
				}

				if (args[i].equals("-OAIAddr"))
				    {
				    if (i < args.length - 1) {
				    properties.put("OAIAddr", args[++i]);
				    }
				    }
				if (args[i].equals("-orbacus")) {
					sharedLogger.log(AcsLogLevel.NOTICE, "ORBacus is no longer supported, switching to JacORB.");
					//System.err.println(
					//	"ORBacus is no longer supported, switching to JacORB.");
					useJacORB = true;
				}
				if (args[i].equals("-jacorb")) {
					useJacORB = true;
				}
				if (args[i].equals("-o")) {
					if (i < args.length - 1) {
						iorFileName = args[++i];
					} else {
						iorFileName = "DAL.ior";
					}
				}

			}

		
			if (useJacORB) {
				sharedLogger.log(AcsLogLevel.DELOUSE, "DALfs will use JacORB ORB");
				properties.put("org.omg.CORBA.ORBClass", "org.jacorb.orb.ORB");
				properties.put(
					"org.omg.CORBA.ORBSingletonClass",
					"org.jacorb.orb.ORBSingleton");

				// port
				properties.put("OAPort", Integer.toString(portNumber));

				// ORB implementation name
				properties.put("jacorb.implname", "ORB");

				/*
				 * by setting the following property, the ORB will
				 * accept client requests targeted at the object with
				 * key "CDB", so more readable corbaloc URLs
				 * can be used
				 */

				properties.put(
					"jacorb.orb.objectKeyMap.CDB",
					"ORB/dalPOA/CDB");

			} else {
				properties.put(
					"com.sun.CORBA.POA.ORBPersistentServerPort",
					Integer.toString(portNumber));
			}

			// create and initialize the ORB
			ORB orb = ORB.init(args, properties);

			// get reference to rootpoa & activate the POAManager
			POA rootpoa =
				POAHelper.narrow(orb.resolve_initial_references("RootPOA"));

			/* create a user defined poa for the naming contexts */

			org.omg.CORBA.Policy[] policies = new org.omg.CORBA.Policy[2];

			policies[0] =
				rootpoa.create_id_assignment_policy(
					IdAssignmentPolicyValue.USER_ID);
			if (useJacORB)
				policies[1] =
					rootpoa.create_lifespan_policy(
						LifespanPolicyValue.PERSISTENT);
			else
				policies[1] =
					rootpoa.create_lifespan_policy(
						LifespanPolicyValue.TRANSIENT);

			POA dalpoa =
				rootpoa.create_POA(
					"dalPOA",
					rootpoa.the_POAManager(),
					policies);

			for (int i = 0; i < policies.length; i++)
				policies[i].destroy();

			rootpoa.the_POAManager().activate();

			// create servant and register it with the ORB

			final WDALImpl servantDelegate = new WDALImpl(args, orb, dalpoa, sharedLogger);
			WJDALOperations topLevelServantDelegate = servantDelegate;
			
			if (Boolean.getBoolean(LOG_CDB_CALLS_PROPERTYNAME)) {
				// Currently we only intercept the functional IDL-defined methods, by wrapping servantDelegate.
				// If we want to also intercept the CORBA admin methods, then *servant* should be wrapped with a dynamic proxy instead.
				WJDALOperations interceptingServantDelegate = SimpleCallInterceptor.createSimpleInterceptor(WJDALOperations.class, servantDelegate, sharedLogger);
				topLevelServantDelegate = interceptingServantDelegate;
			}
			final Servant servant = new WJDALPOATie(topLevelServantDelegate);
			
			//create object id
			byte[] id = { 'C', 'D', 'B' };

			//activate object
			dalpoa.activate_object_with_id(id, servant);

			// get object reference from the servant
			org.omg.CORBA.Object ref = dalpoa.servant_to_reference(servant);
			jdal = JDALHelper.narrow(ref);

			// try to bind it in IOR
			if (useJacORB) {
				// nothing to do here
			} else {
				(
					(com.sun.corba.se.internal.Interceptors.PIORB) orb)
							.register_initial_reference(
					"CDB",
					rootpoa.servant_to_reference(servant));
			}
			// register in name service if available
			try {
				// get the root naming context
				org.omg.CORBA.Object objRef = orb.resolve_initial_references("NameService");
				NamingContext ncRef = NamingContextHelper.narrow(objRef);
				// Bind the object reference in naming
				NameComponent nc = new NameComponent("CDB", "");
				NameComponent path[] = { nc };
				ncRef.rebind(path, jdal);
			}
			// GCH 2003-07-04
                        // Here I have to catch Exception and not RuntimeException,
                        // because otherwise org.omg.CORBA.ORBPackage.InvalidName
                        // is not catched and jDAL gives up.
                        // This situation occurrs when the NameSetvice initial reference
                        // is not set at all with the ORBInitRef.NameService property
			catch (Exception e1) {
				sharedLogger.log(AcsLogLevel.NOTICE, "JDAL is NOT registered in the name service because of: " + e1);
			}
			if (Integer.getInteger("ACS.logstdout", 4) < 4)
			    {
				sharedLogger.log(AcsLogLevel.INFO, "JDAL is listening on " + ACSPorts.getIP() + ":" + portNumber + "/CDB");
			    }

			// recover (notify) clients
			if (servantDelegate instanceof Recoverer) {
				((Recoverer) servantDelegate).recoverClients();
			}

			if (iorFileName != null) {
				// write the object reference to a file
				PrintWriter iorFile =
					new PrintWriter(new FileWriter(iorFileName));
				iorFile.println(orb.object_to_string(jdal));
				iorFile.close();
			}

			sharedLogger.log(AcsLogLevel.INFO, "JDAL is ready and waiting ...");
                        // GCH 2006-11-13
                        // Here we put also a println to be sure that the message
                        // ALWAYS appears on standart output, also if the logging level 
                        // is put higher than INFO.
                        // This is needed because the ACS startup scripts wait for this message
                        // to declare complete the startup of the CDB. 
			System.out.println("JDAL is ready and waiting ...");
                        
			// preload cache
			new Thread(new Runnable() {
				public void run() {
					preloadCache(servantDelegate.getDALImplDelegate(), sharedLogger);
				}
			}, "preload-cache").start();

			// wait for invocations from clients
			orb.run();
			sharedLogger.log(AcsLogLevel.INFO, "JDAL exiting ORB loop ...");

		} catch (Exception e) {
			sharedLogger.log(AcsLogLevel.NOTICE, "ERROR: " + e);
			e.printStackTrace(System.out);
		}
	}
	
	
	public void shutdown() {
		 jdal.shutdown();
	} 
	
	
	private static String[] PRELOAD_TABLE = {
		"MACI/Managers/Manager",
		"MACI/Components"
	};
	
	private static String[] PRELOAD_TABLE_SUBTREE = {
		"MACI/Containers"
	};

	private void preloadCache(DALImpl dal, Logger sharedLogger) {
		boolean allRead = false;
		sharedLogger.log(AcsLogLevel.DEBUG, "Starting pre-filling cache...");
		try
		{
			for (String node : PRELOAD_TABLE)
			{
				if (dal.wasCacheLimitReached())
					return;
				
				try {
					jdal.get_DAO(node);
				} catch (alma.cdbErrType.CDBXMLErrorEx xmlerr) {
					// noop
				} catch (alma.cdbErrType.CDBRecordDoesNotExistEx noRec) {
					// noop
				}
			}
				
			for (String subTree : PRELOAD_TABLE_SUBTREE)
			{
				if (dal.wasCacheLimitReached())
					return;

				String daos = jdal.list_nodes(subTree);
				if (daos != null)
				{
					StringTokenizer tokenizer = new StringTokenizer(daos);
					while (tokenizer.hasMoreTokens())
					{
						try {
							jdal.get_DAO(subTree+"/"+tokenizer.nextToken());
						} catch (alma.cdbErrType.CDBXMLErrorEx xmlerr) {
							// noop
						} catch (alma.cdbErrType.CDBRecordDoesNotExistEx noRec) {
							// noop
						}
					}
				}
			}

			allRead = true;
			
		} catch (NO_RESOURCES nores) {
			// shutdown
			sharedLogger.log(AcsLogLevel.DEBUG, "Cache filling canceled due to server shutdown.");
			return;
		}
		if (allRead)
			sharedLogger.log(AcsLogLevel.DEBUG, "Cache filling fully completed.");
		else
			sharedLogger.log(AcsLogLevel.DEBUG, "Cache filling partly completed, terminated since cache memory limit was reached.");
	}

	
	
}


