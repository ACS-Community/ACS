/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci.plug;

import java.util.Hashtable;
import java.util.logging.Logger;

import javax.naming.Context;
import javax.naming.InitialContext;

import org.omg.CORBA.ORB;

import alma.acs.util.ACSPorts;

/**
 * This component provides access to the CORBA Naming Service
 * via CosNaming JNDI interface.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class NamingServiceRemoteDirectory
{

	/**
	 * The constant denoting the default NS reference (if non is set).
	 */
	private static final String DEFAULT_REFERENCE = "iiop://" + ACSPorts.getIP() + ":" + ACSPorts.getNamingServicePort() + "/";

	/**
	 * Reference to CORBA Naming Service, use IIOP type reference - 'iiop://<host>[:<port>]'.
	 */
	private String reference = DEFAULT_REFERENCE;

	/**
	 * Root context of the remote directory (CORBA Naming Service).
	 */
	private Context context = null;

	/**
	 * Constructor for NamingServiceRemoteDirectory.
	 * @param orb	CORBA ORB.
	 * @param logger logger.
	 */
	public NamingServiceRemoteDirectory(ORB orb, Logger logger)
	{
		// system property overrides default configuration
		reference = System.getProperty("NamingServiceRemoteDirectory.reference", reference);

		internalInitialize(orb, logger);
	}

	/**
	 * Root context of the remote directory (CORBA Naming Service).
	 * @return		root context of the remote directory (CORBA Naming Service)
	 */
	public Context getContext()
	{
		return context;
	}

	/**
	 * Obtains root context of the remote directory (CORBA Naming Service).
	 * @param orb	CORBA ORB.
	 * @param logger logger.
	 */
	private void internalInitialize(ORB orb, Logger logger)
	{
		logger.info("Connecting to CORBA Naming Service with reference '"+reference+"'...");
		
		Hashtable<Object, Object> env = new Hashtable<Object, Object>();
		// set CosNamingFactory 
		env.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.cosnaming.CNCtxFactory");
		// set NS address
		env.put(Context.PROVIDER_URL, reference);
		// set orb
		env.put("java.naming.corba.orb", orb);

		try
		{
			context = new InitialContext(env);
			logger.info("Connected to CORBA Naming Service with reference '"+reference+"'.");
		}
		catch (Throwable ex)
		{
			//logger.log(Level.INFO, "Failed to connect to CORBA Naming Service with reference '"+reference+"'...", ex);
			logger.info("Failed to connect to CORBA Naming Service with reference '"+reference+"'...");
			return;
		}
		
	}

	/**
	 * Returns the reference of the naming service remote directory.
	 * @return String	the reference of the naming service remote directory
	 */
	public String getReference()
	{
		return reference;
	}

}
