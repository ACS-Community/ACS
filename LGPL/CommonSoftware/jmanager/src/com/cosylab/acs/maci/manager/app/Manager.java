/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci.manager.app;

import abeans.core.Identifiable;
import abeans.core.Identifier;
import abeans.core.IdentifierSupport;
import abeans.core.defaults.MessageLogEntry;
import abeans.framework.FrameworkLayer;
import abeans.pluggable.acs.logging.LoggingLevel;

import com.cosylab.abeans.AbeansEngine;
import com.cosylab.acs.maci.manager.ManagerShutdown;

/**
 * Manager activation application.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class Manager implements Identifiable, ManagerShutdown
{
	/**
	 * Identifier.
	 */
	private Identifier id;

	/**
	 * Manager engine.
	 */
	private ManagerEngine engine;

	/**
	 * Shutdown status.
	 */
	private boolean shuttingDown = false;

	/**
	 * Constructor for Manager.
	 */
	public Manager()
	{
		getAbeansEngine().initialize();

		// userAllInitializationsDone() is called only by CosyPanel.initializeOwner(boolean) ?!!!
		//engine.userAllInitializationsDone();
		
		if (isDebug())
		    {
			new MessageLogEntry(this, "Manager", "Manager Application initialized.", LoggingLevel.OFF).dispatch();
		    }
		
	}

	/**
	 * @see com.cosylab.abeans.AbeansLaunchable#getAbeansEngine()
	 */
	public AbeansEngine getAbeansEngine()
	{
		if (engine == null)
			engine = new ManagerEngine(this);
		return engine;
	}

	/**
	 * @see abeans.core.Identifiable#getIdentifier()
	 */
	public Identifier getIdentifier()
	{
		if (id == null)
			id = new IdentifierSupport("Manager", "Manager", "Manager", "Manager", Identifier.APPLICATION);
		return id;
	}

	/**
	 * @see abeans.core.Identifiable#isDebug()
	 */
	public boolean isDebug()
	{
		return true;
	}

	/**
	 * @see com.cosylab.acs.maci.manager.ManagerShutdown#isShutdownInProgress()
	 */
	public boolean isShutdownInProgress()
	{
		return shuttingDown;
	}

	/**
	 * @see com.cosylab.acs.maci.manager.ManagerShutdown#shutdown()
	 */
	public synchronized void shutdown(boolean sigInt)
	{
		// delegate destroy
		if (!shuttingDown) {
			internalDestroy();
			if(!sigInt && !System.getProperty("ACS.noExit", "false").equalsIgnoreCase("true"))
				System.exit(0);
		}
	}

	/**
	 * @see com.cosylab.gui.core.CosyPanel#internalDestroy()
	 * This method is called within synchronized block.
	 */
	public void internalDestroy()
	{
		shuttingDown = true;
		((ManagerEngine)getAbeansEngine()).userDestroy();
		getAbeansEngine().destroy();
	}

 	/*****************************************************************************/
	/***************************** [ Main entry ] ********************************/
	/*****************************************************************************/

	/**
	 * Application main entry point.
	 * @param args
	 */
	public static void main(String[] args)
	{
		for (int i = 0; i < args.length; i++) {
			if (args[i].equals("-n") || args[i].equals("-nr")) {
				// do not read saved state of the Manager
				System.getProperties().put("Manager.recovery", "false");
			}
		}

		// disable Abeans shutdown hook
		System.setProperty(FrameworkLayer.PROPERTY_DISABLE_SHUTDOWN_HOOK, "true");
		
		/*Manager manager =*/ new Manager();
	}

}
