/*
 * @@COPYRIGHT@@
 */
 
package com.cosylab.acs.maci.manager.app;

import java.util.logging.Level;
import java.util.logging.LogRecord;

import alma.acs.logging.formatters.ConsoleLogFormatter;

import com.cosylab.acs.maci.manager.ManagerShutdown;

/**
 * Manager activation application.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class Manager implements ManagerShutdown
{

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
		getManagerEngine().initialize();

		LogRecord record = new LogRecord(Level.INFO, "AcsManagerStatusMessage_ManagerStarted Manager Application initialized.");
		record.setLoggerName(getManagerEngine().getLogger().getName());
		String formattedString = new ConsoleLogFormatter().format(record);
		System.out.println(formattedString);
		//getManagerEngine().getLogger().log(Level.OFF, "AcsManagerStatusMessage_ManagerStarted Manager Application initialized.");
	}

	/**
	 * Get manager engine.
	 */
	public ManagerEngine getManagerEngine()
	{
		if (engine == null)
			engine = new ManagerEngine(this);
		return engine;
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
	 * This method is called within synchronized block.
	 */
	public void internalDestroy()
	{
		shuttingDown = true;
		getManagerEngine().destroy();
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

		/*Manager manager =*/ new Manager();
	}

}
