/*******************************************************************************
 * ALMA - Atacama Large Millimiter Array
 * (c) European Southern Observatory, 2002
 * Copyright by ESO (in the framework of the ALMA collaboration)
 * and Cosylab 2002, All rights reserved
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 */

package alma.acsabeans.examples;

import com.cosylab.datatypes.AbstractDynamicValueEvent;

import abeans.core.AssertionFailed;
import abeans.core.defaults.MessageLogEntry;
import abeans.datatypes.Monitor;
import abeans.datatypes.MonitorAdapter;
import abeans.models.LinkEvent;
import abeans.pluggable.acs.ACSAbeansEngine;
import abeans.pluggable.acs.logging.LoggingLevel;
import alma.ACS.abeans.ROdouble;
import alma.PS.abeans.PowerSupply;

/**
 * Example demonstrating monitor usage. 
 * 
 * Run with arguments (replace environment variables to reflect your local environment):
 * 	-Dabeans.home=$ACSDATA/config/abeans/Config -DManager.defaultReference=corbaloc::$HOST:3000/Manager
 * 
 * <b>NOTE</b>: Manager port should be obtained using <i>getManagerPort()</i> function
 * defined in <i>acsstartupAcsPorts<i> script.<br/>
 * 
 * It can be also obtained dinamically by <code>alma.acs.util.ACSPorts.getManagerPort()</code> method,
 * which requires <i>-DACS.baseport=$ACS_INSTANCE</i> to be passed as JVM parameter.<br/>
 * 
 * All configuration management is automatically done by <i>acsStartJava</i> script,
 * so it is <b>strongly recommended</b> to run your applications (in production env.) using<br/>
 * <pre>acsStartJava full.class.name.of.your.java.application</pre>.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		$Id: HelloMonitor.java,v 1.3 2003/10/31 08:50:47 msekoran Exp $
 */
public class HelloMonitor extends ACSAbeansEngine
{
	/**
	 * Monitored property;
	 */
	private ROdouble readback;
	
	/**
	 * Implementation of monitor listener.
	 * <code>MonitorAdapter</code> support class was used not to implement
	 * the whole set of listener methods, override only needed methods.
	 */
	class MonitorListenerImpl extends MonitorAdapter
	{
			/**
		 * @see abeans.models.LinkListener#linkEstablished(abeans.models.LinkEvent)
		 */
		public void linkEstablished(LinkEvent event) {
			new MessageLogEntry(HelloMonitor.this, "Link established.", LoggingLevel.INFO).dispatch();
		}

		/**
		 * @see abeans.models.LinkListener#linkLost(abeans.models.LinkEvent)
		 */
		public void linkLost(LinkEvent event) {
			new MessageLogEntry(HelloMonitor.this, "Link lost.", LoggingLevel.INFO).dispatch();
		}

		/**
		 * @see com.cosylab.datatypes.DynamicValueListener#valueChanged(com.cosylab.datatypes.AbstractDynamicValueEvent)
		 */
		public void valueChanged(AbstractDynamicValueEvent event) {
			new MessageLogEntry(HelloMonitor.this, "Value changed    : " + readback.getLatestReceivedValue(), LoggingLevel.INFO).dispatch();
		}

		/**
		 * @see com.cosylab.datatypes.DynamicValueListener#valueUpdated(com.cosylab.datatypes.AbstractDynamicValueEvent)
		 */
		public void valueUpdated(AbstractDynamicValueEvent event) {
			new MessageLogEntry(HelloMonitor.this, "Value (heartbeat): " + readback.getLatestReceivedValue(), LoggingLevel.INFO).dispatch();
		}

	}

	/**
	 * Constructor.
	 */
	public HelloMonitor()
	{
		super();
		// initialize the engine (initializes all the services and logs into the manager)
		initialize();
	}

	/**
	 * Registers monitor to readback.
	 */
	public void doMonitoring()
	{
		try
		{
			// create a bean
			PowerSupply powerSupply = new PowerSupply();

			// set remote name and do the connect 		
			powerSupply.setRemoteName("TEST_PS_1");
			
			new MessageLogEntry(this, "Creating monitor...", LoggingLevel.INFO).dispatch();

			// create monitor
			readback = powerSupply.getReadback();
			Monitor monitor = readback.createMonitor(null, new MonitorListenerImpl());

			new MessageLogEntry(this, "Monitor created.", LoggingLevel.INFO).dispatch();

			// -------------------------------------------------------//

			// sleep for a while...
			try {
				// 5 secs
				Thread.sleep(5000);
			} catch (InterruptedException ie) {}

			// change value to see change effect
			powerSupply.getCurrent().setValue(readback.getLatestReceivedValue()+1.0);

			// sleep for a while...
			try {
				// 5 secs
				Thread.sleep(5000);
			} catch (InterruptedException ie) {}

			// change value to see change effect
			powerSupply.getCurrent().setValue(readback.getLatestReceivedValue()+1.0);

			// sleep for a while...
			try {
				// 5 secs
				Thread.sleep(5000);
			} catch (InterruptedException ie) {}

			// -------------------------------------------------------//

			// it is always nice to destroy a monitor
			monitor.destroy();			

			new MessageLogEntry(this, "Monitor destoryed.", LoggingLevel.INFO).dispatch();
		}
		catch (Throwable t)
		{
			// handle the exception 
			AssertionFailed af = new AssertionFailed(this, "Failed to register monitor.", t);
			// set class and name of the method where exception was caught 
			af.caughtIn(this, "registerMonitor");
			
			// exception service will handle this exception...
			
			// if you want to hide exception (no reports) uncomment this line
			//new abeans.core.defaults.ExceptionIgnorer(af);
		}
	}
	
	/**
	 * The main entry point for every Java application.
	 */
	public static void main(String[] args)
	{
		// create app
		HelloMonitor app = new HelloMonitor();
		
		// do monitoring
		app.doMonitoring();
		
		// destroy and exit
		app.destroy();
	}
}
