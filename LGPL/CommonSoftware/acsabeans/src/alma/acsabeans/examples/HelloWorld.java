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

import abeans.core.defaults.MessageLogEntry;
import abeans.pluggable.acs.ACSAbeansEngine;
import abeans.pluggable.acs.logging.LoggingLevel;

/**
 * Standard "Hello world" example. 
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
 * @version		$Id: HelloWorld.java,v 1.3 2003/10/31 08:50:47 msekoran Exp $
 */
public class HelloWorld
{

	/**
	 * The main entry point for every Java application.
	 */
	public static void main(String[] args)
	{
		// initialize the engine (initializes all the services and logs into the manager)
		ACSAbeansEngine engine = new ACSAbeansEngine();
		engine.initialize();

		// print INFO priority message to the console using logging system
		new MessageLogEntry(engine, "Hello world!", LoggingLevel.INFO).dispatch();

		// destroy and exit
		engine.destroy();
	}
}
