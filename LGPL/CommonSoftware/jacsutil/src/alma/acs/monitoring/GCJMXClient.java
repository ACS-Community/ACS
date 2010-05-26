/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) Universidad Tecnica Federico Santa Maria, 2008
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
package alma.acs.monitoring;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.management.ManagementFactory;
import java.util.Date;
import java.util.Timer;
import java.util.TimerTask;

import javax.management.MBeanServerConnection;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;

import com.sun.tools.attach.AttachNotSupportedException;
import com.sun.tools.attach.VirtualMachine;

/**
 * Simple program that connects to a JVM running on the same host,
 * given the classname that contains the main method, and calls the
 * garbage collector periodically over it.
 * 
 * @author rtobar
 * @since ACS 9.0
 */
public class GCJMXClient {
	
	private static final String LOCAL_CONNECTOR_ADDRESS = "com.sun.management.jmxremote.localConnectorAddress";
	private VirtualMachine _vm;
	private MBeanServerConnection _remote;

	public GCJMXClient(String className) {
		
		int pid = 0;

		try {
			pid = getPID(className);
		} catch (IOException e) {
			System.err.println("Error while getting remote PID!: " + e.getMessage());
			System.exit(1);
		}
		
		if( pid == 0 ) {
			System.err.println("Couldn't find process for class " + className);
			System.exit(1);
		} else {
			
			// We have found the PID :)
			try {
				_vm = getVM(pid);
			} catch (IOException e) {
				System.err.println("Can't connect to remote VM: " + e.getMessage());
				System.exit(1);
			} catch (AttachNotSupportedException e) {
				System.err.println("Can't attach to the remote JVM: " + e.getMessage());
				System.exit(1);
			}
			
		}
	}

	public void connect() throws Exception {

			// get the connector address
		   String connectorAddress =
		           _vm.getAgentProperties().getProperty(LOCAL_CONNECTOR_ADDRESS);
		   
		   // no connector address, so we start the JMX agent
		   if (connectorAddress == null) {
		       String agent = _vm.getSystemProperties().getProperty("java.home") +
		               File.separator + "lib" + File.separator + "management-agent.jar";
		       _vm.loadAgent(agent);
		       
		       // agent is started, get the connector address
		       connectorAddress =
		               _vm.getAgentProperties().getProperty(LOCAL_CONNECTOR_ADDRESS);
		       
		       if ( connectorAddress == null ) {
		      	 System.err.println("Cannot connect :(");
		      	 System.exit(1);
		       }
		   }
		   
		   JMXServiceURL remoteURL = new JMXServiceURL(connectorAddress);

		JMXConnector connector = null;
		try {
			connector = JMXConnectorFactory.connect(remoteURL);
		} catch (IOException e) {
			System.err.println("Can't connect to the remote URL");
			System.exit(-1);
		}
		
		try {
			_remote = connector.getMBeanServerConnection();
		} catch (IOException e) {
			System.err.println("Can't get a connection to the remote MBeanServer");
			System.exit(-1);
		}
		
	}

	public MBeanServerConnection getRemote() {
		return _remote;
	}

	private VirtualMachine getVM(int pid) throws AttachNotSupportedException, IOException {

		if( _vm == null ) {
			VirtualMachine vm = VirtualMachine.attach(String.valueOf(pid));
			_vm = vm;
		}
		return _vm;
	}

	private int getPID(String className) throws IOException {
		String s;
		Integer remotePID = null;

		Process p = Runtime.getRuntime().exec("jps -lm");
		
		BufferedReader stdInput = new BufferedReader(new 
				InputStreamReader(p.getInputStream()));
		
		// read the output from the command
		while ((s = stdInput.readLine()) != null) {
			String[] ss = s.split("[\t ]+");
			if (ss.length > 1) {
				if (ss[1].equals(className)) { 
					remotePID = Integer.valueOf(ss[0]);
					break;
				}
			}
		}
		stdInput.close();
		
		if( remotePID == null ){
			//System.err.println("Can't obtain PID for the given class");
			return 0;
		}
		
		return remotePID.intValue();
	}

	public static void main(String[] args) {

		if( args.length < 2 ) {
			System.err.println("Must provide 2 arguments: <classname> <interval-in-ms>");
		}

		final GCJMXClient client = new GCJMXClient(args[0]);
		try {
			client.connect();
		} catch(Exception e) {
			System.err.println(e.getMessage());
			System.exit(1);
		}

		ObjectName objName = null;
		try {
			objName = new ObjectName(ManagementFactory.MEMORY_MXBEAN_NAME);
		} catch (MalformedObjectNameException e1) {
			System.err.println("bad name?");
			System.exit(1);
		}

		final ObjectName objNameF = objName;
		Timer timer = new Timer();
		TimerTask task = new TimerTask() {
			public void run() {
				MBeanServerConnection con = client.getRemote();
				try {
					con.invoke(objNameF, "gc", null, null);
					System.out.println("Called gc() on remote VM");
				} catch (Exception e) {
					System.err.println("Couldn't call gc() on remote VM");
				}
			}
		};
		timer.scheduleAtFixedRate(task, new Date(), Integer.parseInt(args[1]));
	}
}
