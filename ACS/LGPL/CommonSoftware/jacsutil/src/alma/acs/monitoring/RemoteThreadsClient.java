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
import java.net.InetAddress;
import java.net.MalformedURLException;

import javax.management.JMX;
import javax.management.MBeanRegistrationException;
import javax.management.MBeanServerConnection;
import javax.management.MalformedObjectNameException;
import javax.management.ObjectName;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;

import com.sun.tools.attach.AgentInitializationException;
import com.sun.tools.attach.AgentLoadException;
import com.sun.tools.attach.AttachNotSupportedException;
import com.sun.tools.attach.VirtualMachine;

/**
 * Class used for connecting with a {@link RemoteThreads} MBean implementation.
 * The connection can be done with a local process, as well as with a process
 * running in a remote machine, with the RMI connector enabled on it.
 * <p>
 * For further details on the activation of the RMI connector, you can see
 * <a href="http://java.sun.com/docs/books/tutorial/jmx/remote/jconsole.html">
 * http://java.sun.com/docs/books/tutorial/jmx/remote/jconsole.html</a>
 * 
 * @author rtobar
 * @since ACS 7.0
 */
public class RemoteThreadsClient {

	/**
	 * Default port used by the RMI client-side connector
	 */
	public  static final int RMI_DEFAULT_PORT = 9999;
	
	/**
	 * Property name for getting a local JMX connector
	 */
	private static final String LOCAL_CONNECTOR_ADDRESS =
      "com.sun.management.jmxremote.localConnectorAddress";
	
	private JMXServiceURL remoteURL = null;
	private MBeanServerConnection remote = null;
	private RemoteThreadsMBean rtmb = null;
	private JMXConnector connector = null;
	
	/**
	 * Gets a new <code>RemoteThreadsClient</code> ready to be connected to the
	 * remote JVM indicated by the name of the launcher class (the one containing the
	 * <code>public static void main(String[])</code> method).
	 * @param className The name of the launcher class of the remote process
	 * @throws RemoteThreadsException Is thrown in the following cases:
	 * <ul>
	 * 	<li> The given PID isn't present in the list of java processes, or
	 * it can't be retreived by the <code>jps</code> program.</li>
	 * 	<li> Cannot connect to the remote JVM or can't attach to it.</li>
	 * 	<li> Can't load the JMX agent on the remote JVM or can't initialize it
	 * 	(if it isn't already loaded).</li>
	 * </ul>
	 */
	public RemoteThreadsClient(String className) throws RemoteThreadsException {
		
		int pid = 0;
		
		try {
			pid = getRemotePID(className);
		} catch (IOException e) {
			throw new RemoteThreadsException("Error while getting remote PID!",e);
		}
		
		if( pid == 0 ) {
			// We haven't found the PID :(.
			throw new RemoteThreadsException("Can't find PID for the " + className + " class");
			
		} else {
			
			// We have found the PID :)
			try {
				getServiceURL(pid);
			} catch (IOException e) {
				throw new RemoteThreadsException("Can't connect to remote VM",e);
			} catch (AgentLoadException e) {
				throw new RemoteThreadsException("Can't load the JMX agent into the remote JVM",e);
			} catch (AttachNotSupportedException e) {
				throw new RemoteThreadsException("Can't attach to the remote JVM",e);
			} catch (AgentInitializationException e) {
				throw new RemoteThreadsException("Can't initialize the remote JMX agent",e);
			}
			
		}
	}

	/**
	 * Gets a new <code>RemoteThreadsClient</code> ready to be connected to the
	 * remote JVM indicated by the given PID
	 * @param pid The PID of the remote java process
	 * @throws RemoteThreadsException If:
	 * <ul>
	 * 	<li> PID is less or equal to 0.</li>
	 * 	<li> Cannot connect to the remote JVM or can't attach to it.</li>
	 * 	<li> Can't load the JMX agent on the remote JVM or can't initialize it
	 * 	(if it isn't already loaded).</li>
	 * </ul>
	 */
	public RemoteThreadsClient(int pid) throws RemoteThreadsException {
		
		if( pid <= 0 ) {
			throw new RemoteThreadsException("PID must be grater than zero");
		}
		
		try {
			getServiceURL(pid);
		} catch (IOException e) {
			throw new RemoteThreadsException("Can't connect to remote VM",e);
		} catch (AgentLoadException e) {
			throw new RemoteThreadsException("Can't load the JMX agent into the remote JVM",e);
		} catch (AttachNotSupportedException e) {
			throw new RemoteThreadsException("Can't attach to the remote JVM",e);
		} catch (AgentInitializationException e) {
			throw new RemoteThreadsException("Can't initialize the remote JMX agent",e);
		}
	}
	
	/**
	 * Gets a new <code>RemoteThreadsClient</code> ready to be connected to the
	 * remote JVM present in the given host address. It uses the {@link #RMI_DEFAULT_PORT}
	 * for connecting to the remote host.
	 * @param remoteHost The remote host where the remote JVM is placed
	 * @throws RemoteThreadsException If a malformed URL is produced with the given
	 * host address.
	 */
	public RemoteThreadsClient(InetAddress remoteHost) throws RemoteThreadsException {
		try {
			getServiceURL(remoteHost, RMI_DEFAULT_PORT);
		} catch (MalformedURLException e) {
			throw new RemoteThreadsException(e);
		}
	}
	
	/**
	 * Gets a new <code>RemoteThreadsClient</code> ready to be connected to the
	 * remote JVM present in the given host address and port.
	 * @param remoteHost The remote host where the remote JVM is placed
	 * @param remotePort The port to be used for the RMI connection
	 * @throws RemoteThreadsException If a malformed URL is produced with the given
	 * host address and port.
	 */
	public RemoteThreadsClient(InetAddress remoteHost, int remotePort) throws RemoteThreadsException {
		
		if( remotePort <= 0 ) {
			throw new RemoteThreadsException("Port must be grater than zero");
		}
		
		try {
			getServiceURL(remoteHost,remotePort);
		} catch (MalformedURLException e) {
			throw new RemoteThreadsException(e);
		}
	}
	
	/**
	 * Connects the object instance to the remote JVM agent and gets a direct connection
	 * with the MBeanServer in order to be able to get the remote MBean.
	 * 
	 * @return If the connection has been sucessful or not
	 */
	public boolean connect() {
		
		try {
			connector = JMXConnectorFactory.connect(remoteURL);
		} catch (IOException e) {
			System.err.println("Can't connect to the remote URL");
			return false;
		}
		
		try {
			remote= connector.getMBeanServerConnection();
		} catch (IOException e) {
			System.err.println("Can't get a connection to the remote MBeanServer");
			return false;
		}
		
		return true;
	}

	/**
	 * Closes the connection with the remote MBeanServer. 
	 * @throws RemoteThreadsException If the connection has not been opened.
	 */
	public void close() throws RemoteThreadsException {
		if( connector != null) {
			try {
				connector.close();
			} catch (IOException e) {
				System.err.println("Cannot close connection to the remote MBeanServer");
			}
		} else {
			throw new RemoteThreadsException("The connection has not been opened");
		}
	}
	
	/**
	 * Returns an object representing the MBean registered on the remote JVM agent. If the
	 * MBean has not been registered, then it is registered and then retreived.
	 * @return A {@link RemoteThreadsMBean} object representing the MBean registered
	 * on the remote JVM agent.
	 * @throws RemoteThreadsException If:
	 * <ul>
	 * 	<li>The MBean can't be registered on the remote JVM agent</li>
	 * 	<li>A local reference for this MBean can't be get</li>
	 * </ul>
	 */
	public RemoteThreadsMBean getMBean() throws RemoteThreadsException {
		if ( rtmb == null ) {
			
			try {
			ObjectName remoteThreadsName = new ObjectName("alma.acs.monitoring:type=RemoteThreads");
			if( ! remote.isRegistered(remoteThreadsName))
	        	remote.createMBean("alma.acs.monitoring.RemoteThreads", remoteThreadsName);
	        
			rtmb = JMX.newMBeanProxy(remote,remoteThreadsName,RemoteThreadsMBean.class);
			} catch(MalformedObjectNameException e) {
				// shouldn't get never get here
			} catch(MBeanRegistrationException e) {
				throw new RemoteThreadsException("Can't register the MBean in the remote server",e);
			} catch(Exception e) {
				throw new RemoteThreadsException("Can't get a reference to the remote MBean",e);
			}
		}
		
		return rtmb;
	}

	private int getRemotePID(String className) throws IOException {
		String s;
		Integer remotePID = null;

		// The only way to get the java processes list is to use the
		// jps command (it's like a ps, but for only java processes).
		// The command output is parsed to get the name of the runnable
		// class. Then, the associated PID is stored.
		//
		// The sun.tools.jps.Jps class in the tools.jar jarfile is used
		// by the jps process. Anyways, this class only has a main
		// method, so no information can be retrieved by means of
		// public methods. So, if the main method is invoked in a static
		// way, the output should be parsed as well as it's being done
		// now (this is far more complex, since the stdout should be
		// redirected to another PrinterStream, and then read from it).
		// Better we execute the jps command and read directly from its
		// associated InputStream.

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

	private void getServiceURL(int pid)
		throws IOException, AgentLoadException, RemoteThreadsException,
		AttachNotSupportedException, AgentInitializationException {
		//	attach to the target application
	   final VirtualMachine vm = VirtualMachine.attach(String.valueOf(pid));
	   
	   // get the connector address
	   String connectorAddress =
	           vm.getAgentProperties().getProperty(LOCAL_CONNECTOR_ADDRESS);
	   
	   // no connector address, so we start the JMX agent
	   if (connectorAddress == null) {
	       String agent = vm.getSystemProperties().getProperty("java.home") +
	               File.separator + "lib" + File.separator + "management-agent.jar";
	       vm.loadAgent(agent);
	       
	       // agent is started, get the connector address
	       connectorAddress =
	               vm.getAgentProperties().getProperty(LOCAL_CONNECTOR_ADDRESS);
	       
	       if ( connectorAddress == null )
	      	 throw new RemoteThreadsException("Can't get the remote JVM connector address");
	   }
	   
	   remoteURL = new JMXServiceURL(connectorAddress);
	}

	private void getServiceURL(InetAddress host, int port) throws MalformedURLException {
		String hostName = host.getHostName();
		String connectorAddress = "service:jmx:rmi:///jndi/rmi://" +
											hostName + ":" + 
											port + "/jmxrmi";
	
		remoteURL = new JMXServiceURL(connectorAddress);
	}
}
