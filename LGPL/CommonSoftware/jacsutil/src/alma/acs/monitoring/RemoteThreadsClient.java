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
package alma.acs.monitoring;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
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
 * 
 * For further details on the activation of the RMI connector, you can see
 * http://java.sun.com/docs/books/tutorial/jmx/remote/jconsole.html
 * 
 * @author rtobar
 */
public class RemoteThreadsClient {

	public  static final int RMI_DEFAULT_PORT = 9999;
	
	public  static final String RMI_DEFAULT_HOST = "localhost";
	
	private static final String LOCAL_CONNECTOR_ADDRESS =
      "com.sun.management.jmxremote.localConnectorAddress";
	
	private JMXServiceURL remoteURL = null;
	private MBeanServerConnection remote = null;
	private RemoteThreadsMBean rtmb = null;
	private JMXConnector connector = null;
	
	/**
	 * @param className
	 * @throws RemoteThreadsException
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

	public RemoteThreadsClient(int pid) throws RemoteThreadsException {
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
	
	public RemoteThreadsClient(String remoteHost, int remotePort) throws RemoteThreadsException {
		try {
			getServiceURL(remoteHost,remotePort);
		} catch (MalformedURLException e) {
			throw new RemoteThreadsException(e);
		}
	}
	
	private int getRemotePID(String className) throws IOException {
		String s;
		Integer remotePID = null;
		Process p = Runtime.getRuntime().exec("jps -lm");
		
		BufferedReader stdInput = new BufferedReader(new 
				InputStreamReader(p.getInputStream()));
		
		BufferedReader stdError = new BufferedReader(new 
				InputStreamReader(p.getErrorStream()));
		
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
		
		// read any errors from the attempted command
		while ((s = stdError.readLine()) != null) {
			System.out.println(s);
		}
		stdError.close();
		
		if( remotePID == null ){
			System.err.println("Can't obtain PID for the given class");
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

	private void getServiceURL(String host, int port) throws MalformedURLException {
		String connectorAddress = "service:jmx:rmi:///jndi/rmi://" +
											host + ":" + 
											port + "/jmxrmi";

		remoteURL = new JMXServiceURL(connectorAddress);
	}

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
}
