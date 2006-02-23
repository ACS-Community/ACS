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
package alma.acs.container.corba;

import java.util.Properties;

import alma.acs.util.CmdLineRegisteredOption;


/**
 * Configures the <a href="http://www.jacorb.org">JacORB</a> ORB.
 * <p>
 * Created on 24-Feb-2003
 * @author hsommer
 */
public class JacOrbConfigurator extends OrbConfigurator
{
	/* (non-Javadoc)
	 * @see alma.acs.container.corba.OrbConfigurator#getPortPropertyName()
	 */
	public String getPortPropertyName()
	{
		return "OAPort";
	}

	/**
	 * @see alma.acs.container.corba.OrbConfigurator#getORBClass()
	 */
	public String getORBClass() 
	{
		return "org.jacorb.orb.ORB";
	}

	/**
	 * @see alma.acs.container.corba.OrbConfigurator#getORBSingleton()
	 */
	public String getORBSingleton() 
	{
		return "org.jacorb.orb.ORBSingleton";
	}


	/**
	 * Delivers the properties to configure the JacORB ORB and POA.
	 * <p>
	 * From the JacORB 1.4 Programming Guide: 
	 * <table width="100%" border="1">
	 * <tr>
	 * <th>Property</th>
	 * <th>Description</th>
	 * <th>Type</th>
	 * </tr>
	 * <tr>
	 * <td>ORBInitRef.&lt;service&gt;</td>
	 * <td>Properties of this form configure initial service
	 *       objects which can be resolved via the ORB resolve_initial_references. A
	 *       variety of URL formats are supported.</td>
	 * <td>URL</td>
	 * </tr>
	 * <tr>
	 * <td>org.omg.PortableInterceptor.<br>ORBInitializerClass.&lt;name&gt;</td>
	 * <td>A portable interceptor initializer class instantiated
	 *       at ORBcreation.</td>
	 * <td>class</td>
	 * </tr>
	 * <tr>
	 * <td>jacorb.orb.objectKeyMap.&lt;name&gt;</td>
	 * <td>Maps an object key to an arbitrary string thereby
	 *       enabling better readability for corbaloc URLs.</td>
	 * <td>string</td>
	 * </tr>
	 * <tr>
	 * <td>jacorb.orb.print_version</td>
	 * <td>If enabled, the ORB's version number is printed
	 *       whenever the ORB is initialized. Default is on.</td>
	 * <td>boolean</td>
	 * </tr>
	 * <tr>
	 * <td>jacorb.verbosity</td>
	 * <td>Diagnostic verbosity level: 0 = off, 1 = important messages
	 *       and exceptions, &gt;= 3 = debug-level output (output may be extensive)</td>
	 * <td>integer</td>
	 * </tr>
	 * <tr>
	 * <td>jacorb.logfile</td>
	 * <td>Output destination for diagnostic log file. If
	 *       not set, then diagnostics are sent to standard output.</td>
	 * <td>file</td>
	 * </tr>
	 * <tr>
	 * <td>jacorb.debug.dump_outgoing_messages</td>
	 * <td>Hex dump outgoing messages. Default is off.</td>
	 * <td>boolean</td>
	 * </tr>
	 * <tr>
	 * <td>jacorb.debug.dump_incoming_messages</td>
	 * <td>Hex dump incoming messages. Default is off.</td>
	 * <td>boolean</td>
	 * </tr>
	 * <tr>
	 * <td>jacorb.giop_minor_version</td>
	 * <td>The GIOP minor version number to use
	 *       for newly createdIORs. Default is 2.</td>
	 * <td>integer</td>
	 * </tr>
	 * <tr></tr>
	 * <tr>
	 * <td>jacorb.retries</td>
	 * <td>Number of retries if connection cannot directly
	 *       be established. Default is 5.</td>
	 * <td>integer</td>
	 * </tr>
	 * <tr>
	 * <td>jacorb.retry_interval</td>
	 * <td >Time in milliseconds to wait between retries. Default is 500.</td>
	 * <td>millisec.</td>
	 * </tr>
	 * <tr>
	 * <td>jacorb.outbuf_size</td>
	 * <td>Size of network buffers for outgoing messages
	 *       in bytes. Default is 2048.</td>
	 * <td>byte</td>
	 * </tr>
	 * <tr>
	 * <td>jacorb.maxManagedBufSize</td>
	 * <td>This is NOT the maximum buffer size that can be
	 *       used, but just the largest size of buffers that will be kept and managed.
	 *       This value will be added to an internal constant of 5,so the real value
	 *       in bytes is 2** (5 + maxManagedBufSize- 1). You only need to increase this
	 *       value if you are dealing with LOTS of LARGE data structures. You may decrease it
	 *       to make the buffer manager release large buffers immediately rather than
	 *       keeping them for later reuse. Default is18.</td>
	 * <td>integer</td>
	 * </tr>
	 * <tr>
	 * <td>jacorb.connection.client_timeout</td>
	 * <td>Client-side timeout. This is set to non-zero in
	 *       order to stop blocking after specified number of milliseconds. Not set by
	 *       default.</td>
	 * <td>millisec.</td>
	 * </tr>
	 * <tr>
	 * <td>jacorb.connection.server_timeout</td>
	 * <td>Maximum time in milliseconds that a server keeps
	 *       a connection open if nothing happens. Not set by default.</td>
	 * <td>millisec.</td>
	 * </tr>
	 * <tr>
	 * <td>jacorb.reference_caching</td>
	 * <td>Whether or not JacORB caches objects references.
	 *       Not set by default.</td>
	 * <td>boolean</td>
	 * </tr>
	 * <tr>
	 * <td>jacorb.hashtable_class</td>
	 * <td>The following property specifies the class which
	 *       is used for reference caching. WeakHashtable uses WeakReferences, so entries
	 *       get garbage collected if only the Hashtable has a reference to them. This
	 *       is useful if you have many references to short-living non-persistent CORBA
	 *       objects. It is only available for java 1.2 and above. On the other hand the
	 *       standard Hashtable keeps the references until they are explicitly deleted
	 *       by calling release(). This is useful for persistent and long-living CORBA
	 *       objects. Defaults to Hashtable.</td>
	 * <td>class</td>
	 * </tr>
	 * <tr>
	 * <td>jacorb.use_bom</td>
	 * <td >Use GIOP 1.2 byte order markers, since CORBA 2.4-5. Default is off.</td>
	 * <td>boolean</td>
	 * </tr>
	 * <tr>
	 * <td>jacorb.giop.add_1_0_profiles</td>
	 * <td>Add additional IIOP 1.0 profiles even if using IIOP 1.2. Default is off.</td>
	 * <td>boolean</td>
	 * </tr>
	 * <tr>
	 * <td>org.omg.PortableInterceptor.<br>ORBInitializerClass.bidir_init
	 * </td>
	 * <td>This portable interceptor must be configured to
	 *       support bi-directional GIOP. Not set by default.</td>
	 * <td>class</td>
	 * </tr>
	 * <tr>
	 * <td>jacorb.ior_proxy_host</td>
	 * <td>The jacorb.ior proxy host and jacorb.ior proxy port properties
	 *       inform the ORB what IP/port IORs should contain, if the ServerSockets IP/port
	 *       can't be used (e.g. for traffic through a firewall). WARNING: this is just
	 *       dumb replacing, so you have to take care of your configuration! Not set by
	 *       default.</td>
	 * <td>node</td>
	 * </tr>
	 * <tr>
	 * <td>jacorb.ior_proxy_port</td>
	 * <td>See jacorb.ior proxy host above. Not set by default.</td>
	 * <td>port</td>
	 * </tr>
	 * <tr>
	 * <td>OAIAddr</td>
	 * <td>The Object Adapter Internet Address: IPaddress
	 *       on multi-homed host (this gets encoded in object references). NOTE: Addresses
	 *       like 127.0.0.X will only be accessible from the same machine! Not set by
	 *       default.</td>
	 * <td>node</td>
	 * </tr>
	 * <tr>
	 * <td>OAPort</td>
	 * <td>See OAIAddr above. Not set by default.</td>
	 * <td>port</td>
	 * </tr>
	 * <tr>
	 * <td>org.omg.PortableInterceptor.<br>ORBInitializerClass.standard_init
	 * </td>
	 * <td>Standard portable interceptor. DO NOT REMOVE.</td>
	 * <td>class</td>
	 * </tr>
	 * <tr>
	 * <td>jacorb.net.socket_factory</td>
	 * <td>Sets or defines the socket factory that must implement
	 *       the operations defined in the org.jacorb.orb.factory.SocketFactory interface.</td>
	 * <td>class</td>
	 * </tr>
	 * <tr>
	 * <td>jacorb.net.server_socket_factory</td>
	 * <td>Sets or defines the socket factory that must implement
	 *       the operations defined in the org.jacorb.orb.factory.ServerSocketFactory
	 *       interface.</td>
	 * <td>class</td>
	 * </tr>
	 * <tr>
	 * <td>jacorb.net.socket_factory.port.min
	 * </td>
	 * <td>Sets the minimum port number that can be used
	 *       for an additional supported socket factory. This property is used in conjunction
	 *       with the jacorb.net.socket factory.port.max property. These properties enable
	 *       the factory to traverse firewalls through a fixed port range. Default is
	 *       unset, disabling the factory.</td>
	 * <td>integer</td>
	 * </tr><tr>
	 * <td>jacorb.net.socket_factory.port.max</td>
	 * <td>Sets the maximum port number that can be used
	 *       for the additional supported socket factory. Refer to jacorb.net.socket factory.port.min
	 *       above. Default is unset, disabling the factory.</td>
	 * <td>integer</td>
	 * </tr><tr>
	 * <td>jacorb.poa.monitoring</td>
	 * <td>Displays a GUI monitoring tool for servers. Default is off.</td>
	 * <td>boolean</td>
	 * </tr><tr>
	 * <td>jacorb.poa.thread_pool_max</td>
	 * <td>Maximum thread pool configuration for request processing.</td>
	 * <td>integer</td>
	 * </tr><tr>
	 * <td>jacorb.poa.thread_pool_min</td>
	 * <td>Minimum thread pool configuration for request processing.</td>
	 * <td>integer</td>
	 * </tr><tr>
	 * <td>jacorb.poa.thread_priority</td>
	 * <td>If set, request processing threads in the POA will run at this priority. 
	 *       If not set or invalid, MAX PRIORITY will be used. Not set by default.</td>
	 * <td>integer</td>
	 * </tr><tr>
	 * <td>jacorb.poa.queue_max</td>
	 * <td>The size of the request queue. Clients will receive Corba.TRANSIENT exceptions 
	 *       if load exceeds this limit. Default is 100.</td>
	 * <td>integer</td>
	 * </tr>
	 * </table>
	 * <p>
	 * Information on threading: 
	 * <p>
	 * JacORB currently offers one server side thread model. 
	 * The POA responsible for a given request will obtain a request processor thread 
	 * from a central thread pool. The pool has a certain size which is always between 
	 * the maximum and minimum value configured by setting the properties 
	 * <code>jacorb.poa.thread_pool_max</code> and <code>jacorb.poa.thread_pool_min</code>.
	 * <p><font size="-1">
	 * When a request arrives and the pool is found to contain no threads because all existing
	 * threads are active, new threads may be started until the total number of threads reaches
	 * <code>jacorb.poa.thread_pool_max</code>. 
	 * Otherwise, request processing is blocked until a thread is returned to the pool. 
	 * Upon returning threads that have finished processing a request to the pool, 
	 * it must be decided whether the thread should actually remain in the pool or be destroyed. 
	 * If the current pool size is above the minimum, a processor thread will not 
	 * be out into the pool again. 
	 * Thus, the pool size always oscillates between max and min.
	 * Setting min to a value greater than one means keeping a certain number of threads 
	 * ready to service incoming requests without delay. This is especially useful if you know 
	 * that requests are likely to come in in a bursty fashion. 
	 * Limiting the pool size to a certain maximum is done to prevent servers from occupying
	 * all available resources.
	 * Request processor threads usually run at the highest thread priority. 
	 * It is possible to influence thread priorities by setting the property 
	 * <code>jacorb.poa.thread_priority</code> to a value between Java's <code>Thread.MIN PRIORITY</code> 
	 * and <code>Thread.MAX PRIORITY</code>. If the configured priority value is invalid 
	 * JacORB will assign maximum priority to request processing threads.
	 * </font>
	 * 
	 * @see alma.acs.container.corba.OrbConfigurator#getProperties()
	 */
	protected Properties _getProperties()
	{
		Properties props = new Properties();
		
		if (debug)
		{
			props.put("jacorb.verbosity", "3");
		}

		return props;
	}


	/**
	 * @see alma.acs.container.corba.OrbConfigurator#_declareOptions()
	 */
	protected CmdLineRegisteredOption[] _declareOptions()
	{
		return null;
	}

}
