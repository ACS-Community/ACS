/*
 * @@COPYRIGHT@@
 */

package abeans.pluggable.acs.maci;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.logging.Level;

import javax.naming.NamingException;

import com.cosylab.util.NameValueList;

import org.omg.CORBA.IntHolder;
import org.omg.CORBA.ORB;
import org.omg.CosNaming.NamingContextHelper;

import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ClientInfo;
import si.ijs.maci.ClientPOA;
import si.ijs.maci.Manager;
import si.ijs.maci.ManagerHelper;

import abeans.core.AssertionFailed;
import abeans.core.ComponentDescriptor;
import abeans.core.ComponentInitializationException;
import abeans.core.ComponentManager;
import abeans.core.IllegalComponentStateException;
import abeans.core.InitializationException;
import abeans.core.Node;
import abeans.core.Root;
import abeans.core.UnableToInstallComponentException;
import abeans.core.defaults.AbeansProperties;
import abeans.core.defaults.Configurable;
import abeans.core.defaults.ExceptionIgnorer;
import abeans.core.defaults.MessageLogEntry;
import abeans.datatypes.TypelessProperty;
import abeans.models.Connectable;
import abeans.models.acs.baci.TypelessPropertyDescriptor;
import abeans.pluggable.DatabaseProxy;
import abeans.pluggable.Plug;
import abeans.pluggable.Proxy;
import abeans.pluggable.RemoteDirectory;
import abeans.pluggable.RemoteException;
import abeans.pluggable.RemoteInfo;
import abeans.pluggable.acs.CORBAService;
import abeans.pluggable.acs.DefaultCORBAService;
import abeans.pluggable.acs.logging.RemoteLoggingService;

import alma.acs.util.ACSPorts;

import alma.maciErrType.CannotGetComponentEx;
import alma.maciErrType.ComponentNotAlreadyActivatedEx;
import alma.maciErrType.ComponentConfigurationNotFoundEx;
import alma.maciErrType.NoPermissionEx;

/**
 * Implementation of the <code>abeans.pluggable.Plug</code> that starts up and
 * shuts down the ACS database and take care for configuration and policy management.
 * The plug is managed automatically by the Abeans framework.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 * @see		Plug
 */
public class ACSPlug extends Plug implements Configurable
{

	
	Map unavailableConnectables = new HashMap();
	
	/**
	 * Implementation of maci::Client interface. 
	 */
	private class Client extends ClientPOA
	{
		/**
		 * Client name. 
		 */
		private static final String CLIENT_NAME = "Abeans R3 Client";

		/**
		 * Manager stringified reference (corbaloc). 
		 */
		private String managerReference = null;
		
		/**
		 * Default constructor.
		 */
		public Client(String managerReference)
		{
			this.managerReference = managerReference;
		}
		
		/**
		 * @see si.ijs.maci.ClientOperations#authenticate(java.lang.String)
		 */
		public String authenticate(String question) {
			return "C";
		}

		/**
		 * Find all connectables and reconnect
		 * @see si.ijs.maci.ClientOperations#components_available(si.ijs.maci.COBInfo[])
		 */
		public void components_available(ComponentInfo[] components) {
			for (int n = 0; n < components.length; n++)
			{
				ComponentInfo ci = components[n];
				String curl = ci.name;
				if (curl == null || ci.reference == null)
					continue;

				new MessageLogEntry(ACSPlug.this, "Client.components_available", "Component '" + curl + "' is available again, reconnecting...", Level.INFO).dispatch();

				synchronized (componentProxyReferenceCountMap)
				{
					ComponentProxyReferenceInfoEntry cprie = (ComponentProxyReferenceInfoEntry)componentProxyReferenceCountMap.get(curl);
					if (cprie != null)
					{
						
						// create new proxy, update it and mark as available
						String compomentProxyClassName = IDtoClassName(ci.type, true);
						Proxy newProxy = instatiateComponentProxy(curl, ci.reference, compomentProxyClassName);

						cprie.setComponentProxy(newProxy);
						cprie.markAvailable();

						
						// reconnect
						ArrayList al = cprie.getConnectables();
						for (int i = 0; i < al.size(); i++)
						{
							Connectable c = (Connectable)al.get(i);

							// remove from unavailable list
							unavailableConnectables.remove(c);

							try {
								c.initialize(cprie.getComponentProxy());
							} catch (Throwable t) {
								AssertionFailed af = new AssertionFailed(ACSPlug.this, "Failed to reconnect: '" + curl + "'.", t);
								af.caughtIn(this, "Client.components_available");
								af.putValue("c", c);
								af.putValue("curl", curl);
								// execption handler will handle this
							}
						}
					}
				}
			}
		}

		/**
		 * @see si.ijs.maci.ClientOperations#components_unavailable(java.lang.String[])
		 */
		public void components_unavailable(String[] curls) {
			// call disconnect on all connectables
			for (int n = 0; n < curls.length; n++)
			{
				String curl = curls[n];

				new MessageLogEntry(ACSPlug.this, "Client.components_unavailable", "Component '" + curl + "' is reported as unavailable, disconnecting...", Level.INFO).dispatch();
				
				synchronized (componentProxyReferenceCountMap)
				{
					ComponentProxyReferenceInfoEntry cprie = (ComponentProxyReferenceInfoEntry)componentProxyReferenceCountMap.get(curl);
					if (cprie != null)
					{
						// mark as unavailable
						cprie.markAsUnavailable();
						
						ArrayList al = cprie.getConnectables();
						for (int i = 0; i < al.size(); i++)
						{
							Connectable c = (Connectable)al.get(i);

							// add to unavailable list (remember its curl)
							unavailableConnectables.put(c, curl);
							
							c.disconnect();
						}
					}
				}
			}
		}

		/**
		 * @see si.ijs.maci.ClientOperations#disconnect()
		 */
		public void disconnect() {
			// TODO disconnect

		}

		/**
		 * @see si.ijs.maci.ClientOperations#message(short, java.lang.String)
		 */
		public void message(short type, String message) {
			
			String typeOfMessage;
			Level level;
			if (type == Client.MSG_ERROR)
			{
				typeOfMessage = "[ERROR]";
				level = Level.WARNING;
			}
			else
			{
				typeOfMessage = "[INFO]";
				level = Level.INFO;
			}
			
			new MessageLogEntry(ACSPlug.this, "message", "Message from the Manager '" + managerReference + "': " + typeOfMessage + " " + message, level).dispatch();
		}

		/**
		 * @see si.ijs.maci.ClientOperations#name()
		 */
		public String name() {
			return CLIENT_NAME;
		}

		/**
		 * @see si.ijs.maci.ClientOperations#ping()
		 */
		public boolean ping() {
			return true;
		}

	}

	/**
	 * ManagerClient structure. 
	 */
	private class ManagerClientInfoEntry
	{
		/**
		 * Manager CORBA referece.
		 */
		private Manager manager;
		
		/**
		 * ClientInfo structure.
		 */
		private ClientInfo clientInfo;

		/**
		 * Default constructor.
		 */
		public ManagerClientInfoEntry(Manager manager, ClientInfo clientInfo)
		{
			this.manager = manager;
			this.clientInfo = clientInfo;
		}
		
		/**
		 * Returns ClientInfo structure.
		 * @return ClientInfo structure.
		 */
		public ClientInfo getClientInfo() {
			return clientInfo;
		}

		/**
		 * Returns manager CORBA reference.
		 * @return manager CORBA reference.
		 */
		public Manager getManager() {
			return manager;
		}

	}

	/**
	 * ComponentReference structure. 
	 */
	private class ComponentProxyReferenceInfoEntry
	{
		/**
		 * Component Proxy.
		 */
		private Proxy proxy;
		
		/**
		 * Reference count.
		 */
		private int referenceCount;
		
		private boolean unavailable = false;
		
		public void markAsUnavailable()
		{
			unavailable = true;
		}

		public void markAvailable()
		{
			unavailable = false;
		}

		public boolean isUnavailable()
		{
			return unavailable;
		}

		private ArrayList connectables = new ArrayList();

		public ArrayList getConnectables()
		{
			return connectables;
		}
		
		/**
		 * Default constructor.
		 */
		public ComponentProxyReferenceInfoEntry(Proxy proxy, Connectable connectable)
		{
			this.proxy = proxy;
			connectables.add(connectable);
			this.referenceCount = 1;
		}
		
		/**
		 * Returns component proxy.
		 * @return component proxy.
		 */
		public Proxy getComponentProxy() {
			return proxy;
		}

		/**
		 * Set component proxy.
		 * @param newProxy component proxy.
		 */
		public void setComponentProxy(Proxy newProxy) {
			this.proxy = newProxy;
		}

		/**
		 * Returns reference count.
		 * @return reference count.
		 */
		public int getReferenceCount() {
			return referenceCount; // or connectables.size();
		}
		
		/**
		 * Increments reference count.
		 */
		public void incrementReferenceCount(Connectable c)
		{
			connectables.add(c);
			referenceCount++;
		}

		/**
		 * Decrements reference count.
		 * @return reference count.
		 */
		public int decrementReferenceCount(Connectable c)
		{
			connectables.remove(c);
			referenceCount--;
			return referenceCount;
		}

	}

	/**
	 * The constant denoting the name of this plug.
	 */
	public static final String PLUG_NAME = "ACS";
	
	/**
	 * The constant denoting the name of the configuration resource (file).
	 */
	private static final String ACS_RESOURCE_LOC = PLUG_NAME+"Plug";
	
	/**
	 * The constant denoting the prefix of the MANAGER configuration keys.
	 */
	private static final String MANAGER_PREFIX = "Manager";

	/**
	 * The constant denoting the name of default DAL reference property key.
	 */
	private static final String DEFAULT_MANAGER_REFERENCE = MANAGER_PREFIX + ".defaultReference";

	/**
	 * The constant denoting the default DAL reference (if non is set).
	 */
	private static final String DEFAULT_REFERENCE = "corbaloc::" + ACSPorts.getIP() + ":" + ACSPorts.getManagerPort() + "/Manager";

	/**
	 * Manager CORBA reference (if not authority is specified).
	 */
	private String defaultManagerReference = null;

	/**
	 * Flag indicating if initial manager connection was done (has to be tried only once). 
	 */
	private boolean initialManagerConnectDone = false;
	
	/**
	 * Reference to the database proxy implementation.
	 */
	private DatabaseProxyImpl db = null;

	/**
	 * CORBA del reference cache.
	 */
	private Map managerCache = new NameValueList();

	/**
	 * CORBA Object Request Broker (ORB) reference.
	 */
	private ORB orb = null;

	/**
	 * Component reference count map.
	 */
	private Map componentProxyReferenceCountMap = new HashMap();
	
	/**
	 * Properties of the plug.
	 */
	private AbeansProperties plugConfig = null;

	
	/**
	 * ACS remote directory.
	 */
	private RemoteDirectory directory = null;

	/**
	 * Component description of this plug.
	 */
	private final transient ComponentDescriptor descriptor =
		new ComponentDescriptor(getClass(), Plug.class, 1, "ACS plug for BACI model", false, false, null);

	/**
	 * Creates a new instance of the plug. This constructor is automatically 
	 * invoked by the Abeans framework.
	 */
	public ACSPlug()
	{
		super(PLUG_NAME);
	}

	/**
	 * Returns <code>BACI</code> to indicate that this plug uses BACI model.
	 * 
	 * @return <code>BACI</code>
	 * @see abeans.pluggable.Plug#getSupportedLibrary()
	 */
	public String getSupportedLibrary()
	{
		return "BACI";
	}

	/**
	 * Performs the connect of the specified connectable.
	 * If the database engine from <code>abeans.engine</code> is supplied as connectable,
	 * an instance of <code>DatabaseProxyImpl</code> is returned. Otherwise
	 * the connectable entity is resolved from the remote info supplied with
	 * connectable <code>c</code> and is returned as proxy into the Abean.
	 * 
	 * @param	c				the connectable to connect, non-<code>null</code>
	 * @throws	RemoteException	if the connection fails
	 * @see 	abeans.pluggable.Plug#internalConnect(Connectable)
	 */
	public void internalConnect(Connectable c) throws RemoteException
	{
		assert (db != null);
		assert (c != null);

		if (c.getRemoteInfo() == null)
		{
			RemoteException re = new RemoteException(this, "Connectable has a null remote info.");
			re.caughtIn(this, "internalConnect"); 
			re.putValue("c", c);
			throw re;
		}

		if (DatabaseProxy.DATABASE_NAME.equals(c.getRemoteInfo().getName()))
		{
			internalDatabaseConnect(c);
		}
		else
		{
			assert (orb != null);
			
			ManagerClientInfoEntry mcie = null;
			String corbaloc = defaultManagerReference;
						
			try
			{
				String authority = c.getRemoteInfo().getAuthority();
				if (authority != null && authority.length() > 0)
				{
					StringBuffer tmp = new StringBuffer("corbaloc::");
					tmp.append(authority);
					tmp.append("/Manager");
					corbaloc = tmp.toString();
				}

				synchronized(managerCache)
				{
					mcie = (ManagerClientInfoEntry)managerCache.get(corbaloc);
				}
				
				if (mcie==null)
				{
					
					// connect
					mcie = connectToManager(corbaloc);

					// cache login - manager and clientInfo
					synchronized(managerCache)
					{
						managerCache.put(corbaloc, mcie);
					}
					
					if (isDebug())
						new MessageLogEntry(this, "internalConnect", "Logged in to the Manager '" + corbaloc + "'.", Level.INFO).dispatch();

				}

			} catch (Exception ex)
			{
				RemoteException re = new RemoteException(this, "Failed to login to the Manager with reference '" + corbaloc + "' for connectable '" + c + "'.", ex);
				re.caughtIn(this, "internalConnect");
				re.putValue("c", c);
				throw re;
			}

			boolean connectComponent = false;
			Proxy proxy = null;
			String curl = null;
			try
			{
				curl = c.getRemoteInfo().toURI().getPath().substring(1);
				
				// check if unavailable (disconnected) connectable is connecting to other compoment
				// if it is, then release the connectable
				String preexistingCURL = (String)unavailableConnectables.get(c); 
				if (preexistingCURL != null)
				{
					releaseConnectable(c, preexistingCURL, true);
				}
				
				// TODO is this OK - e.g. can exist same curls for different authority...
				// check if already connected to this component
				ComponentProxyReferenceInfoEntry cprie = null;
				synchronized (componentProxyReferenceCountMap)
				{
					cprie = (ComponentProxyReferenceInfoEntry)componentProxyReferenceCountMap.get(curl);
					if (cprie != null)
					{
						// check if unavailable or accept in disconnected state???!
						if (cprie.isUnavailable())
						{
							RemoteException re = new RemoteException(this, "Component '" + curl + "' is unavailable.");
							re.caughtIn(this, "internalConnect");
							re.putValue("c", c);
							re.putValue("curl", curl);
							throw re;
						}

						// increment ref. count and take existing proxy
						cprie.incrementReferenceCount(c);
						proxy = cprie.getComponentProxy();
					}
					else
						connectComponent = true; 
				}
				
				// create a new one 
				if (connectComponent)
				{
				    /**
				     * @todo GCH 2006-10-09
				     *       Here we do not do a mapping of the
				     *       received error trace but we simply
				     *       log a new RemoteException.
				     *       This code should be improved.
				     */
				    org.omg.CORBA.Object component=null;
 				    try
					{
					component = mcie.getManager().get_component(mcie.getClientInfo().h, curl, true);
					}
				    catch(CannotGetComponentEx e)
					{
					RemoteException re = new RemoteException(this, "Manager failed to return component with CURL '" + curl + "'.");
					re.caughtIn(this, "internalConnect");
					re.putValue("c", c);
					re.putValue("curl", curl);
					throw re;
					}
				    catch(ComponentNotAlreadyActivatedEx e)
					{
					RemoteException re = new RemoteException(this, "Manager failed to return component with CURL '" + curl + "'.");
					re.caughtIn(this, "internalConnect");
					re.putValue("c", c);
					re.putValue("curl", curl);
					throw re;
					}
				    catch(ComponentConfigurationNotFoundEx e)
					{
					RemoteException re = new RemoteException(this, "Manager failed to return component with CURL '" + curl + "'.");
					re.caughtIn(this, "internalConnect");
					re.putValue("c", c);
					re.putValue("curl", curl);
					throw re;
					}
	
					// obtain component type
					ComponentInfo[] componentInfo = mcie.getManager().get_component_info(mcie.getClientInfo().h, new int[0], curl, "*", true);
					if (componentInfo == null || componentInfo.length != 1 || componentInfo[0].type == null)
					{
						RemoteException re = new RemoteException(this, "Manager failed to return type of component with CURL '" + curl + "'.");
						re.caughtIn(this, "internalConnect");
						re.putValue("c", c);
						re.putValue("curl", curl);
						re.putValue("componentInfo", componentInfo);
						throw re;
					}
	
					// create proxy
					String compomentProxyClassName = IDtoClassName(componentInfo[0].type, true);
					proxy =	instatiateComponentProxy(curl, component, compomentProxyClassName);
					
					// TODO not prefect code, named lock should be used
					// add or increment (sync. leak) reference count
					synchronized(componentProxyReferenceCountMap)
					{
						cprie = (ComponentProxyReferenceInfoEntry)componentProxyReferenceCountMap.get(curl);

						if (cprie == null)
						{
							// add new entry
							componentProxyReferenceCountMap.put(curl, new ComponentProxyReferenceInfoEntry(proxy, c));
						}
						else
						{
							// increment ref. count and take existing proxy
							cprie.incrementReferenceCount(c);
							proxy = cprie.getComponentProxy();
						}
					}

				}
			} catch (Exception ex)
			{
				RemoteException re = new RemoteException(this, "Failed to obtain component for connectable '" + c + "'.", ex);
				re.caughtIn(this, "internalConnect");
				re.putValue("c", c);
				throw re;
			}
				
			try
			{
				c.initialize(proxy);

				updateRemoteDirectoryLinkable(curl, c);

			} catch (Exception ex)
			{
				RemoteException re = new RemoteException(this, "The connectable '" + c + "' rejects the proxy.", ex);
				re.caughtIn(this, "internalConnect");
				re.putValue("c", c);
				throw re;
			}

			if (isDebug())
				new MessageLogEntry(this, "internalConnect", "Connected to component '" + c.getRemoteInfo() + "'.", Level.CONFIG).dispatch();

		}

	}

	/**
	 * Connect to the Manager.
	 * @param corbaloc	manager CORBA reference.
	 * @return	manager and client info entry.
	 * @throws RemoteException	thrown if failed to connect.
	 */
	private ManagerClientInfoEntry connectToManager(String corbaloc) throws RemoteException {
		
		if (isDebug())
			new MessageLogEntry(this, "connectToManager", "Connecting to the Manager '" + corbaloc + "'...", Level.INFO).dispatch();
		
		// connect to the Manager
		Manager manager = ManagerHelper.narrow(orb.string_to_object(corbaloc));
		
		if (manager == null)
			throw new RemoteException(this, "Failed to connect to the Manager with reference, got 'null' reference.");
			
		if (isDebug())
			new MessageLogEntry(this, "connectToManager", "Connected to the Manager '" + corbaloc + "'.", Level.INFO).dispatch();
		
		//
		// login to the manager
		//
		
		if (isDebug())
			new MessageLogEntry(this, "connectToManager", "Logging to the Manager '" + corbaloc + "'.", Level.INFO).dispatch();
		
		Client client = new Client(corbaloc);
		ClientInfo clientInfo = null;
		try
		    {
		    clientInfo = manager.login(client._this(orb));
		    }
		catch (NoPermissionEx ex)
		    {
		    RemoteException re = new RemoteException(this, "No permission to login to the Manager '" + corbaloc + "'.");
			re.caughtIn(this, "connectToManager");
			throw re;
		    }

		if (clientInfo == null || clientInfo.h == 0)
		{
			RemoteException re = new RemoteException(this, "Failed to login to the Manager '" + corbaloc + "'.");
			re.caughtIn(this, "connectToManager");
			throw re;
		}
		
		// create info
		ManagerClientInfoEntry mcie = new ManagerClientInfoEntry(manager, clientInfo);
		
		// update remote directory
		updateRemoteDirectory(corbaloc, mcie);
		
		// return info
		return mcie;
	}

	/**
	 * Queries the default manager and updates remote directory.
	 */
	public void updateRemoteDirectory()
	{

		String corbaloc = defaultManagerReference;
		try
		{
			ManagerClientInfoEntry mcie = null;
						
			synchronized(managerCache)
			{
				mcie = (ManagerClientInfoEntry)managerCache.get(corbaloc);
			}
				
			if (mcie==null)
			{
				if (isDebug())
					new MessageLogEntry(this, "updateRemoteDirectory", "Failed to update remote directory, no default Manager '" + corbaloc + "' logged in.", Level.INFO).dispatch();
			}
			else
				updateRemoteDirectory(corbaloc, mcie);

		} catch (Throwable t)
		{
			RemoteException re = new RemoteException(this, "Failed to update remote directory.", t);
			re.putValue("corbaloc", corbaloc);
			re.caughtIn(this, "initialManagerConnect");
		}

	}

	/**
	 * Queries manager for all components and updates remote directory.
	 * @param corbaloc	manager CORBA reference.
	 * @param mcie		manager remote info.
	 */
	private void updateRemoteDirectory(String corbaloc,	ManagerClientInfoEntry mcie)
	{
		// TODO removal 
		
		if (isDebug())
			new MessageLogEntry(this, "updateRemoteDirectory", "Updating remote directory...", Level.FINE).dispatch();

		try
		{
			int newComponents = 0;
			int updatedComponents = 0;
			
			// get all components
			ComponentInfo[] componentInfo = mcie.getManager().get_component_info(mcie.getClientInfo().h, new int[0], "*", "*", false);
			if (componentInfo == null)
			{
				RemoteException re = new RemoteException(this, "Failed to query manager '" + corbaloc + "' for components info, not updating leaving remote directory...");
				re.caughtIn(this, "updateRemoteDirectory");
				re.putValue("corbaloc", corbaloc);
				re.putValue("componentInfo", componentInfo);
				throw re;
			}
		
			if (isDebug())
				new MessageLogEntry(this, "updateRemoteDirectory", "Manager returned " + componentInfo.length + "components.", Level.FINE).dispatch();

			// iterate through the info
			for (int i = 0; i < componentInfo.length; i++)
			{
				// protect for loop from exceptions
				String compomentClassName = null;
				try
				{
					compomentClassName = IDtoClassName(componentInfo[i].type, false);
					Class compomentClass = Class.forName(compomentClassName);

					// TODO resolve authority!!! be careful for default?!
					
					// check for existing
					abeans.models.acs.baci.ComponentDescriptor existing =
						(abeans.models.acs.baci.ComponentDescriptor)directory.getContext().lookup(componentInfo[i].name);
						
					if (existing != null && existing.getType() == compomentClass)
						// no update needed
						continue;
					
					abeans.models.acs.baci.ComponentDescriptor cd =
						new abeans.models.acs.baci.ComponentDescriptor(componentInfo[i].name, compomentClass);
					
					if (existing == null)
					{
						newComponents++;
						directory.getContext().bind(componentInfo[i].name, cd);
					}
					else
					{
						updatedComponents++;
						directory.getContext().rebind(componentInfo[i].name, cd);
					}
				}
				catch (ClassNotFoundException cnfe)
				{
					// noop
				}
				catch (Throwable t)
				{
					AssertionFailed af = new AssertionFailed(this, "Failed to update query manager '" + corbaloc + "' for component '" + compomentClassName + "' info, not updating leaving remote directory...", t);
					af.caughtIn(this, "updateRemoteDirectory");
					af.putValue("corbaloc", corbaloc);
					af.putValue("cobInfo[i]", componentInfo[i]);
					af.putValue("compomentClassName", compomentClassName);
					new ExceptionIgnorer(af);
				}
			}

			if (isDebug())
				new MessageLogEntry(this, "updateRemoteDirectory", "Remote directory updated, " + newComponents +" new component(s) added and " + updatedComponents + " component(s) updated.", Level.INFO).dispatch();
			
		}
		catch (Throwable t)
		{
			AssertionFailed af = new AssertionFailed(this, "Failed to update remote directory by manager '" + corbaloc + "'...", t);
			af.caughtIn(this, "updateRemoteDirectory");
			af.putValue("corbaloc", corbaloc);
		}
	}

	/**
	 * Updates remote directory for <code>Linkable</code> instances of <code>Connectable</code> instance.
	 * @param connectableName	name of connectable instance.
	 * @param c					connectable instance to be updated.
	 */
	private void updateRemoteDirectoryLinkable(String connectableName, Connectable c)
	{
		try
		{
			// query all connectable children
			Node[] ch = c.getChildren();
			for (int i = 0; i < ch.length; i++)
			
				// check for linkables
				if (ch[i] instanceof TypelessProperty)
				{
					TypelessProperty tp = (TypelessProperty)ch[i];
					String linkableName = connectableName + "/" + tp.getIdentifier().getName(); 
					
					// check for existing
					TypelessPropertyDescriptor existing =
						(TypelessPropertyDescriptor)directory.getContext().lookup(linkableName);
						
					if (existing != null && existing.getType() == ch[i].getClass())
						// no update needed
						continue;

					TypelessPropertyDescriptor td = new TypelessPropertyDescriptor(
																tp.getIdentifier().getName(),
																ch[i].getClass(),
																tp.getAccessTypes(),
																new Class[] { tp.getDataType() },
																tp.isSettable());
					
					if (existing == null)
						directory.getContext().bind(linkableName, td);
					else
						directory.getContext().rebind(linkableName, td);

				}
					
		}		
		catch (Throwable t)
		{
			AssertionFailed af = new AssertionFailed(this, "Failed to update remote directory for for linkable instances of connectable instance '" + connectableName + "'...", t);
			af.caughtIn(this, "updateRemoteDirectoryLinkable");
			af.putValue("connectableName", connectableName);
			af.putValue("c", c);
		}

	}

	/**
	 * Reconnect to the default manager.
	 */
	public synchronized void managerConnect(String managerReference)
	{
		// invalid reference check
		if (managerReference == null)
			return;
			
		managerReference = managerReference.trim();
		if (managerReference.length() == 0)
			return;
			
		if (isConnected())
		{
			// noop check
			if (defaultManagerReference.equals(managerReference))
				return;
				
			// disconnnect first
			managerDisconnect();
		}
		
		initialManagerConnectDone = false;
		defaultManagerReference = managerReference;
		initialManagerConnect();		
	}

	/**
	 * Disconnect from to the default manager.
	 */
	public synchronized void managerDisconnect()
	{
		// noop check
		if (!isConnected())
			return;
			
		managerDisconnect(defaultManagerReference);
		initialManagerConnectDone = false;
	}

	/**
	 * Return connection status, i.e. is connected to the default manager.
	 * @return connection status, i.e. is connected to the default manager.
	 */
	public boolean isConnected()
	{
		return !isDestroyed() && managerCache.containsKey(defaultManagerReference);
	}

	/**
	 * Connect to the default manager.
	 */
	private synchronized void initialManagerConnect()
	{
		if (initialManagerConnectDone)
			return;
		
		initialManagerConnectDone = true;
		
		String corbaloc = defaultManagerReference;
		try
		{
			ManagerClientInfoEntry mcie = null;
						
			synchronized(managerCache)
			{
				mcie = (ManagerClientInfoEntry)managerCache.get(corbaloc);
			}
				
			if (mcie==null)
			{
					
				// connect
				mcie = connectToManager(corbaloc);

				// cache login - manager and clientInfo
				synchronized(managerCache)
				{
					managerCache.put(corbaloc, mcie);
				}
					
				if (isDebug())
					new MessageLogEntry(this, "initialManagerConnect", "Logged in to the default Manager '" + corbaloc + "'.", Level.INFO).dispatch();

				// set NS to new RemoteLoggingService
				/**
				 * @todo GCH 2006-10-09 Here we should catch the
				 *       specific exceptions of get_service and 
				 *       print trace logs of ACS exceptions.
				 */
				try
				{
					RemoteLoggingService remote = (RemoteLoggingService)Root.getComponentManager().getComponent(RemoteLoggingService.class);
					if (remote != null)
					{ 
						// query manager
						org.omg.CORBA.Object obj = mcie.getManager().get_service(0, "NameService", true);
						if (obj != null)
							remote.setCORBARemoteDirectory(NamingContextHelper.narrow(obj));
					}
					
				}
				catch (Throwable t)
				{
					RemoteException re = new RemoteException(this, "Failed to get 'NameService' reference from the default Manager '" + corbaloc + "'.", t);
					re.caughtIn(this, "initialManagerConnect");
				}

			}

		} catch (Throwable t)
		{
			if (isDebug())
				new MessageLogEntry(this, "initialManagerConnect", "Failed to login to the default Manager '" + corbaloc + "'.", Level.INFO).dispatch();

			RemoteException re = new RemoteException(this, "Failed to login to the default Manager '" + corbaloc + "'.", t);
			re.caughtIn(this, "initialManagerConnect");
		}
	}


	/**
	 * Performs the connect of the specified database connectable.
	 * 
	 * @param	c				the database connectable to connect, non-<code>null</code>
	 * @throws	RemoteException	if the connection fails
	 * @see 	abeans.pluggable.Plug#internalConnect(Connectable)
	 */
	private void internalDatabaseConnect(Connectable c) throws RemoteException
	{

		// connect to the default manager to fill remote directory
		initialManagerConnect();

		try
		{
			c.initialize(db);
		} catch (RemoteException cre)
		{
			RemoteException re = new RemoteException(this, "The connectable '" + c + "' rejects the database proxy.", cre);
			re.caughtIn(this, "internalDatabaseConnect"); 
			re.putValue("c", c);
			throw re;
		}
	}

	/**
	 * Sets the proxy of the connectable to <code>null</code>
	 * 
	 * @param	c				the connectable to disconnect, non-<code>null</code>
	 * @throws	RemoteException	never thrown explicitly by this method
	 * @see 	abeans.pluggable.Plug#internalDisconnect(Connectable)
	 */
	public void internalDisconnect(Connectable c) throws RemoteException
	{

		assert (db != null);
		assert (c != null);
		
		if (c.getRemoteInfo() == null)
		{
			RemoteException re = new RemoteException(this, "Connectable has a null remote info.");
			re.caughtIn(this, "internalDisconnect"); 
			re.putValue("c", c);
			throw re;
		}
		
		if (DatabaseProxy.DATABASE_NAME.equals(c.getRemoteInfo().getName()))
		{
			internalDatabaseDisconnect(c);
		}
		else
		{
			// remember current remoteInfo, because just after disconnection it can be changed...
			String curl = c.getRemoteInfo().toURI().getPath().substring(1);

			// acknowledge disconnection
			c.initialize(null);

			releaseConnectable(c, curl, false);
		}
	}

	/**
	 * Releases <code>Connectable</code> (decrements its reference count and if necessary calls release_component on Manager). 
	 * @param c
	 * @param curl
	 * @throws RemoteException
	 */
	private void releaseConnectable(Connectable c, String curl, boolean ignoreUnavailabeFlag) throws RemoteException {
		// release the component
		ManagerClientInfoEntry mcie = null;
		String corbaloc = defaultManagerReference;
			
		try
		{
			String authority = c.getRemoteInfo().getAuthority();
			if (authority != null && authority.length() > 0)
			{
				StringBuffer tmp = new StringBuffer("corbaloc::");
				tmp.append(authority);
				tmp.append("/Manager");
				corbaloc = tmp.toString();
			}

			synchronized(managerCache)
			{
				mcie = (ManagerClientInfoEntry)managerCache.get(corbaloc);
			}
			
			if (mcie==null)
			{
				RemoteException re = new RemoteException(this, "Failed to obtain the manager '" + corbaloc + "' for connectable '" + c + "'.");
				re.caughtIn(this, "internalDisconnect");
				re.putValue("c", c);
				re.putValue("corbaloc", corbaloc);
				throw re;
			}
			else
			{

				boolean release = false;
				
				// decrement reference count
				synchronized(componentProxyReferenceCountMap)
				{
					ComponentProxyReferenceInfoEntry cprie = (ComponentProxyReferenceInfoEntry)componentProxyReferenceCountMap.get(curl);

					// do not remove reference, if disconnect is initiated by Client.component_unavailable
					// we need this reference to automatically reconnect
					if (ignoreUnavailabeFlag || !cprie.isUnavailable())
					{	
						unavailableConnectables.remove(c);

						if (cprie.decrementReferenceCount(c) == 0)
						{
							release = true;
							componentProxyReferenceCountMap.remove(curl);
						}
					}
				}
				
				if (release)
				{
					mcie.getManager().release_component(mcie.getClientInfo().h, curl);
				}

			}

		} catch (Exception ex)
		{
			RemoteException re = new RemoteException(this, "Failed to release component '" + c + "'.", ex);
			re.caughtIn(this, "internalDisconnect");
			re.putValue("c", c);
			throw re;
		}
	}

	/**
	 * Sets the proxy of the database connectable to <code>null</code>
	 * 
	 * @param	c				the connectable to disconnect, non-<code>null</code>
	 * @throws	RemoteException	never thrown explicitly by this method
	 * @see 	abeans.pluggable.Plug#internalDisconnect(Connectable)
	 */
	private void internalDatabaseDisconnect(Connectable c) throws RemoteException
	{
		try
		{
			c.initialize(null);
		} catch (RemoteException cre) {
			RemoteException re = new RemoteException(this, "The connectable '" + c + "' rejects null the database proxy.", cre);
			re.caughtIn(this, "internalDatabaseDisconnect"); 
			re.putValue("c", c);
			throw re;
		}
	}

	/**
	 * Delegates to the database proxy implementation.
	 * Install CORBA Service (DefaultCORBAService).
	 * 
	 * @see abeans.pluggable.Plug#resumeInternal()
	 */
	protected void resumeInternal()
	{
		// install CORBA service
		try
		{
			// plug or root component manager; I decided for root, since CORBAService is used application-wide
			//if (getServiceManager().getComponent(CORBAService.class) == null) getServiceManager().installComponent(DefaultCORBAService.class);
			//orb = ((DefaultCORBAService)getServiceManager().getComponent(CORBAService.class)).getORB();
			
			if (Root.getComponentManager().getComponent(CORBAService.class) == null) Root.getComponentManager().installComponent(DefaultCORBAService.class);
			orb = ((DefaultCORBAService)Root.getComponentManager().getComponent(CORBAService.class)).getORB();
			
			if (orb == null)
				throw new AssertionFailed(this, "CORBA Service returned 'null' ORB.");

			assert(orb != null);
		} catch (UnableToInstallComponentException utice)
		{
			utice.caughtIn(this, "resumeInternal");
			AssertionFailed af = new AssertionFailed(this, "Cannot install service 'DefaultCORBAService', aborting plug installation...", utice);
			af.putValue("getParent()", getParent());
			af.putValue("db", db);
			throw af;
		}
		
		// install remote directory
		try
		{
			directory = (RemoteDirectory)getServiceManager().getComponent(ACSRemoteDirectory.class);
			if (getServiceManager().getComponent(ACSRemoteDirectory.class) == null)
			{
				getServiceManager().installComponent(ACSRemoteDirectory.class);
				directory = (RemoteDirectory)getServiceManager().getComponent(ACSRemoteDirectory.class);
			}
		} catch (UnableToInstallComponentException utice)
		{
			utice.caughtIn(this, "resumeInternal");
			AssertionFailed af = new AssertionFailed(this, "Cannot install distributed service 'ACSRemoteDirectory', aborting plug installation...", utice);
			af.putValue("getParent()", getParent());
			af.putValue("db", db);
			throw af;
		}

		if (db != null) db.resume();
	}

	/**
	 * Delegates to the database proxy implementation.
	 * 
	 * @see abeans.pluggable.Plug#suspendInternal()
	 */
	protected void suspendInternal()
	{
		if (db != null) db.suspend();
	}

	/**
	 * Creates the remote info by calculating the plug prefix (scheme) and the global namespace
	 * if present.
	 * 
	 * @param	authority	the naming authority that will be used when resolving the remote info,
	 * 						can be <code>null</code>, in which case the plug will provide default
	 * 						authority
	 * @param	abeanName	the nameof the abean, non-<code>null</code>; this is the name stripped
	 * 						of the plug prefix and the namespace
	 * @return				a new remote info instance
	 * @see 				abeans.core.Plug#createRemoteInfo(String)
	 */
	public RemoteInfo createRemoteInfo(String authority, String abeanName)
	{
		assert (abeanName != null);
		
		if (isDestroyed()) 
		{
			AssertionFailed af = new AssertionFailed(this, "Cannot issue new instances of 'RemoteInfo' because the plug has been destroyed.");
			af.putValue("abeanName", abeanName);
			throw af;
		}

		try {
			return new ACSRemoteInfo(abeanName, authority, getName());
		}
		catch (NamingException ne) {
			AssertionFailed af = new AssertionFailed(this, "Naming exception caught.", ne);
			af.putValue("abeanName", abeanName);
			throw af;
		}
	}

	/**
	 * Returns the component descriptor for this class.
	 * 
	 * @return	the component descriptor for this
	 * @see	abeans.core.Component#getComponentDescriptor()
	 */
	public ComponentDescriptor getComponentDescriptor()
	{
		return descriptor;
	}

	/**
	 * Initializes this plug by creating a new instance of the database proxy implementation.
	 * 
	 * @param	manager		manager that will contain this plug (plug layer), non-<code>null</code>
	 * @param	state		<code>null</code>
	 * @param	cdesc		<code>null</code>
	 * @throws	IllegalComponentStateException
	 * 						not thrown explicitly by this method
	 * @throws	ComponentInitializationException
	 * 						if the state transfer is attempted
	 * @see 	abeans.core.Component#initialize(ComponentManager, Object, ComponentDescriptor)
	 */
	public void initialize(ComponentManager manager, java.lang.Object state, ComponentDescriptor cdesc)
		throws IllegalComponentStateException, ComponentInitializationException
	{
		assert (manager != null);
		if (state != null || cdesc != null)
			throw new ComponentInitializationException(this, "This component does not support state transfers.");
		
		setParent(manager);
		
		try
		{
			db = new DatabaseProxyImpl(this);
		} catch (RemoteException re)
		{
			ComponentInitializationException cie = new ComponentInitializationException(this, "Cannot construct an instance of 'DatabaseProxyImpl'.", re);
			cie.putValue("manager", manager);
			throw cie;
		}
		
		if (isDebug())
			new MessageLogEntry(this, "initialize", "Pluggable engine for database created.", Level.FINER).dispatch();
	}

	/**
	 * Returns the descriptors for this plug. 
	 * 
	 * @return an array of configuration descriptions
	 * @see abeans.core.defaults.Configurable#getConfigurationDescriptions()
	 */
	public String[][] getConfigurationDescriptions()
	{
		String[][] retVal = new String[2][2];
		retVal[0][0] = DEFAULT_MANAGER_REFERENCE;
		retVal[0][1] = "Manager CORBA reference (corbaloc).";
		retVal[1][0] = DEBUG;
		retVal[1][1] = "Can take values 'true' or 'false'. If 'true', plug will print out additional debug information.";
		return retVal;
	}

	/**
	 * Returns name of the plug, the configuration name of this plug.
	 * 
	 * @return configuration name
	 * @see 	abeans.core.defaults.Configurable#getConfigurationName()
	 */
	public String getConfigurationName()
	{
		return ACS_RESOURCE_LOC;
	}

	/**
	 * Interprets the configuration delivered by Abeans configuration service.
	 * 
	 * @param 	prop		the configuration, if <code>null</code>, the method returns NOP
	 * @throws InitializationException 
	 * 						when the configuration cannot be interpreted
	 * @see				abeans.core.defaults.Configurable#setConfiguration(AbeansProperties)
	 */
	public void setConfiguration(AbeansProperties props) throws InitializationException
	{
		if (props == null) return;
		plugConfig = props;

		// set default manager reference
		defaultManagerReference = props.getProperty(DEFAULT_MANAGER_REFERENCE, DEFAULT_REFERENCE);

		// system property overrides default configuration
		defaultManagerReference = System.getProperty(DEFAULT_MANAGER_REFERENCE, defaultManagerReference);

		// check ACS.manager property, this overrides all
		defaultManagerReference = System.getProperty("ACS.manager", defaultManagerReference);

		// debug && configure databaseProxy
		boolean debug = Boolean.valueOf(props.getProperty(DEBUG, "false")).booleanValue(); 
		setDebug(debug);
		db.setDebug(debug);
	}

	/**
	 * Disconnect the manager. 
	 * @param corbaloc	reference of the manager to be disconnected.
	 */
	private void managerDisconnect(String corbaloc)
	{
		synchronized (managerCache)
		{
			ManagerClientInfoEntry mcie = (ManagerClientInfoEntry)managerCache.get(corbaloc);
			if (mcie == null)
				return;
				
			try
			{
				if (isDebug())
					new MessageLogEntry(this, "managerDisconnect", "Logging out from the Manager '" + corbaloc + "'...", Level.INFO).dispatch();
	
				// TODO release all managers components ?!!!
				// (now it can get funny if default manager is changed)
	
				// this will also release all the components
				mcie.getManager().logout(mcie.getClientInfo().h);
				managerCache.remove(corbaloc);
						
				if (isDebug())
					new MessageLogEntry(this, "managerDisconnect", "Logged out from the Manager '" + corbaloc + "'.", Level.INFO).dispatch();
			}
			catch (Throwable t)
			{
				if (isDebug())
					new MessageLogEntry(this, "managerDisconnect", "Failed to log out from the Manager '" + corbaloc + "'.", t, Level.INFO).dispatch();
			}
		}
	}

	/**
	 * @see abeans.core.Node#destroy()
	 */
	public void destroy() {
		super.destroy();

		// wait until all connectables are destroyed,
		// but wait only up to WAIT_TIME seconds
		final int WAIT_TIME = 5;	// 5s
		for (int waited = 0;
			 waited < WAIT_TIME && componentProxyReferenceCountMap.size() > 0;
			 waited++)
		{
			// wait for a second
		 	try	{ Thread.sleep(1000); }	catch (InterruptedException ie) {}
		}
		
		if (isDebug() && componentProxyReferenceCountMap.size() > 0)
			new MessageLogEntry(this, "destroy", componentProxyReferenceCountMap.size() + " components left activated.", Level.FINE).dispatch();

		// logout from all the managers (non should be added now)
		synchronized (managerCache)
		{
			Iterator i = managerCache.keySet().iterator();
			while (i.hasNext())
			{
				String corbaloc = (String)i.next();
				
				// iterators allow elements to be removed during iteration
				managerDisconnect(corbaloc);
			}
			
			// clear cache
			managerCache.clear();
		}
	}

	/**
	 * Converts CORBA ID to Abeans R3 model proxy class name.
	 * Example: IDL:cosylab.com/ACS/PowerSupply:1.0 -> com.cosylab.ACS.abeans.proxy.PowerSupplyProxy
	 * 
	 * @param id			CORBA ID to be converted to proxy class name.
	 * @param proxyClass	set to <code>true</code>, if you want to get proxy class name
	 * @return		Abeans R3 model proxy class name.
	 */
	public static String IDtoClassName(String id, boolean proxyClass)
	{
		int index1 = 0;
		int index2 = 0;
		index1 = id.indexOf(':');
		index2 = id.lastIndexOf(':');
		if (index1 == index2)
			throw new IllegalArgumentException("IDL ID '" + id + "' is not well-formed because it contains only one ':' character");
			
		String partial = id.substring(index1 + 1, index2);
		index1 = partial.lastIndexOf('/');
		index2 = partial.indexOf('/');
		if (index1 == -1 || index2 == -1)
			throw new IllegalArgumentException("IDL ID '" + id + "' is not well-formed because it does not contain module separators '/'.");

		// we have pragma prefix
		if (index1 != index2) 
		{
			// reverse order of pragma prefix
			final String delimiter = ".";
			StringTokenizer stringTokenizer = new StringTokenizer(partial.substring(0, index2), delimiter);
			String reversedPragma = stringTokenizer.nextToken();
			while (stringTokenizer.hasMoreTokens())
				reversedPragma = stringTokenizer.nextToken() + delimiter + reversedPragma;
	
			partial = reversedPragma + partial.substring(index2);		
		}

		partial = partial.replace('/', '.');
		
		index2 = partial.lastIndexOf('.');
		
		if (proxyClass)
		{
			if (index2 <= 0)
				return "abeans.proxy" + partial + "Proxy";
			else
				return partial.substring(0, index2+1) + "abeans.proxy." + partial.substring(index2+1) + "Proxy";
		}
		else
		{
			if (index2 <= 0)
				return "abeans" + partial;
			else
				return partial.substring(0, index2+1) + "abeans." + partial.substring(index2+1);
		}
		
		
	}

	/**
	 * Instatiates a component proxy of given class name.
	 * 
	 * @param c							connectable instance.
	 * @param curl						curl of <code>c</code>.
	 * @param component	 				component for which proxy is being created. 
	 * @param compomentProxyClassName	name of the component proxy class.
	 * @return 							requested proxy instance.
	 * @throws AssertionFailed
	 */
	private Proxy instatiateComponentProxy(String curl,
		org.omg.CORBA.Object component, String compomentProxyClassName) throws AssertionFailed
	{
		try
		{
			Class compomentProxyClass = Class.forName(compomentProxyClassName);
			
			Class[] constructorParameters = { org.omg.CORBA.Object.class };
			Constructor constructorMethod = compomentProxyClass.getConstructor(constructorParameters);
			
			if (constructorMethod == null)
			{
				AssertionFailed af = new AssertionFailed(this, "No constructor with 'omg.org.CORBA.Object' parameters found.");
				af.caughtIn(this, "internalConnect");
				af.putValue("compomentProxyClassName", compomentProxyClassName);
				throw af;
			}
			
			java.lang.Object proxyObject = constructorMethod.newInstance(new java.lang.Object[] { component });
			if (proxyObject instanceof Proxy)
				return (Proxy)proxyObject;
			else
			{
				AssertionFailed af = new AssertionFailed(this, "Proxy does not implement 'abeans.pluggable.Proxy' interface.");
				af.caughtIn(this, "internalConnect");
				af.putValue("compomentProxyClassName", compomentProxyClassName);
				af.putValue("proxyObject", proxyObject);
				throw af;
			}
		}
		catch (Exception ex)
		{
			AssertionFailed af = new AssertionFailed(this, "Failed to instantiate proxy for component with CURL: '" + curl + "'.",ex);
			af.caughtIn(this, "internalConnect");
			af.putValue("curl", curl);
			af.putValue("compomentProxyClassName", compomentProxyClassName);
			throw af;
		}
	}

	/**
	 * Return default Manager reference.
	 * @return default Manager reference.
	 */
	public String getDefaultManagerReference() {
		return defaultManagerReference;
	}

	/**
	 * Set default Manager reference.
	 * @param reference new default Manager reference.
	 */
	public void setDefaultManagerReference(String reference) {
		defaultManagerReference = reference;
	}

}
