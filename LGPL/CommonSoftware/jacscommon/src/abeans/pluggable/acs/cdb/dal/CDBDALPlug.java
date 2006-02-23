/*
 * @@COPYRIGHT@@
 */

package abeans.pluggable.acs.cdb.dal;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;

import javax.naming.NamingException;

import com.cosylab.CDB.DAL;
import com.cosylab.CDB.DALChangeListener;
import com.cosylab.CDB.DALChangeListenerPOA;
import com.cosylab.CDB.DALHelper;
import com.cosylab.CDB.DAO;
import com.cosylab.util.NameValueList;

import org.omg.CORBA.ORB;

import abeans.core.AssertionFailed;
import abeans.core.ComponentDescriptor;
import abeans.core.ComponentInitializationException;
import abeans.core.ComponentManager;
import abeans.core.IllegalComponentStateException;
import abeans.core.InitializationException;
import abeans.core.Root;
import abeans.core.UnableToInstallComponentException;
import abeans.core.defaults.AbeansProperties;
import abeans.core.defaults.Configurable;
import abeans.core.defaults.MessageLogEntry;
import abeans.models.Connectable;
import abeans.pluggable.DatabaseProxy;
import abeans.pluggable.Plug;
import abeans.pluggable.RemoteException;
import abeans.pluggable.RemoteInfo;
import abeans.pluggable.acs.CORBAService;
import abeans.pluggable.acs.DefaultCORBAService;

import alma.acs.util.ACSPorts;

/**
 * Implementation of the <code>abeans.pluggable.Plug</code> that starts up and
 * shuts down the CDBDAL database and take care for configuration and policy management.
 * The plug is managed automatically by the Abeans framework.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 * @see		Plug
 */
public class CDBDALPlug extends Plug implements Configurable
{

	/**
	 * The constant denoting the name of this plug.
	 */
	public static final String PLUG_NAME = "CDBDAL";
	
	/**
	 * The constant denoting the name of the configuration resource (file).
	 */
	private static final String CDBDAL_RESOURCE_LOC = PLUG_NAME+"Plug";
	
	/**
	 * The constant denoting the prefix of the DAL configuration keys.
	 */
	private static final String CDBDAL_PREFIX = "DAL";

	/**
	 * The constant denoting the prefix of the DAO configuration keys.
	 */
	private static final String CDBDAO_PREFIX = "DAO";

	/**
	 * The constant denoting the name of default DAL reference property key.
	 */
	private static final String CDBDAL_DEFAULT_REFERENCE = CDBDAL_PREFIX + ".defaultReference";

	/**
	 * The constant denoting the default DAL reference (if non is set).
	 */
	private static final String DEFAULT_REFERENCE = "corbaloc::" + ACSPorts.getIP() + ":" + ACSPorts.getCDBPort() + "/CDB";

	/**
	 * The constant denoting the name of default DAO remote switch property key.
	 */
	private static final String CDBDAO_REMOTE = CDBDAO_PREFIX + ".remote";

	/**
	 * Switch to turn remote DAO on.
	 */
	private boolean remoteDAO = true;

	/**
	 * Default DAL CORBA reference (if not authority is specified).
	 */
	private String defaultDAL = null;

	/**
	 * Reference to the database proxy implementation.
	 */
	private DatabaseProxyImpl db = null;

	/**
	 * CORBA del reference cache.
	 */
	private Map dalCache = new NameValueList();

	/**
	 * CORBA Object Request Broker (ORB) reference.
	 */
	private ORB orb = null;

	/**
	 * Listener for CDB change.
	 */
	private ChangeListener changeListener = null;


	/**
	 * This private class will handle CDB restart or data change in the CDB.
	 * 
	 * @author		Dragan Vitas (dragan.vitas@cosylab.com)
	 * @version	@@VERSION@@
	 */
	private class ChangeListener extends DALChangeListenerPOA
	{
		/**
		 * Map of monitored CURLs entities.
		 */
		private HashMap curlMap = new HashMap();
		
		/**
		 * Map of registered listeners on DALs.
		 */
		private HashMap dalMap = new HashMap();
		
		/**
		 * CORBA instance of <code>DALChangeListener</code> object.
		 */
		private DALChangeListener cl = null;

		/**
		 * Reconnect connectable in separate thread to avoid blocking.
		 */
		private class ReconnectTask extends Thread
		{
			/**
			 * Object to be reconnected.
			 */
			Connectable connectable;
			
			/**
			 * Constructor of the class.
			 * 
			 * @param	connectable	object to be reconnected.
			 */
			public ReconnectTask(Connectable connectable)
			{
				this.connectable = connectable;
			}
			
			/**
			 * Thread worker implementation.
			 */
			public void run()
			{
				final int RETRIES = 3;
				
				for (int i = 0; i < RETRIES; i++)
				{
					try
					{
						internalConnect(connectable);
						break;
					}
					catch (RemoteException e)
					{
						// if we cannot reestablish connection the exception will report elsewhere
					}
				}
			}
		}

		/**
		 * Called from DAL server when curl changed or when DAL startups.
		 * NOTE: reconnection does not work if DAL is running on Java (Sun) CORBA - server problem (DAL reference!)
		 * 
		 * @param	curl	changed entity in CDB
		 * @see DALChangeListenerOperations#object_changed(String)
		 */
		public void object_changed(String curl)
		{
			// get the connectable from cache and reconnect it in separate thread
			Connectable connectable = (Connectable) curlMap.get(curl);
			if (connectable != null)
				new ReconnectTask(connectable).start();
		}

		/**
		 * Add listener for curl on DAL server so the connectable object can be reconnected.
		 * 
		 * @param dal the reference of the DAL server where the curl is obtained
		 * @param curl the path for our DAO object
		 * @param conn the Abeans object for wich we made the DAO
		 */
		public void handle(DAL dal, String curl, Connectable conn)
		{
			// create CORBA instance
			if (cl == null)
				cl = changeListener._this(orb);
			
			// register DAL listener, if not already
			Integer listenerID = (Integer) dalMap.get(dal);
			if (listenerID == null)
			{
				int id = dal.add_change_listener(cl);
				listenerID = new Integer(id);
				dalMap.put(dal, listenerID);
			}
			
			// listen for <code>curl</code> changes
			dal.listen_for_changes(curl, listenerID.intValue());

			// after all registrations are done successfully
			// remember that we are monitoring <code>curl</code>
			curlMap.put(curl, conn);
		}

		/**
		 * Checks if object with the given CURL is already registered to this listener.
		 * @param curl the path for our DAO object
		 */
		public boolean isRegistered(String curl)
		{
			return curlMap.containsKey(curl);
		}

		/**
		 * Unregister this listener from DAL server(s).
		 */
		public void destroy()
		{
			DAL dal;
			Integer listenerID;
			
			Iterator iter = dalMap.keySet().iterator();
			while (iter.hasNext())
			{
				dal = (DAL) iter.next();
				listenerID = (Integer) dalMap.get(dal);
				
				// do not bail out if derefistration of one listener fails
				try
				{
					dal.remove_change_listener(listenerID.intValue());
				}
				catch (Exception ex)
				{
					// no-op.
				}
			}
			
			// clear caches
			dalMap.clear();
			curlMap.clear();
		}
	};

	/**
	 * Component description of this plug.
	 */
	private final transient ComponentDescriptor descriptor =
		new ComponentDescriptor(getClass(), Plug.class, 1, "CDBDAL plug for Channel model", false, false, null);

	/**
	 * Creates a new instance of the plug. This constructor is automatically 
	 * invoked by the Abeans framework.
	 */
	public CDBDALPlug()
	{
		super(PLUG_NAME);
	}

	/**
	 * Returns <code>Channel</code> to indicate that this plug uses Channel model.
	 * 
	 * @return <code>Channel</code>
	 * @see abeans.pluggable.Plug#getSupportedLibrary()
	 */
	public String getSupportedLibrary()
	{
		return "Channel";
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
			assert (changeListener != null);
			
			String curl = null;			
			DAOProxy proxy = null;
			DAL dal = null;
			String corbaloc = defaultDAL;
						
			try
			{
				String authority = c.getRemoteInfo().getAuthority();
				if (authority != null && authority.length() > 0)
				{
					StringBuffer tmp = new StringBuffer("corbaloc::");
					tmp.append(authority);
					tmp.append("/CDB");
					corbaloc = tmp.toString();
				}

				synchronized(dalCache)
				{
					dal = (DAL)dalCache.get(corbaloc);
				}
				
				if (dal==null)
				{

					if (isDebug())
						new MessageLogEntry(this, "internalConnect", "Connecting to DAL '" + corbaloc + "'...", Level.INFO).dispatch();
			
					// connect to DAL
					dal = DALHelper.narrow(orb.string_to_object(corbaloc));

					if (dal == null)
						throw new RemoteException(this, "Failed to connect to the DAL object with reference, got 'null' reference.");
						
					synchronized(dalCache)
					{
						dalCache.put(corbaloc, dal);
					}

					if (isDebug())
						new MessageLogEntry(this, "internalConnect", "Connected to DAL '" + corbaloc + "'.", Level.INFO).dispatch();

				}

			} catch (Exception ex)
			{
				RemoteException re = new RemoteException(this, "Failed to connect to the DAL object with reference '" + corbaloc + "' for connectable '" + c + "'.", ex);
				re.putValue("c", c);
				re.caughtIn(this, "internalConnect");
				throw re;
			}

			try
			{

				curl = c.getRemoteInfo().toURI().getPath().substring(1);
				
				if (remoteDAO)
				{
					DAO dao = dal.get_DAO_Servant(curl);
					proxy = new DAOProxy(dal, curl, dao);
				}
				else
				{
					proxy = new DAOLocalProxy(dal, curl);				
				}
				
				// register listener, if not already registered
				if (changeListener != null)
				{
					if (!changeListener.isRegistered(curl))
						changeListener.handle(dal, curl, c);
				}
				
			} catch (Exception ex)
			{
				RemoteException re = new RemoteException(this, "Failed to obtain DAO object for connectable '" + c + "'.", ex);
				re.putValue("c", c);
				re.caughtIn(this, "internalConnect");
				throw re;
			}
				
			try
			{
				c.initialize(proxy);
			} catch (Exception ex)
			{
				RemoteException re = new RemoteException(this, "The connectable '" + c + "' rejects the proxy.", ex);
				re.putValue("c", c);
				re.caughtIn(this, "internalConnect");
				throw re;
			}

			if (isDebug())
				new MessageLogEntry(this, "internalConnect", "Connected to DAO '" + c.getRemoteInfo() + "'.", Level.CONFIG).dispatch();

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
		try
		{
			c.initialize(db);
			changeListener = new ChangeListener();
		} catch (RemoteException cre) {
			RemoteException re = new RemoteException(this, "The connectable '" + c + "' rejects the database proxy.");
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
			c.initialize(null);
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
			if(changeListener != null)
				changeListener.destroy();
		} catch (RemoteException cre) {
			RemoteException re = new RemoteException(this, "The connectable '" + c + "' rejects null database proxy.");
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
		    return new CDBDALRemoteInfo(abeanName, authority, getName());
		} catch (NamingException ne) {
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
	public void initialize(ComponentManager manager, Object state, ComponentDescriptor cdesc)
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
		String[][] retVal = new String[3][2];
		retVal[0][0] = CDBDAL_DEFAULT_REFERENCE;
		retVal[0][1] = "Default CORBA reference to the DAL remote object..";
		retVal[1][0] = CDBDAO_REMOTE;
		retVal[1][1] = "Can take values 'true' or 'false'. If 'true', plug will use remote DAO object, othervise local DAO implementation will be used.";
		retVal[2][0] = DEBUG;
		retVal[2][1] = "Can take values 'true' or 'false'. If 'true', plug will print out additional debug information.";
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
		return CDBDAL_RESOURCE_LOC;
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

		// set default DAL
		defaultDAL = props.getProperty(CDBDAL_DEFAULT_REFERENCE, DEFAULT_REFERENCE);

		// system property overrides default configuration
		defaultDAL = System.getProperty(CDBDAL_DEFAULT_REFERENCE, defaultDAL);

		// set remote DAO
		remoteDAO = Boolean.valueOf(props.getProperty(CDBDAO_REMOTE, "true")).booleanValue();
		
		// debug && configure databaseProxy
		boolean debug = Boolean.valueOf(props.getProperty(DEBUG, "false")).booleanValue(); 
		setDebug(debug);
		db.setDebug(debug);
	}

}
