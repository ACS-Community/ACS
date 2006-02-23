/*
 * @@COPYRIGHT@@
 */

package abeans.pluggable.acs;

import java.util.Hashtable;
import java.util.logging.Level;
import javax.naming.Context;
import javax.naming.InitialContext;

import org.omg.CORBA.ORB;

import abeans.core.ComponentDescriptor;
import abeans.core.ComponentInitializationException;
import abeans.core.ComponentManager;
import abeans.core.ComponentSupport;
import abeans.core.Identifier;
import abeans.core.IllegalComponentStateException;
import abeans.core.InitializationException;
import abeans.core.Root;
import abeans.core.UnableToInstallComponentException;
import abeans.core.defaults.AbeansProperties;
import abeans.core.defaults.Configurable;
import abeans.core.defaults.ConfigurationService;
import abeans.core.defaults.MessageLogEntry;
import abeans.pluggable.RemoteDirectory;

import alma.acs.util.ACSPorts;

/**
 * This component provides access to the CORBA Naming Service
 * via CosNaming JNDI interface.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class NamingServiceRemoteDirectory
	extends ComponentSupport
	implements RemoteDirectory, Configurable
{

	/**
	 * The constant denoting the name of the configuration resource (file).
	 */
	private static final String NS_RESOURCE_LOC = "NamingServiceRemoteDirectory";

	/**
	 * The constant denoting the value of the configuration key.
	 */
	private static final String CONF_REFERENCE = "reference";

	/**
	 * The constant denoting the default NS reference (if non is set).
	 */
	private static final String DEFAULT_REFERENCE = "iiop://" + ACSPorts.getIP() + ":" + ACSPorts.getNamingServicePort() + "/";

	/**
	 * Properties of the service.
	 */
	private AbeansProperties serviceConfig = null;

	/**
	 * Reference to CORBA Naming Service, use IIOP type reference - 'iiop://<host>[:<port>]'.
	 */
	private String reference = DEFAULT_REFERENCE;

	/**
	 * Root context of the remote directory (CORBA Naming Service).
	 */
	private Context context = null;

	/**
	 * Component description of this plug.
	 */
	private final transient ComponentDescriptor descriptor =
		new ComponentDescriptor(getClass(), RemoteDirectory.class, 1, "CORBA Naming Service Remote Directory", false, true, null);

	/**
	 * Constructor for NamingServiceRemoteDirectory.
	 */
	public NamingServiceRemoteDirectory()
	{
		super("NamingServiceRemoteDirectory", "NamingSvcRemDir", Identifier.PLUG);
	}

	/**
	 * Not implemented (irrelevant).
	 * @see abeans.pluggable.RemoteDirectory#setCacheLifetime(long)
	 */
	public void setCacheLifetime(long milliseconds)
	{
	}

	/**
	 * Not implemented (irrelevant).
	 * @see abeans.pluggable.RemoteDirectory#getCacheLifetime()
	 */
	public long getCacheLifetime()
	{
		return 0;
	}

	/**
	 * Root context of the remote directory (CORBA Naming Service).
	 * @return		root context of the remote directory (CORBA Naming Service)
	 * @see abeans.models.meta.ContextRepresentable#getContext()
	 */
	public Context getContext()
	{
		return context;
	}

	/**
	 * @see abeans.core.Component#getComponentDescriptor()
	 */
	public ComponentDescriptor getComponentDescriptor()
	{
		return descriptor;
	}

	/**
	 * Initializes the component by placing it into the hierarchy.
	 *
	 * @param	manager		the parent of this component, non-<code>null</code>
	 * @param 	state 		must be <code>null</code>
	 * @param 	cdesc 		must be <code>null</code>
	 * @throws IllegalComponentStateException 
	 * 						when the <code>cdesc</code> is not <code>null</code>
	 * @throws ComponentInitializationException 
	 * 						when the manager already contains an authentication service
	 * 						instance
	 * @see abeans.core.Component#initialize(ComponentManager, Object, ComponentDescriptor)
	 */
	public void initialize(
		ComponentManager manager,
		Object state,
		ComponentDescriptor cdesc)
		throws IllegalComponentStateException, ComponentInitializationException
	{
		if (manager == null)
			throw new ComponentInitializationException(this, "Parameter 'manager' passed to initialize() was null.");
			
		if (cdesc != null)
		{
			IllegalComponentStateException icse = new IllegalComponentStateException(this, "Cannot interpret a non-null component state.");
			icse.putValue("state", state);
			icse.putValue("cdesc", cdesc);
			throw icse;
		}
		
		ConfigurationService configurationService = (ConfigurationService)Root.getComponentManager().getComponent(ConfigurationService.class);
		if (configurationService == null) 
		{
			ComponentInitializationException cie = new ComponentInitializationException(this, "There is no Configuration service installed.");
			throw cie;
		}

		AbeansProperties ap = null;
		try
		{
			// load configuration (this has to be done before internalInitialize method is called
			ap = configurationService.getConfiguration(this, getConfigurationName());
			setConfiguration(ap);
		}
		catch (InitializationException ie)
		{
			ComponentInitializationException cie = new ComponentInitializationException(this, "Unable to initialize service.", ie);
			cie.putValue("ap", ap);
			throw cie;
		}

		// try to install Default CORBA Service, if none installed yet
		CORBAService corbaService = (CORBAService)Root.getComponentManager().getComponent(CORBAService.class);
		if (corbaService == null) 
		{
			try
			{
				Root.getComponentManager().installComponent(DefaultCORBAService.class);
			}
			catch (UnableToInstallComponentException uice)
			{
				ComponentInitializationException cie = new ComponentInitializationException(this, "Unable to install Default CORBA Service.", uice);
				throw cie;
			}
			corbaService = (CORBAService)Root.getComponentManager().getComponent(CORBAService.class);
		}
		
		if (corbaService == null) 
		{
			ComponentInitializationException cie = new ComponentInitializationException(this, "There is no CORBA Service installed.");
			throw cie;
		}
		
		setParent(manager);
		
		internalInitialize(corbaService);
		
		if (isDebug())
			new MessageLogEntry(this, "initialize", "CORBA Naming Service Remote Directory successfully initialized.", Level.INFO).dispatch();
	}

	/**
	 * Obtains root context of the remote directory (CORBA Naming Service).
	 * @param corbaService		CORBAService object to serve its ORB
	 * @throws ComponentInitializationException
	 */
	private void internalInitialize(CORBAService corbaService) throws ComponentInitializationException
	{
		assert (corbaService != null);
		
		assert (reference != null);
		
		// check for valid ORB
		ORB orb = corbaService.getORB();
		if (orb == null) 
		{
			ComponentInitializationException cie = new ComponentInitializationException(this, "ORB returned from CORBA Service is 'null'.");
			throw cie;
		}
		
		if (isDebug())
			new MessageLogEntry(this, "internalInitialize", "Connecting to CORBA Naming Service with reference '"+reference+"'...", Level.INFO).dispatch();
		
		Hashtable env = new Hashtable();
		// set CosNamingFactory 
		env.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.cosnaming.CNCtxFactory");
		// set NS address
		env.put(Context.PROVIDER_URL, reference);
		// set orb
		env.put("java.naming.corba.orb", orb);

		try
		{
			context = new InitialContext(env);
		}
		catch (Exception ex)
		{
			if (isDebug())
				new MessageLogEntry(this, "internalInitialize", "Failed to connect to CORBA Naming Service with reference '"+reference+"'...", Level.INFO).dispatch();

			ComponentInitializationException cie = new ComponentInitializationException(this, "Failed to create initial context.", ex);
			cie.putValue("Context.PROVIDER_URL", reference);
			throw cie;
		}
		
		if (isDebug())
			new MessageLogEntry(this, "internalInitialize", "Connected to CORBA Naming Service with reference '"+reference+"'.", Level.INFO).dispatch();
	}

	/**
	 * Returns the descriptors for this service. 
	 * 
	 * @return an array of configuration descriptions
	 * @see abeans.core.defaults.Configurable#getConfigurationDescriptions()
	 */
	public String[][] getConfigurationDescriptions()
	{
		String[][] retVal = new String[2][2];
		retVal[0][0] = CONF_REFERENCE;
		retVal[0][1] = "Reference to CORBA Naming Service, use IIOP type reference - 'iiop://<host>[:<port>]'.";
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
		return NS_RESOURCE_LOC;
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
		if (props == null || serviceConfig != null) return;
		serviceConfig = props;

		// set reference
		reference = props.getProperty(CONF_REFERENCE, DEFAULT_REFERENCE);

		// system property overrides default configuration
		reference = System.getProperty(NS_RESOURCE_LOC + "." + CONF_REFERENCE, reference);

		// debug
		boolean debug = Boolean.valueOf(props.getProperty(DEBUG, "false")).booleanValue(); 
		setDebug(debug);
	}

	/**
	 * Returns the reference of the naming service remote directory.
	 * @return String	the reference of the naming service remote directory
	 */
	public String getReference()
	{
		return reference;
	}

}
