/*
 * @@COPYRIGHT@@
 */

package abeans.pluggable.acs;

import java.util.Properties;
import java.util.logging.Level;

import org.omg.CORBA.ORB;
import org.omg.PortableServer.POA;
import org.omg.PortableServer.POAHelper;
import org.omg.PortableServer.POAManager;

import abeans.core.ComponentDescriptor;
import abeans.core.ComponentInitializationException;
import abeans.core.ComponentManager;
import abeans.core.ComponentSupport;
import abeans.core.Identifier;
import abeans.core.IllegalComponentStateException;
import abeans.core.InitializationException;
import abeans.core.Root;
import abeans.core.defaults.AbeansProperties;
import abeans.core.defaults.Configurable;
import abeans.core.defaults.ConfigurationService;
import abeans.core.defaults.MessageLogEntry;

/**
 * Class that provides default ACS CORBA service implementation.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class DefaultCORBAService extends ComponentSupport implements CORBAService, Configurable, Runnable
{
	/**
	 * The constant denoting the name of the configuration resource (file).
	 */
	private static final String DCS_RESOURCE_LOC = "DefaultCORBAService";

	/**
	 * Component description of this plug.
	 */
	private final transient ComponentDescriptor descriptor =
		new ComponentDescriptor(getClass(), CORBAService.class, 1, "Default ACS CORBA Service", false, true, null);

	/**
	 * Properties of the service.
	 */
	private AbeansProperties serviceConfig = null;

	/**
	 * Object Request Broker (ORB) object.
	 */
	private ORB orb = null;

	/**
	 * Additional sync check (what if component is destroyed before thread is started).
	 */
	private volatile boolean destroyState = false;

	/**
	 * Root Portable Object Adapter (POA) object.
	 */
	private POA rootPOA = null;
	
	/**
	 * Constructor for DefaultCORBAService.
	 */
	public DefaultCORBAService()
	{
		super("DefaultCORBAService", "DefCORBA", Identifier.PLUG);
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
		
		if (Root.getComponentManager().getComponent(CORBAService.class) != null) 
		{
			ComponentInitializationException cie = new ComponentInitializationException(this, "An CORBA service is already installed.");
			cie.putValue("Root.getComponentManager().getComponent(CORBAService.class)", Root.getComponentManager().getComponent(CORBAService.class));
			throw cie;
		}
		
		//
		// Configuration
		//

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

		setParent(manager);
		
		internalInitialize();
		
		if (isDebug())
			new MessageLogEntry(this, "initialize", "Default CORBA Service successfully initialized.", Level.INFO).dispatch();
	}

	/**
	 * Initializes the CORBA.
	 * @throws ComponentInitializationException
	 */
	private void internalInitialize() throws ComponentInitializationException
	{

		// ORB stanza
		Properties orbprops = System.getProperties();
		
		// add additional properties		
		if (serviceConfig != null)
			orbprops.putAll(serviceConfig);
		
		orb = ORB.init(new String[0], orbprops);
			
		// POA stanza, use rootPOA
		try
		{
			// resolve RootPOA
			rootPOA = POAHelper.narrow(orb.resolve_initial_references("RootPOA"));
			
			// activate POA
			POAManager manager = rootPOA.the_POAManager();
			manager.activate();

			// spawn ORB thread (to process incoming requests)
			new Thread(this, "DefaultCORBAService").start();

		} catch (Exception e)
		{
			ComponentInitializationException ex = new ComponentInitializationException(this, "Failed to initialize CORBA.", e);
			throw ex;
		}
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
		retVal[0][0] = "*";
		retVal[0][1] = "All properties all passed to the ORB (overriding system properties).";
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
		return DCS_RESOURCE_LOC;
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

		// debug 
		boolean debug = Boolean.valueOf(props.getProperty(DEBUG, "false")).booleanValue(); 
		setDebug(debug);
	}

	/**
	 * Overloads the destroy to first perform a ORB shutdown.
	 */
	public void destroy()
	{
		destroyState = true;
		
		// destory ORB
		if (orb != null)
		{
			// possible solution with orb.work_pending
			// but JacORB has no implementation of it
			
			// do not wait for completion
			orb.shutdown(false);
			
			// and finally destroy
			orb.destroy();
			orb = null;
		}

		super.destroy();
	}
	
	/**
	 * Returns Object Request Broker (ORB) object.
	 * 
	 * @return		Object Request Broker (ORB) object
	 * @see 		abeans.pluggable.acs.CORBAService#getORB()
	 */
	public ORB getORB()
	{
		return orb;
	}

	/**
	 * Returns root Portable Object Adapter (POA) object.
	 * 
	 * @return		root Portable Object Adapter (POA) object
	 * @see 		abeans.pluggable.acs.CORBAService#getRootPOA()
	 */
	public POA getRootPOA()
	{
		return rootPOA;
	}

	/**
	 * Main thread to handle CORBA requests.
	 * 
	 * @see java.lang.Runnable#run()
	 */
	public void run()
	{
		ORB localORBRef = orb;
		if (!destroyState && localORBRef != null)
			localORBRef.run();
	}

}
