package alma.test.corbareftest;

import java.util.HashSet;
import java.util.logging.Logger;

import org.omg.CORBA.ORB;
import org.omg.CORBA.SystemException;

import si.ijs.maci.Administrator;
import si.ijs.maci.AdministratorPOA;
import si.ijs.maci.AuthenticationData;
import si.ijs.maci.ClientInfo;
import si.ijs.maci.ClientType;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ContainerInfo;
import si.ijs.maci.ImplLangType;
import si.ijs.maci.Manager;
import si.ijs.maci.ManagerHelper;
import alma.acs.logging.AcsLogLevel;
import alma.maciErrType.NoPermissionEx;

public class AdministratorClient extends AdministratorPOA {
	
	// The manager
	private Manager managerRef=null;
	
	// The manager loc
	private String managerLoc=null;
	
	// The ORB
	private ORB orb=null;
	
	// The logger
	private Logger logger=null;
	
	// The ClientInfo struct for this administrator
	private ClientInfo administratorInfo;
	
	// The listener of login-logout events
	private AdministratorListener listener=null;
	
	// Signal to interrupt the retrieve of info in case the manager is busy
	private volatile boolean interrupted;
	
	// Start time
	private long startTime = System.currentTimeMillis();
	
	// Execution Id.
	private long executionId = 0;
	
	/**
	 * Constructor 
	 * 
	 * @param theOrb The ORB
	 * @param theLogger The Logger
	 */
	public AdministratorClient(ORB theOrb, Logger theLogger) {
		if (theOrb==null) {
			throw new IllegalArgumentException("Invalid null ORB");
		}
		orb=theOrb;
		if (theLogger==null) {
			throw new IllegalArgumentException("Invalid null lOGGER");
		}
		logger=theLogger;
	}

	public void client_logged_in(ClientInfo info, long timeStamp, long executionId) {
		if (listener!=null) {
			listener.clientLoggedIn(info);
		}
	}

	public void client_logged_out(int h, long timeStamp) {
		if (listener!=null) {
			listener.clientLoggedOut(h);
		}
	}

	public void component_activated(ComponentInfo info, long timeStamp, long executionId) {
		// TODO @todo this method can be used to get activation event, but now this is done via components_requested
	}

	public void component_deactivated(int h, long timeStamp) {
		// TODO @todo this method can be used to get deactivation event, but now this is done via components_released
	}

	public void components_released(int[] clients, int[] components, long timeStamp) {
		// This method notifies the listener only if the component has been 
		// unloaded.
		// If it is the case, then the component is not found in the list
		// of all the available components.
		if (listener==null) {
			return;
		}
		HashSet<Integer> compSet = new HashSet<Integer>();
		for (int i=0; i<components.length; i++) {
				compSet.add(components[i]);
		}
		for (Integer handle: compSet) {
			try {
				ComponentInfo info = getComponentInfo(handle);
				if (info==null) {
					listener.componentLoggedOut(handle);
				} else {
					listener.componentReleased(info);
				}
			} catch (Exception e) {}
		}
	}

	public void components_requested(int[] clients, int[] components, long timeStamp) {
		if (listener==null) {
			return;
		}
		HashSet<Integer> compSet = new HashSet<Integer>();
		for (int i=0; i<components.length; i++) {
				compSet.add(components[i]);
		}
		for (Integer handle: compSet) {
			try {
				ComponentInfo info = getComponentInfo(handle);
				if (info!=null) {
					listener.componentLoggedIn(info);
				}
			} catch (Exception e) {}
		}
	}

	public void container_logged_in(ContainerInfo info, long timeStamp, long executionId) {
		if (listener!=null) {
			listener.containerLoggedIn(info);
		}
	}

	public void container_logged_out(int h, long timeStamp) {
		if (listener!=null) {
			listener.containerLoggedOut(h);
		}
	}

	public AuthenticationData authenticate(long executionId, String question) {
		if (this.executionId == 0)
			this.executionId = executionId;
		return new AuthenticationData("", ClientType.ADMINISTRATOR_TYPE, ImplLangType.JAVA, true, startTime, this.executionId);
	}

	public void components_available(ComponentInfo[] components) {
		if (listener==null) {
			return;
		}
	}

	public void components_unavailable(String[] component_names) {
		if (listener==null) {
			return;
		}
	}

	public void disconnect() {
		if (administratorInfo==null) {
			// Already disconnected
			return;
		}
		try {
			int hhhh = administratorInfo.h;
			if (managerRef!=null) {
				managerRef.logout(hhhh);
			}
		} catch (Exception exc) {
			logger.log(AcsLogLevel.ERROR,"Couldn't log out from manager: "+exc.getMessage());
			exc.printStackTrace(System.err);
		} finally {
			administratorInfo=null;
			managerRef=null;
			managerLoc=null;
		}
	}

	public void message(short type, String message) {
		System.out.println("Message received "+message);

	}

	public void taggedmessage(short type, short tag, String message) {
		System.out.println("Tagged Message received "+message);

	}

	public String name() {
		return "AdministratorClient";
	}

	public boolean ping() {
		// TODO Auto-generated method stub
		return true;
	}
	
	/**
	 * Connect to the manager 
	 */
	public void connectToManager () throws Exception {
		managerLoc = System.getProperty("ACS.manager");
		if (managerLoc==null || managerLoc.length()==0) {
			throw new Exception("Error getting the manager loc: "+managerLoc);
		}
    	managerLoc=managerLoc.trim();
    	
		org.omg.CORBA.Object object = orb.string_to_object(managerLoc);
		if (object==null) {
			throw new Exception("Error getting the manager "+managerLoc);
		}
		
		managerRef = ManagerHelper.narrow(object);
		if (managerRef == null) {
			throw new Exception("Error narrowing the " + managerLoc);
		}

		Administrator adminIF = _this(orb); // <-- throws "port occupied"
		administratorInfo = managerRef.login(adminIF);
		logger.info("Connected to manager (name="+administratorInfo.name+", handle="+administratorInfo.h+")");
	}
	
	/**
	 * Getter method.
	 * 
	 * @return The manager loc
	 */
	public String getManagerLoc() {
		return managerLoc;
	}
	
	/**
	 * The containerHandles argument cannot be specified here. Reason: There's apparently a
	 * bug in Manager.get_activator_info() in ACS2.x (nothing known about ACS3.x): The
	 * containerHandles argument is not evaluated properly, instead empty (therefore
	 * useless) ContainerInfos are returned.
	 * 
	 * @param name_wildcard not verified to work as expected, recommended to use '*'.
	 * @return
	 * @throws NoPermissionEx
	 * @throws NotConnectedToManagerException
	 * @throws SystemException
	 */
	public ContainerInfo[] retrieveContainerInfo (String name_wildcard) throws Exception {

		int[] container_handles = new int[]{}; // bug workaround, see above
		return managerRef.get_container_info(administratorInfo.h, container_handles, name_wildcard);
	}

	/**
	 * @throws NoPermissionEx 
	 * @throws NotConnectedToManagerException
	 * @throws SystemException
	 */
	public ClientInfo[] retrieveClientInfo (String name_wildcard) throws Exception {
		int[] client_handles = new int[]{};
		ClientInfo[] ret=null;
		boolean done=false;
		int attempt=0;
		interrupted=false;
		return ret;
	}

	/**
	 * @throws NoPermissionEx 
	 * @throws NotConnectedToManagerException
	 * @throws SystemException
	 */
	public ComponentInfo[] retrieveComponentInfo (String name_wildcard) throws Exception {
		int[] component_handles = new int[]{};
		String type_wildcard = "*";
		return managerRef.get_component_info(administratorInfo.h, component_handles, name_wildcard, type_wildcard, true);
	}
	
	
	/**
	 * Set the listener for login/logout events
	 * generated by components/containers/clients
	 * 
	 * @param l The listener (can be <code>null</code>)
	 * @see <code>AdministratorListener</code>
	 */
	public void addLogListener(AdministratorListener l) {
		listener=l;
	}
	
	/**
	 * Get the ComponentInfo of the component with the given name
	 * 
	 * @param name The name of the component
	 * @return The info for the component with the given name
	 *         null if the component with the given name does not exist
	 */
	private ComponentInfo getComponentInfo(String name) throws Exception{
		ComponentInfo[] infos = retrieveComponentInfo("*");
		for (int t=0; t<infos.length; t++) {
			if (infos[t].name.equals(name)) {
				return infos[t];
			}
		}
		return null;
	}
	
	/**
	 * Get the ComponentInfo of the component with the given handle
	 * 
	 * @param handle The handle of the component
	 * @return The info for the component with the given handle
	 *         null if the component with the given name does not exist
	 */
	private ComponentInfo getComponentInfo(int handle) throws Exception{
		ComponentInfo[] infos = retrieveComponentInfo("*");
		for (int t=0; t<infos.length; t++) {
			if (infos[t].h==handle) {
				return infos[t];
			}
		}
		return null;
	}

	/**
	 * Getter method
	 * 
	 * @return The manager reference
	 */
	public Manager getManagerRef() {
		return managerRef;
	}
	
	public void interruptManagerBusy() {
		interrupted=true;
	}
	
}

