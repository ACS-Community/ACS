package alma.COUNTER.CounterTestImpl;

import java.util.Arrays;
import java.util.List;

import junit.framework.Assert;
import junit.framework.AssertionFailedError;

import si.ijs.maci.Administrator;
import si.ijs.maci.AdministratorPOATie;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ContainerInfo;
import si.ijs.maci.LoggingConfigurable;
import si.ijs.maci.LoggingConfigurableHelper;

import alma.JavaContainerError.wrappers.AcsJContainerEx;
import alma.acs.container.AcsManagerProxy;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogger;
import alma.maciErrType.NoPermissionEx;


/**
 * Provides container access to JUnit tests. 
 * Uses JUnit asserts from {@link Assert} which means that runtime exceptions such as 
 * {@link AssertionFailedError} may be thrown.
 * 
 * @author hsommer
 */
public class ContainerTestUtil
{
	private final ContainerServices containerServices;
	private final AcsLogger logger;
	private final AcsManagerProxy managerProxy;
	private AcsManagerProxy adminProxy;

	
	/**
	 * The constructor only stores the parameters.
	 * You must call {@link #loginToManager()} explicitly before using the other methods.
	 * Make sure to call {@link #logoutFromManager()} when done. 
	 */
	public ContainerTestUtil(ContainerServices cs, AcsManagerProxy managerProxy) {
		Assert.assertNotNull(cs);
		Assert.assertNotNull(managerProxy);
		this.containerServices = cs;
		this.logger = cs.getLogger();
		this.managerProxy = managerProxy;
	}	
	
	public void loginToManager() throws AcsJContainerEx {
		if (adminProxy == null) {
	    	AdministratorPOATie adminpoa = new AdministratorPOATie(new ManagerAdminClient(containerServices.getName(), logger)); 
			Administrator adminCorbaObj = adminpoa._this(containerServices.getAdvancedContainerServices().getORB());
			
			adminProxy = managerProxy.createInstance();
			adminProxy.loginToManager(adminCorbaObj, false);
			int adminManagerHandle = adminProxy.getManagerHandle();
			Assert.assertTrue(adminManagerHandle > 0);
		}
	}
	
	public void logoutFromManager() {
		if (adminProxy != null) {
			adminProxy.logoutFromManager();
			adminProxy = null;
		}
	}

	
	/**
	 * Gets a reference to the LoggingConfigurableOperations interface of a container with a given name. 
	 * <p>
	 * Note that only in test code like here we are allowed to talk directly with the manager.
	 * For operational code, the ContainerServices methods must be used and extended if necessary.
	 *  
	 * @param containerName
	 */
	public LoggingConfigurable getContainerLoggingIF(String containerName) throws AcsJContainerEx, NoPermissionEx {
		for (ContainerInfo containerInfo : getAllContainerInfos()) {
			if (containerInfo.name.equals(containerName)) {
				return LoggingConfigurableHelper.narrow(containerInfo.reference);
			}
		}
		Assert.fail("No container '" + containerName + "' found.");
		return null; // to appease the compile which does not see the terminal nature of Assert.fail
	}
	
	/**
	 * Gets the name of the container running (or configured to run) the component of the given name.
	 */
	public String resolveContainerName(String componentName) throws AcsJContainerEx, NoPermissionEx {
		ComponentInfo[] componentInfos = adminProxy.getManager().get_component_info(adminProxy.getManagerHandle(), new int[0], componentName, "*", false);
		Assert.assertNotNull(componentInfos);
		Assert.assertEquals("Exactly one match for component name '" + componentName + "' expected.", 1, componentInfos.length);
		String containerName = componentInfos[0].container_name;
		Assert.assertNotNull(containerName);
		return containerName;
	}

	public List<ContainerInfo> getAllContainerInfos() throws NoPermissionEx, AcsJContainerEx {
		ContainerInfo[] containerInfos = adminProxy.getManager().get_container_info(adminProxy.getManagerHandle(), new int[0], "*");
		return Arrays.asList(containerInfos);
	}

}
