package alma.test.corbareftest;

import java.util.logging.Logger;

import org.omg.CORBA.ORB;

import si.ijs.maci.ClientInfo;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ContainerInfo;

import alma.CorbaRefTest.HelloWorld;
import alma.CorbaRefTest.HelloWorldHelper;
import alma.acs.component.client.AdvancedComponentClient;
import alma.acs.logging.ClientLogManager;

/**
 * A class to test components references.
 * 
 * It gets the java, python and C++ components and call their CORBA method after 
 * the containers have been restarted.
 * 
 * The checks is done twice. The first one shutting down the container in a normal way
 * and the second one by killing the containers.
 *  
 * @author acaproni
 *
 */
public class TestComponents extends AdvancedComponentClient implements AdministratorListener {
	
	// The administrator client to receives events from
	private AdministratorClient administratorClient;
	
	// Booleans saying if the container logged in
	private boolean pyContainerLogged = false;
	private boolean cppContainerLogged = false;
	private boolean javaContainerLogged = false;
	
	// Container handles
	private int pyContHandle;
	private int cppContHandle;
	private int javaContHandle;
	
	// Booleans saying if the component logged in
	private boolean pyComponentLogged = false;
	private boolean cppComponentLogged = false;
	private boolean javaComponentLogged = false;
	
	// Component handles
	private int pyCompHandle;
	private int cppCompHandle;
	private int javaCompHandle;
	
	// ACS components
	private HelloWorld pyHW=null;
	private HelloWorld cppHW=null;
	private HelloWorld javaHW=null;
	
	/**
	 * Constructor 
	 * 
	 * @param logger The logger
	 * @param managerLoc The manager reference
	 */
	public TestComponents(Logger logger, String managerLoc, String clientName) throws Exception {
		super(logger,managerLoc,clientName);
		ORB orb = getAcsCorba().getORB();
		administratorClient = new AdministratorClient(orb,logger);
		administratorClient.connectToManager();
		administratorClient.addLogListener(this);
	}
	
	public static void main(String[] args) {
		// Connect to ACS
        Logger logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("TestComponents",true);
        String managerLoc = System.getProperty("ACS.manager");
        if (managerLoc == null) {
        	System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
        	System.exit(-1);
        }
		TestComponents testObj;
		try {
			testObj= new TestComponents(logger,managerLoc,"TestComponents");
		} catch (Throwable t) {
			System.err.println("Exception: "+t.getMessage());
			t.printStackTrace(System.err);
			return;
		}
		testObj.test();
	}

	/* (non-Javadoc)
	 * @see alma.test.corbareftest.AdministratorListener#clientLoggedIn(si.ijs.maci.ClientInfo)
	 */
	@Override
	public void clientLoggedIn(ClientInfo clientInfo) {
		System.out.println("Client logged in "+clientInfo.name+", "+clientInfo.h);
	}

	/* (non-Javadoc)
	 * @see alma.test.corbareftest.AdministratorListener#clientLoggedOut(int)
	 */
	@Override
	public void clientLoggedOut(int clientHandle) {
		System.out.println("Client logged out: "+clientHandle);
	}

	/* (non-Javadoc)
	 * @see alma.test.corbareftest.AdministratorListener#componentLoggedIn(si.ijs.maci.ComponentInfo)
	 */
	@Override
	public void componentLoggedIn(ComponentInfo compInfo) {
		System.out.println("component logged in: "+compInfo.name+", "+compInfo.h);
		if (compInfo.name.equals("HELLOWORLD_JAVA")) {
			javaComponentLogged=true;
		} else if (compInfo.name.equals("HELLOWORLD_CPP")) {
			cppComponentLogged=true;
		} else if (compInfo.name.equals("HELLOWORLD_PY")) {
			pyComponentLogged=true;
		} else {
			System.out.println("Unknown component logged in: "+compInfo.name);
		}
	}

	/* (non-Javadoc)
	 * @see alma.test.corbareftest.AdministratorListener#componentLoggedOut(int)
	 */
	@Override
	public void componentLoggedOut(int comphandle) {
		System.out.println("Component logged out: "+comphandle);
		if (comphandle==cppCompHandle) {
			cppComponentLogged=false;
		} else if (comphandle==javaCompHandle) {
			javaComponentLogged=false;
		} else if (comphandle==pyCompHandle) {
			pyComponentLogged=false;
		} else {
			System.out.println("?? Unknown component logged out "+comphandle);
		}
	}

	/* (non-Javadoc)
	 * @see alma.test.corbareftest.AdministratorListener#componentReleased(si.ijs.maci.ComponentInfo)
	 */
	@Override
	public void componentReleased(ComponentInfo compInfo) {
		System.out.println("Component released "+compInfo.name);
	}

	/* (non-Javadoc)
	 * @see alma.test.corbareftest.AdministratorListener#containerLoggedIn(si.ijs.maci.ContainerInfo)
	 */
	@Override
	public void containerLoggedIn(ContainerInfo contInfo) {
		System.out.println("Container logged in: "+contInfo.name+", "+contInfo.h);
		if (contInfo.name.equals("cppContainer")) {
			cppContainerLogged=true;
			cppContHandle=contInfo.h;
		} else if (contInfo.name.equals("pyContainer")) {
			pyContainerLogged=true;
			pyContHandle=contInfo.h;
		} else if (contInfo.name.equals("javaContainer")) {
			javaContainerLogged=true;
			javaContHandle=contInfo.h;
		} else {
			System.out.println("??? Unknown container logged in "+contInfo.name);
		}
	}

	/* (non-Javadoc)
	 * @see alma.test.corbareftest.AdministratorListener#containerLoggedOut(int)
	 */
	@Override
	public void containerLoggedOut(int conthandle) {
		System.out.println("Container logged out: "+conthandle);
		if (conthandle==cppContHandle) {
			cppComponentLogged=false;
			cppContainerLogged=false;
		} else if (conthandle==javaContHandle) {
			javaComponentLogged=false;
			javaContainerLogged=false;
		} else if (conthandle==pyContHandle) {
			pyComponentLogged=false;
			pyContainerLogged=false;
		} else {
			System.out.println("?? Unknown container logged out "+conthandle);
		}
	}
	
	/**
	 * Test if the references are valid.
	 * 
	 * It works in this way:
	 *  1. get the components (python, java and C++ versions)
	 *  2. execute the <code>displayMessage()</code> in each component
	 *  3. wait until all the containers are back online
	 *  4. wait until all the components are back online
	 *  5  execute the <code>displayMessage()</code> in each component
	 *  6. wait until all the containers are back online
	 *  7. wait until all the components are back online
	 *  8  execute the <code>displayMessage()</code> in each component
	 *  
	 *  
	 *  At the beginning the containers are online, activated by tat as
	 *  defined in the prologue.
	 *  A separate process shuts down the containers and restarted them by using acs commands.
	 *  Lines 1-5 check if the references are valid with normal restarting of containers.
	 *  
	 *  After that, the external process kills the containers (abnormal termination)
	 *  and after a while restarts them
	 *  Lines 6-8 check if the references are valid also in this second case. 
	 */
	public void test() {
		// Get the components and call displayMessage for each of them
		try {
			getComponents();
		} catch (Exception e) {
			System.err.println("Error getting ACS components");
			e.printStackTrace(System.err);
			releaseComponents();
			return;
		}
		pyHW.displayMessage();
		javaHW.displayMessage();
		cppHW.displayMessage();
		
		// The external process shuts down the container and will restart them after some seconds.
		// We wait until components and containers are back online
		pyComponentLogged=javaComponentLogged=cppComponentLogged=false;
		pyContainerLogged=javaContainerLogged=cppContainerLogged=false;
		// Wait until the container logged in again
		if (!waitContainers()) {
			System.out.println("Not all the containers logged in");
			releaseComponents();
			return;
		} else {
			System.out.println("OK: Containers back online");
		}
		// Wait until the components logged in again
		if (!waitComponents()) {
			System.out.println("Not all the components logged in");
			//releaseComponents();
			//return;
		} else {
			System.out.println("OK: Components back online");
		}
		// Call the IDL methods to check if the references are still valid
		//pyHW.displayMessage();
		javaHW.displayMessage();
		cppHW.displayMessage();
		
		// After some seconds, the external process kills the containers simulating
		// an abnormal termination.
		// We wait until components and containers are back online
		pyComponentLogged=javaComponentLogged=cppComponentLogged=false;
		pyContainerLogged=javaContainerLogged=cppContainerLogged=false;
		// Wait until the container logged in again
		if (!waitContainers()) {
			System.out.println("Not all the containers logged in");
			releaseComponents();
			return;
		} else {
			System.out.println("OK: Containers back online");
		}
		// Wait until the components logged in again
		if (!waitComponents()) {
			System.out.println("Not all the components logged in");
			releaseComponents();
			return;
		} else {
			System.out.println("OK: Components back online");
		}
		// Call the IDL methods to check if the references are still valid
		pyHW.displayMessage();
		javaHW.displayMessage();
		cppHW.displayMessage();
		
		releaseComponents();
	}
	
	/**
	 * Wait until all the containers are back online
	 * 
	 * @return <code>true</code> if the containers are online
	 *         <code>false</code> in case of a timeout
	 */
	private boolean waitContainers() {
		// Wait until the container logged in again
		int t=0;
		while (!(pyContainerLogged && cppContainerLogged && javaContainerLogged) && t<300) {
			try {
				Thread.sleep(1000);
			} catch (Exception e) {}
			t++;
		}
		return pyContainerLogged && cppContainerLogged && javaContainerLogged;
	}
	
	/**
	 * Wait until all the components are back online
	 * 
	 * @return <code>true</code> if the components are online
	 *         <code>false</code> in case of a timeout
	 */
	private boolean waitComponents() {
		// Wait until the container logged in again
		System.out.println("\tWaiting components");
		int t=0;
		while (!(pyComponentLogged && cppComponentLogged && javaComponentLogged) && t<60) {
			try {
				Thread.sleep(1000);
			} catch (Exception e) {}
			t++;
		}
		System.out.println("\tWaiting components exiting "+(pyComponentLogged && cppComponentLogged && javaComponentLogged));
		return pyComponentLogged && cppComponentLogged && javaComponentLogged;
	}
	
	/**
	 * Get ACS components
	 */
	private void getComponents() throws Exception {
		pyHW =   HelloWorldHelper.narrow(getContainerServices().getComponent("HELLOWORLD_PY"));
		javaHW = HelloWorldHelper.narrow(getContainerServices().getComponent("HELLOWORLD_JAVA"));
		cppHW =  HelloWorldHelper.narrow(getContainerServices().getComponent("HELLOWORLD_CPP"));
	}
	
	/**
	 * Release ACS components
	 */
	private void releaseComponents() {
		if (pyHW!=null) {
			getContainerServices().releaseComponent(pyHW.name());
		}
		if (cppHW!=null) {
			getContainerServices().releaseComponent(cppHW.name());
		}
		if (javaHW!=null) {
			getContainerServices().releaseComponent(javaHW.name());
		}
	}

}
