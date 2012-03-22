package alma.acs.component.client;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import alma.ACS.ACSComponent;
import alma.ACS.ACSComponentHelper;
import alma.acs.logging.ClientLogManager;
import alma.acs.shutdown.ShutdownHookBase;
import alma.acs.util.ACSPorts;
import alma.acs.util.AcsLocations;
import alma.acs.util.CmdLineArgs;
import alma.acs.util.CmdLineRegisteredOption;
import alma.acs.util.StopWatch;

/**
 * A little tool inspired by http://jira.alma.cl/browse/COMP-6580 ,
 * which connects to a component and keeps pinging its name() method.
 */
public class ComponentRefTracker extends ComponentClient
{
	ComponentRefTracker(Logger logger, String managerLoc, String name) throws Exception {
		super(logger, managerLoc, name);
	}

	private String targetCompUrl;
	
	private ACSComponent targetComp;

	private int pingDelaySec = -1;

	private volatile CountDownLatch shutdownSync = null;

	private boolean checkExists = false;

	/**
	 * Reader for System.in, or null if option "-interactive" was not specified.
	 */
	private BufferedReader inputReader;

	@Override
	public void initRemoteLogging() {
		// do nothing
	}
	
	@Override
	protected void registerShutdownHook() {
		m_shutdownHook = new ShutdownHookBase(m_logger, "ComponentRefTracker") {
			protected void interruptDetected() {
				try {
					shutdownSync = new CountDownLatch(1);
					m_logger.info("SIGINT received. Will terminate...");
					shutdownSync.await(10, TimeUnit.SECONDS);
				} catch(Exception ex) {
					m_logger.log(Level.WARNING, "Shutdown error: ", ex);
				}
			}
		};
		Runtime.getRuntime().addShutdownHook(m_shutdownHook);
	}
	
	boolean setOptions(String[] args) {
		CmdLineArgs cmdArgs = new CmdLineArgs();
		CmdLineRegisteredOption optHelp = new CmdLineRegisteredOption("--help", "-h", 0);
		cmdArgs.registerOption(optHelp);
		CmdLineRegisteredOption optCurl = new CmdLineRegisteredOption("-curl", 1);
		cmdArgs.registerOption(optCurl);
		CmdLineRegisteredOption optPingDelaySec = new CmdLineRegisteredOption("-delaySec", 1);
		cmdArgs.registerOption(optPingDelaySec);
		CmdLineRegisteredOption optCheckExists = new CmdLineRegisteredOption("-checkExists", 0);
		cmdArgs.registerOption(optCheckExists);
		CmdLineRegisteredOption optInteractive = new CmdLineRegisteredOption("-interactive", 0);
		cmdArgs.registerOption(optInteractive);

		cmdArgs.parseArgs(args);

		if (cmdArgs.isSpecified(optHelp)) {
			System.out.println("Usage: acsStartJava " + getClass().getName()  
					+ "\n\t" + optCurl.getName() + " <curl> "
					+ "\n\t" + "[ " + optInteractive.getName() + " | " + optPingDelaySec.getName() + " <delay in s between component calls> ]"
					+ "\n\t" + "[ " + optCheckExists.getName() + " ]"
					);
			return false;
		}
		else {
			if (cmdArgs.isSpecified(optInteractive)) {
				inputReader = new BufferedReader(new InputStreamReader(System.in));
			} 
			
			if (cmdArgs.isSpecified(optPingDelaySec)) {
				if (inputReader != null) {
					m_logger.info("Ignoring setting for ping delay because only manual component calls will be used.");
				}
				else {
					pingDelaySec = Integer.parseInt(cmdArgs.getValues(optPingDelaySec)[0].trim());
				}
			} 
			else {
				if (inputReader == null) {
					pingDelaySec = 5;
				}
			}
			
			if (cmdArgs.isSpecified(optCurl)) {
				targetCompUrl = cmdArgs.getValues(optCurl)[0].trim();
			} 
			else {
				throw new IllegalArgumentException("Option '" + optCurl.getName() + "' must be specified.");
			}
			
			if (cmdArgs.isSpecified(optCheckExists)) {
				checkExists = true;
			} 
			
		}
		return true;
	}

	
	private void run(String[] args) throws Exception {
		try {
			// @TODO: Call setOptions before connecting to the manager etc in the base class ctor
			if (!setOptions(args)) {
				return;
			}
			
	//		m_logger.fine("Will get reference to component '" + targetCompUrl + "'...");
			try {
				targetComp = ACSComponentHelper.narrow(getContainerServices().getComponent(targetCompUrl));
			} catch (Exception ex) {
				m_logger.warning("Failed to connect to component '" + targetCompUrl + "' in the first place. Will terminate.");
				return;
			}
			String ior = getContainerServices().getAdvancedContainerServices().corbaObjectToString(targetComp);
		
			String msg = "Got reference to component '" + targetCompUrl + "', IOR='" + ior + "'.";
			if (pingDelaySec > 0) {
				msg += " Will call it every " + pingDelaySec + " seconds.";
			}
			m_logger.info(msg);
			
			runLoop();
		}
		finally {
			tearDown();
			
			// if we exit from here through an exception rather than external SIGINT, 
			// the shutdown hook will be called anyway, and we must prepare it for this case... 
			m_shutdownHook.setRegularShutdownExpected();
			
			shutdownSync.countDown(); // free the shutdown hook thread
		}
	}

	private void runLoop() throws IOException, InterruptedException {
		long nextCallTime = -1;
		
		if (inputReader != null) {
			m_logger.info("Press ENTER to call " + targetCompUrl);
		}
		
		while(shutdownSync == null) {
			
			// Check if we should simply wait for user input or more time to pass
			if ( (inputReader != null && !inputReader.ready()) || System.currentTimeMillis() < nextCallTime) {
				Thread.sleep(100); // sleep short time only, to be responsive to shutdown
				continue;
			}

			if (inputReader != null) {
				// We are in interactive mode.
				// There is user input, thus we can dare to call the otherwise non-interruptable blocking readLine() method.
				inputReader.readLine();
			}
			
			StopWatch sw = new StopWatch();
			try {
				if (checkExists && targetComp._non_existent()) {
					// Not sure if we gain anything from calling _non_existent.
					// It may only return true if our proxy object has been destroyed.
					// In case of target container ORB problems, it will throw an exception, just like the name() call does.
					m_logger.info("Component '" + targetCompUrl + "' does not exist (see Corba spec 4.3.5.1). Call took " + sw.getLapTimeMillis() + " ms.");
				}
				else {
					String currentName = targetComp.name();
					
					String msg = "Call to " + targetCompUrl + "#name() returned fine";
					if (!targetCompUrl.equals(currentName)) {
						msg += " but with unexpected name '" + currentName;
					}
					m_logger.info(msg + ". Call took " + sw.getLapTimeMillis() + " ms.");
				}
			} catch (Throwable thr) {
				m_logger.info("Call to component '" + targetCompUrl + "' failed with " + thr.getClass().getName() + ", after " + sw.getLapTimeMillis() + " ms.");
			}
			
			if (inputReader == null) {
				nextCallTime = System.currentTimeMillis() + pingDelaySec * 1000;
			}
		}
	}
	
	
	public static void main(String[] args) {
		
		String name = ComponentRefTracker.class.getSimpleName();
		ClientLogManager.getAcsLogManager().suppressRemoteLogging();
		Logger logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(name, false);
		
		String managerLoc = null;
		if (System.getProperty("ACS.manager") != null) {
			managerLoc = System.getProperty("ACS.manager").trim();
		}
		else {
			// default = localhost
			AcsLocations.convertToManagerLocation(ACSPorts.getIP(), ACSPorts.getManagerPort());
		}

		try {
			new ComponentRefTracker(logger, managerLoc, name).run(args);
		}
		catch (Throwable thr) {
			logger.log(Level.SEVERE, "Top-level exception caught:", thr);
		}
	}

}
