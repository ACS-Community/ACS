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
package alma.acs.container;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.logging.Level;

import alma.JavaContainerError.wrappers.AcsJContainerEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.concurrent.DaemonThreadFactory;
import alma.acs.container.corba.AcsCorba;
import alma.acs.container.corba.OrbConfigurator;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;
import alma.acs.util.CmdLineArgs;
import alma.acs.util.CmdLineRegisteredOption;
import alma.acs.util.StopWatch;


/**
 * The <code>main</code> method of this class starts an {@link AcsContainer}.
 * <p>
 * Tasks performed are:
 * <ul>
 * <li>registers JVM shutdown hook
 * <li>reads configuration info from various locations
 * <li>creates an AcsContainer
 * <li>logs in to the Manager
 * <li>starts ACS logging
 * <li>asks Manager to load a GUI component 
 * <li>waits until the ORB terminates
 * </ul> 
 * <p>
 * The implementation delegates many tasks to {@link alma.acs.container.AcsEmbeddedContainerRunner}, 
 * so that other applications which have to run a container inside can be written similarly to this class.
 * <p>
 * The functionality is comparable to that of <code>maciActivate.cpp</code> and 
 * <code>maciContainerImpl.cpp/ContainerImpl/init(argc, argv)</code>
 * on the C++ side.
 * 
 * @author hsommer
 */
public class AcsContainerRunner
{
	// properties read by this class
	private static final String CONTAINER_NAME_PROPERTYNAME = "ACS.containerName";
    private static final String MANAGER_PROPERTYNAME = "ACS.manager";
	public static final String CONTAINER_STARTTIME_DELAY_MILLIS_PROPERTYNAME = "acs.container.starttimeDelayMillis";
    
	
	protected String m_containerName;
    protected String m_managerLoc;
    protected boolean m_useRecoveryMode;

    protected AcsEmbeddedContainerRunner embeddedRunner;

    protected AcsLogger m_logger;
    protected AcsCorba m_acsCorba;

    protected int m_containerPort = -1;

    private ShutdownHook m_shutdownHook;
	
    protected int initialSleeptimeMillis = 0;

	/**
	 * Empty constructor, only to be called from <code>AcsContainerRunner#main</code> method.
	 */
    protected AcsContainerRunner()  {
	}
	
	
	/**
	 * The one and only <code>main</code> method to run a Java container. Options:
	 * <ul>
	 * <li><code>-manager myManagerloc</code><br>
	 * 		CORBA loc for the Manager, e.g.
	 * 		<code>corbaloc::myhost:xxxx/Manager</code>
	 * 		(same as property <code>ACS.manager</code>, but with higher precedence);
	 * 		if missing, it will be taken from CDB, or will default to localhost.</li>
	 * <li><code>-containerName myContainerName</code><br>
	 * 		name under which the container will introduce itself to the ACS Manager
	 * 		(has precedence over the property <code>ACS.containerName</code>).</li>
	 * <li>any other arguments will be passed on to the CORBA ORB.</li>
	 * </ul> 
	 */
	public static void main(String[] args)
	{
		AcsContainerRunner contRunner = null;
		try {
			contRunner = new AcsContainerRunner();
			contRunner.run(args);
		}
		catch (Throwable thr) {			
			StringWriter sw = new StringWriter();
			thr.printStackTrace(new PrintWriter(sw));
			String msg = "\n *** Top level exception in AcsContainerRunner#main: " + sw.toString();
			if (contRunner.m_logger != null) {
				contRunner.m_logger.severe(msg);
			}
			System.err.println(msg);

			if (contRunner.m_acsCorba != null) {
				contRunner.m_acsCorba.doneCorba();
			}
		}
	}


	/**
	 * Startup choreography: performs the various tasks in the correct order.
	 * <p>
	 * Note on the implementation: the steps involved are grouped as private methods
	 * that access the instance variables. The intent was to make the sequence clearer.
	 *   
	 * @param args  command line args as given to <code>main</code>.
	 * @throws AcsJContainerServicesEx  at the slightest provocation...
	 */
	private void run(String[] args) throws AcsJContainerEx
	{
		StopWatch containerStartWatch = new StopWatch();

		// just a temp logger
		m_logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("AcsContainerRunner", true);
		String argsString = "";
		for (String arg : args) {
			argsString += arg + " ";
		}
		m_logger.log(Level.INFO, "AcsContainerRunner#run with arguments " + argsString);

		setOptions(args);

		embeddedRunner = new AcsEmbeddedContainerRunner(false, m_useRecoveryMode);
		embeddedRunner.setContainerName(m_containerName);
		embeddedRunner.setManagerLoc(m_managerLoc);

		checkReadyToRun();

		// now that embeddedRunner knows the container name, we can switch to the real logger  
		m_logger = embeddedRunner.getContainerLogger();
		m_acsCorba = new AcsCorba(m_logger);

        if (initialSleeptimeMillis > 0) {
        	m_logger.fine("Container will sleep for " + initialSleeptimeMillis + " ms, e.g. to allow remote debuggers to attach at this early stage.");
            try {
                Thread.sleep(initialSleeptimeMillis);
            } catch (InterruptedException e) {
            	m_logger.info("Woken up too early from initial-delay sleep.");
            }
        }

		m_acsCorba.initCorba(args, m_containerPort);	
		m_acsCorba.runCorba();
		
		embeddedRunner.run(m_acsCorba);

		m_shutdownHook = new ShutdownHook(m_logger);
		Runtime.getRuntime().addShutdownHook(m_shutdownHook);
		m_shutdownHook.setAcsContainer(embeddedRunner.getContainer());
						
        initAcsLogging(embeddedRunner.getManagerProxy());

		m_logger.fine("entering orb loop");

		containerStartWatch.setLogger(m_logger);
		containerStartWatch.logLapTime("start the container");
		
		// here we hang for the life time of the container...
		m_acsCorba.blockOnORB();
		m_logger.fine("orb loop over.");
		
		m_logger.exiting(AcsContainerRunner.class.getName(), "run");
		m_shutdownHook.setRegularShutdownExpected();
	}
	


    /**
     * Asynchronously connects to the log service so that after some time the locally collected log records
     * will be sent over the wire. 
     */
    protected void initAcsLogging(final AcsManagerProxy managerProxy) {       
        Runnable cmd = new Runnable() {
            public void run() {
                
                // TODO: move to separate CDBConfigReader class and set logger data independently of
                // AcsLogManager().init when the CDB becomes available.                
                //                int cacheSize = -1;
                //                int minCachePriority = AcsLogLevel.ACS_LEVEL_UNKNOWN;
                //                int maxCachePriority = -1;
                //                try {
                //                    DAO dao = cdb.get_DAO_Servant("MACI/Containers/" + containerName);
                //                    cacheSize = dao.get_long("cacheSize");
                //                    minCachePriority = dao.get_long("minCachePriority");
                //                    maxCachePriority = dao.get_long("maxCachePriority");
                //                } catch (Throwable thr) {
                //                    thr.printStackTrace();
                //                }                 
                
                m_logger.finer("asynchronously calling ClientLogManager#initRemoteLogging()...");
                boolean gotLogService = false;
                try
                {
                    gotLogService = ClientLogManager.getAcsLogManager().initRemoteLogging(
                            m_acsCorba.getORB(), 
                            managerProxy.getManager(), 
                            managerProxy.getManagerHandle(),
                            true);
                } catch (Exception e) {
                    // just log below
                }
                if (gotLogService) {
                    m_logger.finer("done ClientLogManager#initRemoteLogging");
                }
                else {
                    m_logger.log(Level.WARNING, "ACS central logging not available!");
                }
            }            
        };
        ExecutorService executor = Executors.newSingleThreadExecutor(new DaemonThreadFactory("InitRemoteLogging"));
        executor.execute(cmd);
    }

    
    
	/**
	 * Parses commandline and property options.
	 * 
	 * @param args		as received by main()
	 */
	void setOptions(String[] args) throws AcsJContainerEx
	{
		// -- prepare arg parser
		CmdLineArgs cmdArgs = new CmdLineArgs();
		// container name
		CmdLineRegisteredOption optContainerName =
			new CmdLineRegisteredOption("-containerName", 1);
		cmdArgs.registerOption(optContainerName);
		// container port; TODO unify port argument / property with CDB, C++, etc., abstract from ORB option 
		CmdLineRegisteredOption optContainerPort =
			new CmdLineRegisteredOption("-OAPort", "-OAport", 1);
		cmdArgs.registerOption(optContainerPort);
//		CmdLineRegisteredOption optContainerPort2 =
//			new CmdLineRegisteredOption("-OAport", 1);
//		cmdArgs.registerOption(optContainerPort2);
		// manager reference
		CmdLineRegisteredOption optManagerLoc =
			new CmdLineRegisteredOption("-manager", "-m", 1);
		cmdArgs.registerOption(optManagerLoc);
		// recovery mode 
		CmdLineRegisteredOption optRecoveryMode =
			new CmdLineRegisteredOption("-recovery", "-r", 0);
		cmdArgs.registerOption(optRecoveryMode);
		// no-recovery mode 
		CmdLineRegisteredOption optNoRecoveryMode =
			new CmdLineRegisteredOption("-norecovery", "-nr", 0);
		cmdArgs.registerOption(optNoRecoveryMode);

		// -- parse and set args
		try
		{
			cmdArgs.parseArgs(args);
			
			// -- container name
			if (cmdArgs.isSpecified(optContainerName))
			{
				m_containerName = cmdArgs.getValues(optContainerName)[0].trim();
			}
			else
			{
				m_containerName = System.getProperty(CONTAINER_NAME_PROPERTYNAME);
			}
			
			// -- container port
			if (cmdArgs.isSpecified(optContainerPort))
			{
				m_containerPort = Integer.parseInt(cmdArgs.getValues(optContainerPort)[0]);
			}
			else
			{
//				default port -- C++ container uses -ORBEndpoint, default 3 0 5 0
				m_containerPort = OrbConfigurator.ORB_DEFAULT_PORT; 
			}
			
			// -- manager
			if (cmdArgs.isSpecified(optManagerLoc))
			{
				m_managerLoc = cmdArgs.getValues(optManagerLoc)[0].trim();
			}
			else if (System.getProperty(MANAGER_PROPERTYNAME) != null)
			{
				m_managerLoc = System.getProperty(MANAGER_PROPERTYNAME).trim();
			}
			else
			{
				// default = localhost
				m_managerLoc = AcsManagerProxy.getLocalManagerCorbaloc();
			}
			
			// -- recovery mode: command line default is "-recovery", so only "-nr" or "-norecovery" is interesting.
			//    For historical reasons we also have the "-r" or "-recovery" flag, and we check both to rule out a conflict.
			//    As long as the java container does not evaluate the Recovery attribute from the CDB, 
			//    we at least use the CDB default (instead of cmd line default which would be overwritten later).
			if (!cmdArgs.isSpecified(optNoRecoveryMode) && !cmdArgs.isSpecified(optRecoveryMode)) {
				m_useRecoveryMode = (new alma.maci.containerconfig.Container()).getRecovery();
			}
			if (cmdArgs.isSpecified(optNoRecoveryMode) && cmdArgs.isSpecified(optRecoveryMode)) {
				m_logger.warning("Conflicting command line options for recovery mode: both -r and -nr are specified. Will use -r.");
				m_useRecoveryMode = true;
			}
			
            Integer starttimeDelayMillisProperty = Integer.getInteger(CONTAINER_STARTTIME_DELAY_MILLIS_PROPERTYNAME);
            if (starttimeDelayMillisProperty != null) {
                initialSleeptimeMillis = starttimeDelayMillisProperty.intValue();
            }
		}
		catch (Throwable thr)
		{
			AcsJContainerEx ex = new AcsJContainerEx(thr);
			ex.setContextInfo("incorrect or missing arguments.");
			throw ex;
		}
	}

	protected void checkReadyToRun() throws AcsJContainerEx {
		String msg = null;
		if (m_containerPort <= 0) {
			msg = "no container port specified; ";
		}
		embeddedRunner.checkReadyToRun(msg);
	}

}
