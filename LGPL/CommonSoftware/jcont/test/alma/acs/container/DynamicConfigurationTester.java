/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
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

//import java.io.PrintWriter;
//import java.util.Random;
//import java.util.concurrent.BlockingQueue;
//import java.util.concurrent.TimeUnit;
//import java.util.logging.Level;

//import com.cosylab.logging.engine.log.ILogEntry;
//import com.cosylab.logging.engine.log.LogTypeHelper;

//import alma.acs.logging.AcsLogLevel;
//import alma.acs.logging.ClientLogManager;
//import alma.acs.logging.engine.LogReceiver;
//import alma.acs.logging.engine.LogReceiver.DelayedLogEntry;
//import alma.maci.loggingconfig.LoggingConfig;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import alma.acs.concurrent.DaemonThreadFactory;
import alma.acs.container.corba.AcsCorba;
import alma.acs.container.corba.OrbConfigurator;
import alma.JavaContainerError.wrappers.AcsJContainerEx;
import si.ijs.maci.LoggingConfigurablePackage.LogLevels;
import alma.acs.util.StopWatch;
import alma.acs.logging.*;
import alma.acs.component.client.ComponentClientTestCase;

/**
 * Tests for {@link alma.acs.container.AcsContainer
 * INFO: This test is not used, it is only written as a guideline, instead we are using maciContainerLogLevels tool 
 * @author nbarriga
 * created 28.02.2007
 */
public class DynamicConfigurationTester extends ComponentClientTestCase {
        // properties read by this class
        private static final String CONTAINER_NAME_PROPERTYNAME = "ACS.containerName";
        private static final String MANAGER_PROPERTYNAME = "ACS.manager";
        public static final String CONTAINER_STARTTIME_DELAY_MILLIS_PROPERTYNAME = "acs.container.starttimeDelayMillis";


        protected AcsEmbeddedContainerRunner embeddedRunner;
        protected Logger m_logger;
        protected String m_containerName;
        protected String m_managerLoc;
        protected AcsCorba m_acsCorba;
        protected int m_containerPort;
        private ShutdownHook m_shutdownHook;

        protected int initialSleeptimeMillis = 1000;

        /**
         * Asynchronously connects to the log service so that after some time the locally collected log records
         * will be sent over the wire.
         */
        protected void initAcsLogging(final AcsManagerProxy managerProxy) {
                Runnable cmd = new Runnable() {
                        public void run() {


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
        protected void initAll() throws Exception{
         //       StopWatch containerStartWatch = new StopWatch();
                m_containerName="dynamicTestContainer";
                m_containerPort = OrbConfigurator.ORB_DEFAULT_PORT;
                m_managerLoc=AcsManagerProxy.getLocalManagerCorbaloc();

                
                m_logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("DynamicConfigurationTester", true);
                m_logger.log(Level.INFO, "DynamicConfigurationTester::initAll ");
                embeddedRunner = new AcsEmbeddedContainerRunner(true, false);
                embeddedRunner.setContainerName(m_containerName);
                embeddedRunner.setManagerLoc(m_managerLoc);

                // now that embeddedRunner knows the container name, we get switch to the real logger
                m_logger = embeddedRunner.getContainerLogger();
                m_acsCorba = new AcsCorba(m_logger);
                if(initialSleeptimeMillis>0){
                        m_logger.fine("Container will sleep for "+initialSleeptimeMillis+"  ms, e.g. to allow remote debuggers to attach at this early stage.");
                        try {
                                Thread.sleep(initialSleeptimeMillis);
                        } catch (InterruptedException e) {
                                m_logger.info("Woken up too early from initial-delay sleep.");
                        }
                }
                try{
                        embeddedRunner.checkReadyToRun(null);
                }catch (AcsJContainerEx ex){
                        m_logger.warning("embeddedRunner.checkReadyToRun threw an exception!");
                        ex.log(m_logger);
                }

                m_acsCorba.initCorba(null, m_containerPort);
                
                m_acsCorba.runCorba();
                try{
                        embeddedRunner.run(m_acsCorba);
                }catch (AcsJContainerEx ex){
                        m_logger.warning("embeddedRunner.run threw an exception!");
                        ex.log(m_logger);
                }

                m_shutdownHook = new ShutdownHook(m_logger);
                Runtime.getRuntime().addShutdownHook(m_shutdownHook);
                m_shutdownHook.setAcsContainer(embeddedRunner.getContainer());

                initAcsLogging(embeddedRunner.getManagerProxy());

               // containerStartWatch.setLogger(m_logger);
                //containerStartWatch.logLapTime("start the container");
            
        }
        protected void shutdownAll() throws Exception{
                m_shutdownHook.setRegularShutdownExpected();
                embeddedRunner.m_container.shutdown(0, true);
                m_acsCorba.shutdownORB(true);
                m_acsCorba.doneCorba();
        }
        public DynamicConfigurationTester() throws Exception {
                super("DynamicConfigurationTester");

        }

        protected void setUp() throws Exception {
                super.setUp();
                initAll();
        }

        protected void tearDown() throws Exception {
                super.tearDown();
                shutdownAll();
        }

        public void testRefreshLoggingConfig(){
                m_logger.info("Testing INFO(4) logging");
                m_logger.warning("Testing WARNING(6) logging");
               /* Need to add code here to change the xml file, maybe calling a script
                    
                try {
                        Thread.sleep(10000);
                } catch (InterruptedException e) {
                        m_logger.info("Interrupted my sleep!!!!");
                }*/
                embeddedRunner.m_container.refresh_logging_config();
                m_logger.info("Testing INFO(4) logging");
                m_logger.warning("Testing WARNING(6) logging");
        }

        public void testGetDefaultLogLevels(){
                m_logger.info("Starting testGetDefaultLogLevels");
                LogLevels logLevels=embeddedRunner.m_container.get_default_logLevels();
                assertEquals(0,logLevels.minLogLevel);
                assertEquals(2,logLevels.minLogLevelLocal);
        }

        public void testSetDefaultLogLevels(){
                m_logger.info("Starting testSetDefaultLogLevels");
                LogLevels logLevels=embeddedRunner.m_container.get_default_logLevels();
                assertEquals(0,logLevels.minLogLevel);
                assertEquals(2,logLevels.minLogLevelLocal);
                m_logger.info("Testing INFO(4) logging");
                m_logger.warning("Testing WARNING(6) logging");
                LogLevels newLogLevels=new LogLevels();
                newLogLevels.useDefault=false;
                newLogLevels.minLogLevel=5;
                newLogLevels.minLogLevelLocal=6;
                embeddedRunner.m_container.set_default_logLevels(newLogLevels);
                logLevels=embeddedRunner.m_container.get_default_logLevels();
                assertEquals(5,logLevels.minLogLevel);
                assertEquals(6,logLevels.minLogLevelLocal);
                m_logger.info("Testing INFO(4) logging");
                m_logger.warning("Testing WARNING(6) logging");

        }


}
