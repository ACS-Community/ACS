/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory,
 * 2002 Copyright by ESO (in the framework of the ALMA collaboration), All
 * rights reserved
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation; either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package alma.contLogTest.TestLogLevelsCompImpl;
import java.util.logging.Handler;
import java.util.logging.Logger;

import alma.ACS.ComponentStates;
import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.IllegalArgumentEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalArgumentEx;
import alma.acs.component.ComponentLifecycle;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.AcsLoggingHandler;
import alma.acs.logging.StdOutConsoleHandler;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.contLogTest.TestLogLevelsCompOperations;
import alma.maci.loggingconfig.LoggingConfig;
import alma.typeSafeLogs.LOG_TEST_DummyMessage;
/**
 * A very simple component that does not make use of 
 * {@link alma.acs.component.ComponentImplBase}.
 * 
 * Javadoc comments have been removed to keep the
 * listing for the tutorial shorter.
 * 
 * @author eallaert
 */
public class TestLogLevelsCompImpl implements ComponentLifecycle, TestLogLevelsCompOperations
{
	public static final String PROP_ASSERTION_MESSAGE = "TestLogLevelsCompAssert"; 
	
	private ContainerServices m_containerServices;
	private Logger m_logger;

	private int levels[];

	/////////////////////////////////////////////////////////////
	// Implementation of ComponentLifecycle
	/////////////////////////////////////////////////////////////

	public void initialize(ContainerServices containerServices) {
		m_containerServices = containerServices;
		m_logger = m_containerServices.getLogger();
	//	m_logger.info("initialize() called...");
	}

	public void execute() {
	//	m_logger.info("execute() called...");
	}

	public void cleanUp() {
	//	m_logger.info("cleanUp() called..., nothing to clean up.");
	}

	public void aboutToAbort() {
		cleanUp();
	//	m_logger.info("managed to abort...");
		System.out.println("TestLogLevelsComp component managed to abort... you should know this even if the logger did not flush correctly!");
	}

	/////////////////////////////////////////////////////////////
	// Implementation of ACSComponent
	/////////////////////////////////////////////////////////////

	public ComponentStates componentState() {
		return m_containerServices.getComponentStateManager().getCurrentState();
	}
	public String name() {
		return m_containerServices.getName();
	}

	/////////////////////////////////////////////////////////////
	// Implementation of TestLogLevelsCompOperations
	/////////////////////////////////////////////////////////////

	public int[] getLevels() throws CouldntPerformActionEx {
		//m_logger.info("getLevels called...");
    	/*
    	 *  alma.maci.loggingconfig.LoggingConfig got generated from LoggingConfig.xsd and
    	 *  it contains the "default values" (also called "hardcoded").
    	 */
		levels = new int[5]; 
		LoggingConfig logConfig = new LoggingConfig();
		int hcMinLogLevel = Integer.parseInt(logConfig.getMinLogLevel().toString());
		int hcMinLogLevelLocal = Integer.parseInt(logConfig.getMinLogLevelLocal().toString());
		AcsLogLevel acsLevel = AcsLogLevel.getNativeLevel(m_logger.getLevel());
		int acsCoreLevel = acsLevel.getAcsLevel().value;
		
		// get separately the stdout and remote levels
		Handler[] handlers = m_logger.getHandlers();
		if (handlers.length != 2) {
			
			AcsJCouldntPerformActionEx ex = new AcsJCouldntPerformActionEx();
			ex.setProperty(PROP_ASSERTION_MESSAGE, "Found " + handlers.length + " log handlers where 2 were expected.");
			throw ex.toCouldntPerformActionEx();
			//m_logger.info("Found " + handlers.length + " log handlers where 2 were expected.");
			
		}		
		AcsLogLevel levelStdout = null;
		AcsLogLevel levelRemote = null;
		for (Handler logHandler : handlers) {
			if (logHandler instanceof StdOutConsoleHandler) {
				levelStdout = AcsLogLevel.getNativeLevel(logHandler.getLevel());
			}
			else if (logHandler instanceof AcsLoggingHandler) {
				levelRemote = AcsLogLevel.getNativeLevel(logHandler.getLevel());
			} 
			else {
				AcsJCouldntPerformActionEx ex = new AcsJCouldntPerformActionEx();
				ex.setProperty(PROP_ASSERTION_MESSAGE, "Handler " + logHandler + " is neither StdOutConsoleHandler nor AcsLoggingHandler");
				throw ex.toCouldntPerformActionEx();
			}
		}

		levels[0] = hcMinLogLevel;
		levels[1] = hcMinLogLevelLocal;
		levels[2] = acsCoreLevel;
		if (levelRemote == null)	// should never be the case, but anyway ...
			levels[3] = -1;
		else
			levels[3] = levelRemote.getAcsLevel().value;
		if (levelStdout == null)	// should never be the case, but anyway ...
			levels[4] = -1;
		else
			levels[4] = levelStdout.getAcsLevel().value;
		
		return levels;
	}

	
	
	/**
	 * @throws IllegalArgumentEx 
	 * @see alma.contLogTest.LogLevelsOperations#logDummyMessagesForAllLevels()
	 */
	public void logDummyMessages(int[] coreLevels) throws IllegalArgumentEx {
		// add a minor delay to be sure receiving side is all set-up and ready
		try {
			Thread.sleep(150);
		}
		catch (InterruptedException ex) {
		}
		AcsLogLevel acsLogLevel = null;
		for (int coreLevel : coreLevels) {
			String name;
			try {
				acsLogLevel = AcsLogLevel.fromAcsCoreLevel(AcsLogLevelDefinition.fromInteger(coreLevel));
				name = AcsLogLevelDefinition.fromInteger(coreLevel).name();
			} catch (AcsJIllegalArgumentEx e) {
				throw e.toIllegalArgumentEx();
			}
			m_logger.log(acsLogLevel, "dummy log message for core level " + coreLevel + "/" + name);
		}
		acsLogLevel = AcsLogLevel.fromAcsCoreLevel(AcsLogLevelDefinition.EMERGENCY);
		m_logger.log(acsLogLevel, "===last log message===");
		//LOG_TEST_DummyMessage.log(m_logger, "A beautiful name with a cherry on top", "Dr. F. Amous");
	}
}

