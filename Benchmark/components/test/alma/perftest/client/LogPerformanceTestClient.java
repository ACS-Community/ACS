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
 *
 *
 */
package alma.perftest.client;

import java.util.logging.Level;
import java.util.logging.Logger;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.component.client.ComponentClient;
import alma.perftest.LogStressWithDelay;

/**
 * Client application that accesses the LogStressWithDelay component. It 
 * is used to start up multiple logging components and 'stress' 
 * test the logging channel. This client is rather simple and doesn't have anything
 * too fancy in the way of error handling or 'usage' statements, etc. given 
 * that it was developed just for a quick test that we were running for debugging
 * the PSIL.
 *
 * You can adjust/tweak the number of components
 * that this test uses in the following way:
 *
 * 1) You must have entries in the CDB for each instance of the component
 * that you wish to use, for example:
 *
		<_ Name="REPLACE_WITH_COMPONENT_NAME" 	    
         Code="REPLACE_WITH_COMPONENT_CODE" 
         Type="IDL:alma/perftest/LogStressWithDelay:1.0" 	
         Container="cppContainer1"/>

 * Note: this client currently requires that the numbering starts with '1' - so if you have N 
 * component instances they must be named COMPONENT_NAME1, COMPONENT_NAME2, COMPONENT_NAME3, ... COMPONENT_NAMEN.
 *
 * 2) You should invoke the client passing a couple of properties to the java runtime.
 *    These two properties are: a) NumLogs -> an integer indicating how many logs to send 
 *                              b) NumComponents -> an integer indicating the number of components that will
 *                                 be started in parallel.
 *
 *    For example: acsStartJava -D NumLogs=5000 -D NumComponents=3 alma.perftest.client.LogPerformanceTestClient
 *
 * 3) This client starts the C++ component(s) which sends logs to the loggingChannel. There is also a 
 *    java component which does something similar, but it must be started by hand (if it is desired 
 *    for some reason to use a java component to send logs), e.g. using objexp, as this client doesn't 
 *    use it at present.
 *    
 *
 * @author sharring 06/20/2006
 */
public class LogPerformanceTestClient extends ComponentClient
{
	private static final String DELAY = "Delay";
	private static final String NUM_COMPONENTS = "NumComponents";
	private static final String NUM_LOGS = "NumLogs";
	private static final String ACS_MANAGER = "ACS.manager";
	private static final String COMPONENT_NAME = "ComponentName";
	private static final String DEFAULT_COMPONENT_NAME = "LOGSTRESSCPP";

	private LogStressWithDelay[] m_logStressComp;

	/**
	 * @param logger the logger to use for logging messages.
	 * @param managerLoc location of the ACS manager.
	 * @param clientName name of the client.
	 * @throws Exception can throw Exceptions if there are problems.
	 */
	public LogPerformanceTestClient(Logger logger, String managerLoc, String clientName) 
		throws Exception 
	{
		super(logger, managerLoc, clientName);
	}

	/**
	 * Calls logNumTimes() on the logStress component.
	 */
	public void activateComponentsAndSendLogs() throws AcsJContainerServicesEx  
	{
		int numToLog = 25;
		int numComponents = 1;
		int delay = 0;
		String numToLogString = System.getProperty(NUM_LOGS);
		String numComponentsString = System.getProperty(NUM_COMPONENTS);
		String delayBetweenLogsString = System.getProperty(DELAY);
		String componentName = System.getProperty(COMPONENT_NAME);

		try {
			if(null != numToLogString) {
				numToLog = Integer.parseInt(numToLogString);
			}
			if(null != numComponentsString) {
				numComponents = Integer.parseInt(numComponentsString);
			}
			if(null != delayBetweenLogsString) {
				delay = Integer.parseInt(delayBetweenLogsString);
			}
			if(null == componentName) {
				componentName = DEFAULT_COMPONENT_NAME;
			}
		}
		catch (NumberFormatException ex) {
			numToLog = 25;
			numComponents = 1;
			delay = 0;
		}

		m_logStressComp = new LogStressWithDelay[numComponents]; 
		Logger logger = getContainerServices().getLogger();
		for(int i = 1; i <= numComponents; i++) 
		{
			//try 
			{
				org.omg.CORBA.Object obj = getContainerServices().getComponent(componentName + i);
				System.out.println("obj is: " + obj);
				logger.log(Level.INFO, "acquired generic corba object for " + componentName + " " + i);
				m_logStressComp[i-1] = alma.perftest.LogStressWithDelayHelper.narrow(obj);
			}
		}

		for(int i = 0; i < m_logStressComp.length; i++) {
			m_logStressComp[i].logNumTimes(numToLog, delay);
		}

		boolean doneArray[] = new boolean[m_logStressComp.length];
		for(int i = 0; i < doneArray.length; i++) {
			doneArray[i] = false;
		}

		int doneCount = 0;
		while(doneCount < numComponents) 
		{
			try 
			{
				Thread.sleep(355);
			}
			catch(InterruptedException ex) 
			{
				//noop	
			}
			for(int i = 0; i < numComponents; i++) 
			{
				if(!doneArray[i] && m_logStressComp[i].getThreadDone()) 
				{
					getContainerServices().releaseComponent(componentName + (i+1));
					doneArray[i] = true;
					doneCount++;
					logger.log(Level.SEVERE, "releasing component: " + i);
				}
			}
		}
	}

	/**
	 * Checks whether the Java property 'ACS.manager' is set and calls the
	 * other methods from this class.
	 */
	public static void main(String[] args) 
	{
		try {
			Thread.sleep(25000);
		} 
		catch(InterruptedException ex) {}
		
		String managerLoc = System.getProperty(ACS_MANAGER);
		if (managerLoc == null) {
			System.out.println("Java property 'ACS.manager' must be set to the corbaloc of the ACS manager!");
			System.exit(-1);
		}
		String clientName = "LogPerformanceTestClient";
		LogPerformanceTestClient performanceTestClient = null;
		try {
			performanceTestClient = new LogPerformanceTestClient(null, managerLoc, clientName);
			performanceTestClient.activateComponentsAndSendLogs();
		}
		catch (Exception e) {
            try {
                Logger logger = performanceTestClient.getContainerServices().getLogger();
                logger.log(Level.SEVERE, "Client application failure", e);
            } catch (Exception e2) {
                e2.printStackTrace(System.err);
            }
		}
		finally {
			if (performanceTestClient != null) {
				try {
					performanceTestClient.tearDown();
				}
				catch (Exception e3) {
					// bad luck
                    e3.printStackTrace();
				}
			}
		}
		System.exit(0);
	}
}

