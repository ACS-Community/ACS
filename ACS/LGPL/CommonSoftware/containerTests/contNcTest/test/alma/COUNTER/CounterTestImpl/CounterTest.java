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
package alma.COUNTER.CounterTestImpl;


import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

import junit.framework.Assert;
import junit.framework.TestCase;

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.COUNTER.CounterConsumer;
import alma.COUNTER.CounterSupplier;
import alma.acs.component.client.ComponentClientTestCase;

/**
 * Requires Java component "CONSUMER_JAVA" of type <code>alma.COUNTER.CounterConsumer</code> to be running.
 * 
 * @author eallaert 30 October 2007
 */
public class CounterTest extends ComponentClientTestCase
{
	public static final String PROPERTYNAME_SUPPLIERNAMES = "COUNTER_SUPPLIER";
	public static final String PROPERTYNAME_CONSUMERNAMES = "COUNTER_CONSUMERS";
	public static final String PROPERTYNAME_INITVALUE     = "COUNTER_INIT_VALUE";
	public static final String PROPERTYNAME_LASTVALUE     = "COUNTER_LAST_VALUE";
	public static final String PROPERTYNAME_PERIOD        = "COUNTER_PERIOD";
        
    private Set<String> supplierNames = new LinkedHashSet<String>();
    private Set<String> consumerNames = new LinkedHashSet<String>();
    private List<CounterConsumer> consumers;
    private List<CounterSupplier> suppliers;
    private int initVal = 1;
    private int lastVal = 20;
    private int changeVal = 0;
    private float period = 0.5f;
    private ContainerTestUtil containerTestUtil; 
    
    /**
	 * @throws java.lang.Exception
	 */
	public CounterTest() throws Exception
	{
		super(CounterTest.class.getName());
	}

	/**
	 * @see TestCase#setUp()
	 */
	protected void setUp() throws Exception {
		super.setUp();
		
		// set names of test supplier/consumer components
		String compNames = System.getProperty(PROPERTYNAME_SUPPLIERNAMES);
		if (compNames != null) {
			// Note that the tokens need to be separated by slash or comma - the standard
			// whitespace separators cannot be used due to how acsStartJava eliminates quotes
			// with the help of "getopt -u". The way around that would be to set directly
			// the env var JAVA_OPTIONS.
			StringTokenizer tok = new StringTokenizer(compNames, " /,");
			while (tok.hasMoreTokens()) {
				supplierNames.add(tok.nextToken());
			}
			if (supplierNames.size() != 1) {
				Assert.fail("Property " + PROPERTYNAME_SUPPLIERNAMES + " must contain exactly 1 componentName"); 
			}
		}
		
		compNames = System.getProperty(PROPERTYNAME_CONSUMERNAMES);
		if (compNames != null) {
			StringTokenizer tok = new StringTokenizer(compNames, " /,");
			while (tok.hasMoreTokens()) {
				consumerNames.add(tok.nextToken());
			}
		}
		
		String number = System.getProperty(PROPERTYNAME_INITVALUE);
		if (number != null) {
			initVal = Integer.parseInt(number);
		}

		number = System.getProperty(PROPERTYNAME_LASTVALUE);
		if (number != null) {
			lastVal = Integer.parseInt(number);
		}
		changeVal = lastVal;
		
		number = System.getProperty(PROPERTYNAME_PERIOD);
		if (number != null) {
			period = Float.parseFloat(number);
		}
		
		suppliers = new ArrayList<CounterSupplier>();		
		for (String compName : supplierNames) {			
			suppliers.add(alma.COUNTER.CounterSupplierHelper.narrow(getContainerServices().getComponent(compName)));
		}
		
		consumers = new ArrayList<CounterConsumer>();		
		for (String compName : consumerNames) {
			System.out.println("Bringing up consumer " + compName);
			consumers.add(alma.COUNTER.CounterConsumerHelper.narrow(getContainerServices().getComponent(compName)));
		}
		
		containerTestUtil = new ContainerTestUtil(getContainerServices(), m_acsManagerProxy);
		containerTestUtil.loginToManager();
	}

	/**
	 * @see TestCase#tearDown()
	 */
	protected void tearDown() throws Exception {
		for (String compName : consumerNames) {
			getContainerServices().releaseComponent(compName);
		}
		for (String compName : supplierNames) {
			getContainerServices().releaseComponent(compName);
		}
		containerTestUtil.logoutFromManager();
		super.tearDown();
	}
	
	
	
	/**
	 * @throws Exception
	 */
	public void testNC() throws Exception
	{
		// Start consumers first, then suppliers
		for (CounterConsumer consumer : consumers) {
			String componentName = consumer.name();
			try {
				consumer.getBlocks();
				m_logger.info(componentName + " now expecting blocks");
			} catch (CouldntPerformActionEx ex) {
				throw AcsJCouldntPerformActionEx.fromCouldntPerformActionEx(ex);
			}
		}
		
		
		int blocksOut = 0;
		for (CounterSupplier supplier : suppliers) {
			String componentName = supplier.name();
			try {
				m_logger.info("requesting " + componentName + " to sendBlocks");
				blocksOut = supplier.sendBlocks(initVal, lastVal, changeVal, period);
				m_logger.info(componentName + " has sent " + blocksOut + " blocks");
			} catch (CouldntPerformActionEx ex) {
				throw AcsJCouldntPerformActionEx.fromCouldntPerformActionEx(ex);
			}
		}

		// Now wait till all consumers are done ...
		int blocksIn = 0;
		for (CounterConsumer consumer : consumers) {
			String componentName = consumer.name();
			try {
				blocksIn = consumer.waitTillDone();
				m_logger.info(componentName + " has received " + blocksIn + " blocks");
				Assert.assertEquals(blocksOut, blocksIn);
			} catch (CouldntPerformActionEx ex) {
				throw AcsJCouldntPerformActionEx.fromCouldntPerformActionEx(ex);
			}
		}

		// make sure there was enough time to let logs through 
		//Thread.sleep(1000);

	}


	/**
	 * @TODO We usually don't require a main method for a JUnit test to run successfully.
	 * Therefore instead of getting component names from the arg list, 
	 * they should be given in the PROPERTYNAME_COMPONENTNAMES Java property that gets evaluated in the setUp method.
	 */
	public static void main(String[] args)
	{
		// Same as executing "acsStartjava junit.textui.TestRunner alma.COUNTER.CounterTestImpl.CounterTest"
		junit.textui.TestRunner.run(CounterTest.class);
	}

}


