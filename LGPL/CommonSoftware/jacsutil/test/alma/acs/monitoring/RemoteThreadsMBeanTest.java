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
package alma.acs.monitoring;

import java.lang.management.ManagementFactory;

import junit.framework.TestCase;

/**
 * Test class for the RemoteThreadMBean implementation
 * @author rtobar
 * @since ACS 7.0
 */
public class RemoteThreadsMBeanTest extends TestCase {

	private final int MAX_THREADS     = 100;
	private RemoteThreadsClient rtc   = null;
	private RemoteThreadsMBean  mbean = null;
	
	public void setUp() throws Exception {
		
		boolean throwedException = false;
		
		try {
			rtc = new RemoteThreadsClient(
					Integer.valueOf(
							ManagementFactory.getRuntimeMXBean().getName().split("@")[0]
					));
			assertTrue(rtc.connect());
			mbean = rtc.getMBean();
		} catch (RemoteThreadsException e) {
			throwedException = true;
		}

		assertTrue(!throwedException);
		assertNotNull(rtc);
		assertNotNull(mbean);
	}
	
	public void tearDown() throws Exception {
		
		boolean throwedException = false;
		
		try {
			rtc.close();			
		} catch (RemoteThreadsException e) {
			throwedException = true;
		}
		
		assertTrue(!throwedException);
	}
	
	/*
	 * Test method for 'alma.acs.monitoring.RemoteThreads.getAllThreadsCount()'
	 */
	public void testGetAllThreadsCount() {
		
		assertTrue(mbean.getAllThreadsCount() >= 
			mbean.getAcsContainerThreadsCount() + mbean.getJacORBThreadsCount());
		
	}

	/*
	 * Test method for 'alma.acs.monitoring.RemoteThreads.getAllThreadsInfo()'
	 */
	public void testGetAllThreadsInfo() {

	}

	/*
	 * Test method for 'alma.acs.monitoring.RemoteThreads.getJacORBThreadsCount()'
	 */
	public void testGetJacORBThreadsCount() {
		
	}

	/*
	 * Test method for 'alma.acs.monitoring.RemoteThreads.getJacORBThreadsInfo()'
	 */
	public void testGetJacORBThreadsInfo() {

	}

	/*
	 * Test method for 'alma.acs.monitoring.RemoteThreads.getJacORBThreadsInfo(State)'
	 */
	public void testGetJacORBThreadsInfoState() {

	}

	/*
	 * Test method for 'alma.acs.monitoring.RemoteThreads.getAcsContainerThreadsCount()'
	 */
	public void testGetAcsContainerThreadsCount() {

		Thread[] myThreads = new Thread[MAX_THREADS];
		
		// Count the initial ACS' threads (should be zero)
		// If the test runner is ACS's then we'll have one thread more
		assertTrue(
				mbean.getAcsContainerThreadsCount() == 1 ||
				mbean.getAcsContainerThreadsCount() == 0 );
		
		// We start MAX_THREADS new threads on this class
		for(int i=0; i!= MAX_THREADS; i++) {
			myThreads[i] = new Thread(new Runnable() {
				public void run() {
					try {
						Thread.sleep(5000);
					} catch (InterruptedException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
			});
			myThreads[i].start();
		}
		try { Thread.sleep(1000);} catch (InterruptedException e) {}
		
		// Count the threads (they should be MAX_THREADS)
		// If the test runner is ACS', then we'll have one thread more
		assertTrue(
				mbean.getAcsContainerThreadsCount() == MAX_THREADS ||
				mbean.getAcsContainerThreadsCount() == MAX_THREADS + 1 );

		// Finish all the threads
		for(int i=0; i!= MAX_THREADS; i++)
			try {myThreads[i].join();} catch (InterruptedException e) {}
		try { Thread.sleep(1000);} catch (InterruptedException e) {}
			
		// Count the final ACS' threads (should be zero)
		// If the test runner is ACS's then we'll have one thread more
		assertTrue(
				mbean.getAcsContainerThreadsCount() == 1 ||
				mbean.getAcsContainerThreadsCount() == 0 );
	}

	/*
	 * Test method for 'alma.acs.monitoring.RemoteThreads.getAcsContainerThreadsInfo()'
	 */
	public void testGetAcsContainerThreadsInfo() {

	}

	/*
	 * Test method for 'alma.acs.monitoring.RemoteThreads.getAcsContainerThreadsInfo(State)'
	 */
	public void testGetAcsContainerThreadsInfoState() {

	}

}
