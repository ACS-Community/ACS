/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) Universidad Tecnica Federico Santa Maria, 2008
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
import java.lang.management.ThreadInfo;

import javax.management.openmbean.CompositeData;

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
	
	protected void setUp() throws Exception {
		
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
	
	protected void tearDown() throws Exception {
		
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
		assertNotNull(mbean.getAllThreadsInfo());
	}

	/*
	 * Test method for 'alma.acs.monitoring.RemoteThreads.getJacORBThreadsCount()'
	 */
	public void testGetJacORBThreadsCount() {
		assertTrue( mbean.getJacORBThreadsCount() >= 0 );
	}

	/*
	 * Test method for 'alma.acs.monitoring.RemoteThreads.getJacORBThreadsInfo()'
	 */
	public void testGetJacORBThreadsInfo() {
		CompositeData []data = null;
		ThreadInfo[] info    = null;
		
		// Get the threads info
		data = mbean.getJacORBThreadsInfo();
		assertNotNull(data);
		
		info = RemoteThreadsUtil.toThreadsInfo(data);
		assertNotNull(info);
		
		System.out.println(mbean.getJacORBThreadsCount());
		// See if these are ACS threads...
		for(int i=0; i!= info.length; i++) {
			int idx = info[i].getStackTrace().length - 1;
			String className = info[i].getStackTrace()[idx].getClassName();
			if( className.equals("java.lang.Thread") ) {
				className = info[i].getStackTrace()[idx - 1].getClassName();
			}
			
			boolean isJacORBThread = false;
			for(int j=0; j!=RemoteThreads.JACORB_CLASSES.length; j++) {
				if( className.startsWith(RemoteThreads.JACORB_CLASSES[j]) ) {
					isJacORBThread = true;
					break;
				}
			}
			assertTrue(isJacORBThread);
		}

	}

	/*
	 * Test method for 'alma.acs.monitoring.RemoteThreads.getJacORBThreadsInfo(State)'
	 */
	public void testGetJacORBThreadsInfoState() {
		CompositeData []data = null;
		ThreadInfo[] info    = null;
		
		// Loop over different thread's states
		for(Thread.State state : Thread.State.values()) {
			
			// Get the threads info for this state
			data = mbean.getJacORBThreadsInfo(state);
			assertNotNull(data);
			
			info = RemoteThreadsUtil.toThreadsInfo(data);
			assertNotNull(info);
			
			// See if these are ACS threads...
			for(int i=0; i!= info.length; i++) {
				int idx = info[i].getStackTrace().length - 1;
				String className = info[i].getStackTrace()[idx].getClassName();
				if( className.equals("java.lang.Thread") ) {
					className = info[i].getStackTrace()[idx - 1].getClassName();
				}
				
				boolean isJacORBThreadAndState = false;
				for(int j=0; j!=RemoteThreads.JACORB_CLASSES.length; j++) {
					if( className.startsWith(RemoteThreads.JACORB_CLASSES[j]) && 
							info[i].getThreadState() == state ) {
						isJacORBThreadAndState = true;
						break;
					}
				}
				assertTrue(isJacORBThreadAndState);
			}
		}
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
		CompositeData []data = null;
		ThreadInfo[] info    = null;
		Thread[] myThreads = new Thread[MAX_THREADS];
		
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
		
		// Get the threads info
		data = mbean.getAcsContainerThreadsInfo();
		assertNotNull(data);
		
		info = RemoteThreadsUtil.toThreadsInfo(data);
		assertNotNull(info);
		
		// See if these are ACS threads...
		for(int i=0; i!= info.length; i++) {
			int idx = info[i].getStackTrace().length - 1;
			String className = info[i].getStackTrace()[idx].getClassName();
			if( className.equals("java.lang.Thread") ) {
				className = info[i].getStackTrace()[idx - 1].getClassName();
			}
			
			boolean isACSThread = false;
			for(int j=0; j!=RemoteThreads.ACS_CLASSES.length; j++) {
				if( className.startsWith(RemoteThreads.ACS_CLASSES[j]) ) {
					isACSThread = true;
					break;
				}
			}
			assertTrue(isACSThread);
		}
		
		//	Finish all the threads
		for(int i=0; i!= MAX_THREADS; i++)
			try {myThreads[i].join();} catch (InterruptedException e) {}
		try { Thread.sleep(1000);} catch (InterruptedException e) {}
	}

	/*
	 * Test method for 'alma.acs.monitoring.RemoteThreads.getAcsContainerThreadsInfo(State)'
	 */
	public void testGetAcsContainerThreadsInfoState() {
		CompositeData []data = null;
		ThreadInfo[] info    = null;
		Thread[] myThreads = new Thread[MAX_THREADS];
		
		// We start MAX_THREADS new threads on this class
		for(int i=0; i!= MAX_THREADS; i++) {
			myThreads[i] = new Thread(new Runnable() {
				public void run() {
					try {
						Thread.sleep(10000);
					} catch (InterruptedException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
			});
			myThreads[i].start();
		}
		try { Thread.sleep(1000);} catch (InterruptedException e) {}
		
		// Loop over thread's state
		for(Thread.State state : Thread.State.values()) {
			// Get the threads info
			data = mbean.getAcsContainerThreadsInfo(state);
			assertNotNull(data);
			
			info = RemoteThreadsUtil.toThreadsInfo(data);
			assertNotNull(info);
			
			// See if these are ACS threads...
			for(int i=0; i!= info.length; i++) {
				int idx = info[i].getStackTrace().length - 1;
				String className = info[i].getStackTrace()[idx].getClassName();
				if( className.equals("java.lang.Thread") ) {
					className = info[i].getStackTrace()[idx - 1].getClassName();
				}
				
				boolean isACSThreadState = false;
				for(int j=0; j!=RemoteThreads.ACS_CLASSES.length; j++) {
					if( className.startsWith(RemoteThreads.ACS_CLASSES[j]) &&
					    info[i].getThreadState() == state) {
						isACSThreadState = true;
						break;
					}
				}
				assertTrue(isACSThreadState);
			}
		}
		//	Finish all the threads
		for(int i=0; i!= MAX_THREADS; i++)
			try {myThreads[i].join();} catch (InterruptedException e) {}
		try { Thread.sleep(1000);} catch (InterruptedException e) {}
	}

}
