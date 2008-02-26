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
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.List;

import junit.framework.TestCase;

/**
 * @author rtobar
 * created Feb 18, 2008 2:06:46 PM
 */
public class RemoteThreadsClientTest extends TestCase {

	private RemoteThreadsClient rtc = null;
	
	protected void setUp() throws Exception {
		super.setUp();
		
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	/*
	 * Test method for 'alma.acs.monitoring.RemoteThreadsClient.RemoteThreadsClient(String)'
	 */
	public void testRemoteThreadsClientString() {
		boolean throwedException = false;
		
		// We know that java.lang.String has now main method!
		try {
			rtc = new RemoteThreadsClient("java.lang.String");
		} catch(RemoteThreadsException e) {
			throwedException = true;
		}
		
		assertTrue("Can't connect to java.lang.String",throwedException);

		// Let's try connecting to the class ""
		throwedException = false;
		try {
			rtc = new RemoteThreadsClient("");
		} catch(RemoteThreadsException e) {
			throwedException = true;
		}
		
		assertTrue("Can't connect to the \"\" class",throwedException);
		
		List<String> lala = ManagementFactory.getRuntimeMXBean().getInputArguments();
		for(int i =0; i!= lala.size(); i++)
			System.out.println(lala.get(i));
	}

	/*
	 * Test method for 'alma.acs.monitoring.RemoteThreadsClient.RemoteThreadsClient(int)'
	 */
	public void testRemoteThreadsClientInt() {
		
		boolean throwedException = false;
		try {
			rtc = new RemoteThreadsClient(0);
		} catch (RemoteThreadsException e) {
			throwedException = true;
		}
		assertTrue("PID can't be 0", throwedException);
		
		throwedException = false;
		try {
			rtc = new RemoteThreadsClient(
					Integer.valueOf(
							ManagementFactory.getRuntimeMXBean().getName().split("@")[0]
					));
		} catch (RemoteThreadsException e) {
			throwedException = true;
		}
		
		assertTrue("PID of the current application", !throwedException );
		assertNotNull(rtc);
	}

	/*
	 * Test method for 'alma.acs.monitoring.RemoteThreadsClient.RemoteThreadsClient(InetAddress)'
	 */
	public void testRemoteThreadsClientInetAddress() {
		
		rtc = null;
		boolean remoteThreadsException = false;
		boolean unknownHostException   = false;
		
		try {
			InetAddress localhost = InetAddress.getByName("localhost");
			rtc = new RemoteThreadsClient(localhost);
		} catch (RemoteThreadsException e) {
			remoteThreadsException = true;
		} catch (UnknownHostException e) {
			unknownHostException = true;
		}
		
		assertTrue(!remoteThreadsException);
		assertTrue(!unknownHostException);
		assertNotNull(rtc);
	}
	
	/*
	 * Test method for 'alma.acs.monitoring.RemoteThreadsClient.RemoteThreadsClient(InetAddress, int)'
	 */
	public void testRemoteThreadsClientInetAddressInt() {

		rtc = null;
		boolean remoteThreadsException = false;
		
		// Let's try with a negative port...
		try {
			InetAddress localhost = InetAddress.getByName("localhost");
			rtc = new RemoteThreadsClient(localhost,-12);
		} catch (RemoteThreadsException e) {
			remoteThreadsException = true;
		} catch (UnknownHostException e) {
		}
		
		assertTrue(remoteThreadsException);
		assertNull(rtc);
		
		// Now with a good port...
		remoteThreadsException = false;
		try {
			InetAddress localhost = InetAddress.getByName("localhost");
			rtc = new RemoteThreadsClient(localhost,1234);
		} catch (RemoteThreadsException e) {
			remoteThreadsException = true;
		} catch (UnknownHostException e) {
		}
		
		assertTrue(!remoteThreadsException);
		assertNotNull(rtc);
	}

	/*
	 * Test method for 'alma.acs.monitoring.RemoteThreadsClient.connect()'
	 */
	public void testConnect() {
		
		boolean throwedException = false; 
		
		rtc = null;
		
		// Connect to this process and get the mbean
		try {
			rtc = new RemoteThreadsClient(
					Integer.valueOf(
							ManagementFactory.getRuntimeMXBean().getName().split("@")[0]
					));
		} catch (RemoteThreadsException e) {
			throwedException = true;
		}
		
		assertTrue("Connection to the MBean unsucessful",rtc.connect());
		assertTrue(!throwedException);
	}

	/*
	 * Test method for 'alma.acs.monitoring.RemoteThreadsClient.getMBean()'
	 */
	public void testGetMBean() {
		
		RemoteThreadsMBean mbean = null;
		boolean throwedException = false; 
		
		rtc = null;
		
		// Connect to this process and get the mbean
		try {
			rtc = new RemoteThreadsClient(
					Integer.valueOf(
							ManagementFactory.getRuntimeMXBean().getName().split("@")[0]
					));
			rtc.connect();
			mbean = rtc.getMBean();
			rtc.close();
		} catch (RemoteThreadsException e) {
			throwedException = true;
		}
		
		assertNotNull(rtc);
		assertNotNull(mbean);
		assertTrue(!throwedException);
	}

	/*
	 * Test method for 'alma.acs.monitoring.RemoteThreadsClient.close()'
	 */
	public void testClose() {
		
		boolean throwedException = false; 
		rtc = null;
		
		// Create the client and close it (should throw exception)
		try {
			rtc = new RemoteThreadsClient(
					Integer.valueOf(
							ManagementFactory.getRuntimeMXBean().getName().split("@")[0]
					));
			rtc.close();
		} catch (RemoteThreadsException e) {
			throwedException = true;
		}
		
		assertTrue(throwedException);
		
		// Let's do it as it should have been done
		throwedException = false;
		try {
			rtc = new RemoteThreadsClient(
					Integer.valueOf(
							ManagementFactory.getRuntimeMXBean().getName().split("@")[0]
					));
			rtc.connect();
			rtc.close();
		} catch (RemoteThreadsException e) {
			throwedException = true;
		}
		
		assertTrue(!throwedException);
	}
}
