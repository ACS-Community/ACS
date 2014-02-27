/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2014 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.containerTests.contHandleTest;

import junit.framework.TestCase;

import si.ijs.maci.ContainerInfo;
import si.ijs.maci.Manager;
import si.ijs.maci.ManagerHelper;

import alma.acs.concurrent.DaemonThreadFactory;
import alma.acs.container.corba.AcsCorba;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;
import alma.acs.util.AcsLocations;

/** 
 * @since ACS 12.3
 */
public class ContainerHandleTest extends TestCase {

	private static final String APP_NAME = ContainerHandleTest.class.getName();
	
	protected AcsCorba acsCorba;
	protected AcsLogger m_logger;
	
	@Override
	protected void setUp() throws Exception {
		m_logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(APP_NAME, false);

		acsCorba = new AcsCorba(m_logger);
		acsCorba.initCorbaForClient(false);
	}

	@Override
	protected void tearDown() throws Exception {
		try
		{
			ClientLogManager.getAcsLogManager().shutdown(true);
		}
		finally
		{
			if (acsCorba != null) {
				acsCorba.shutdownORB(true, false);
				// as a workaround for this problem, for now we run this async with a timeout
				Thread destroyThread = (new DaemonThreadFactory("OrbDestroy")).newThread(new Runnable() {
					public void run() {
						acsCorba.doneCorba();
					}
				});
				destroyThread.start();
				destroyThread.join(20000);
			}
			// just in case... should give the OS time to reclaim ORB ports and so on
			Thread.sleep(1000);
		}
	}

	public void testContainerHandles() throws Throwable
	{
		String managerRef = AcsLocations.figureOutManagerLocation();
		
		m_logger.info("Using manager reference: " + managerRef);
		
		org.omg.CORBA.Object mgrObj = acsCorba.getORB().string_to_object(managerRef);
		Manager manager = ManagerHelper.narrow(mgrObj);

		m_logger.info("Manager reference resolved and narrowed.");

		final int MANAGER_HANDLE = 0x05555555;

		m_logger.info("Querying manager for container info.");

		ContainerInfo[] infos = manager.get_container_info(MANAGER_HANDLE, new int[0], "*");

		m_logger.info("Got infos from " + infos.length + " container(s).");
	
		for (ContainerInfo info : infos)
		{
			int h = info.reference.get_handle();
			m_logger.info("Manager reported handle 0x" + Integer.toHexString(info.h) + ", get_handle() returned 0x" + Integer.toHexString(h));
			assertEquals(info.name + " handle mismatch.", info.h, h);
		}
	}
		
}

