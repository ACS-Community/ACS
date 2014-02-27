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
		super.setUp();
	}

	@Override
	protected void tearDown() throws Exception {
		super.tearDown();
	}

	public void testContainerHandles() throws Throwable
	{
		System.out.println("Test started");
	}
		
}

