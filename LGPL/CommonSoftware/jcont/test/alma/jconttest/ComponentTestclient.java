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
package alma.jconttest;

import org.omg.CORBA.StringHolder;

import alma.acs.component.client.ComponentClientTestCase;

/**
 * @author hsommer
 * created Sep 17, 2004 3:49:45 PM
 */
public class ComponentTestclient extends ComponentClientTestCase
{
	private static final String CONTSRVCOMP_INSTANCE = "CONT_SERVICES_TESTER";
	
	private ContainerServicesTester m_contSrvTesterComp;
	
	public ComponentTestclient() throws Exception
	{
		super("ComponentTestclient");
	}

	protected void setUp() throws Exception
	{
		super.setUp();
		org.omg.CORBA.Object compObj = getContainerServices().getComponent(CONTSRVCOMP_INSTANCE);
		assertNotNull(compObj);
		m_contSrvTesterComp = ContainerServicesTesterHelper.narrow(compObj);
	}

	public void testComponentName() {
		StringHolder nameHolder = new StringHolder();
		boolean ret = m_contSrvTesterComp.testComponentName(nameHolder);
		assertTrue("test execution successful on the server component", ret);		
		assertEquals(CONTSRVCOMP_INSTANCE, nameHolder.value);
	}
	
	public void testStateManager() {
		StringHolder stateNameHolder = new StringHolder();
		boolean ret = m_contSrvTesterComp.testStateManager(stateNameHolder);
		assertTrue("test execution successful on the server component", ret);		
		assertEquals("OPERATIONAL", stateNameHolder.value);
	}
	
	public void testGetDynamicDummyComponent() {
		StringHolder compNameHolder = new StringHolder();
		boolean ret = m_contSrvTesterComp.testGetDynamicDummyComponent(compNameHolder);
		assertTrue("test execution successful on the server component", ret);
		System.out.println("got dummy component called " + compNameHolder.value);
//		assertEquals("OPERATIONAL", compNameHolder.value);
	}

    public void testGetThreadFactory() {
        boolean ret = m_contSrvTesterComp.testGetThreadFactory(40, 100000, true);
        assertTrue("test execution successful on the server component", ret);
    }

}
