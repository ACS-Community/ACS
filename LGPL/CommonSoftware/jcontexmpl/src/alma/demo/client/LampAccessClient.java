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
package alma.demo.client;

import alma.demo.LampAccess;
import alma.demo.LampUnavailable;
import alma.acs.component.client.ComponentClientTestCase;

/**
 * JUnit test client for LampAccessComponent.
 * 
 * @author radi
 */
public class LampAccessClient extends ComponentClientTestCase
{
	private LampAccess m_comp;
	
	/**
	 * Creates a component with a name.
	 * @see java.lang.Object#Object()
	 */
	public LampAccessClient() throws Exception
	{
		super("LampAccessClient");
	}

	/**
	 * Sets up the environment and gets a reference to the LampAccess component.
	 * This method is called before the test is executed.
	 * @see junit.framework.TestCase#setUp()
	 */
	protected void setUp() throws Exception
	{		
		super.setUp();
		org.omg.CORBA.Object compObj = getContainerServices().getComponent("LAMPACCESS1");
		assertNotNull(compObj);
		m_comp = alma.demo.LampAccessHelper.narrow(compObj);
	}
	
	/**
	 * Compares the default brightness value with zero. 
	 */ 
	public void testGetLampBrightness() throws LampUnavailable
	{
		double br = m_comp.getLampBrightness();
		String b = "" + br;
		assertEquals("not equal.", "0", b);
	}
	
	/**
	 * Compares the set brightness value with the expected.
	 */
	public void testSetLampBrightness() throws LampUnavailable
	{
		m_comp.setLampBrightness(0.0);
		String expected = "" + 0.0;
		double brightness = m_comp.getLampBrightness();
		String actual = "" + brightness;
		assertEquals(expected, actual);
	}
}
