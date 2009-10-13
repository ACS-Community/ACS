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
package alma.acs.alarmsystem.test;

/**
 * A class to test if baci sends BACI double property alarms
 * but it changes the FF and FM too.
 * 
 * The test is performed by getting TEST_COMPONENT component and executing
 * CORBA calls to change the value, FF and FM of its property. 
 * All the alarms received are written in the stdout and then checked by tat
 * 
 * @author acaproni
 *
 */
public class BACIDoublePropCompleteTest extends BACITest {
	
	/**
	 * Constructor
	 */
	public BACIDoublePropCompleteTest() throws Exception {
		super("BACIDoublePropCompleteTest");
	}
	
	/**
	 * Test the sending of alarms by setting the value of a RODouble property
	 * The limits are defined in the CDB.
	 * 
	 * @throws Exception
	 */
	public void testRODouble() throws Exception {
		m_logger.info("Test: No alarms");
		testComponent.setDoubleVar((float)0.0);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}
		
		// HIGH ON
		m_logger.info("Test: HIGH ON");
		testComponent.setDoubleVar((float)400.0);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}

		// HIGH OFF
		m_logger.info("Test: HIGH OFF");
		testComponent.setDoubleVarComplete((float)340.0,"TestFF","TestFM");
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}

		// HIGH ON
		m_logger.info("Test: HIGH ON");
		testComponent.setDoubleVar((float)400.0);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}

		// HIGH OFF
		m_logger.info("Test: HIGH OFF");
		testComponent.setDoubleVar((float)0.0);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}

		// LOW ON
		m_logger.info("Test: LOW ON");
		testComponent.setDoubleVarComplete((float)-200.0,"AnotherFF","TestFaultMember");
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}

		// LOW OFF
		m_logger.info("Test: LOW OFF");
		testComponent.setDoubleVar((float)-90.0);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}

		// LOW ON
		m_logger.info("Test: LOW ON");
		testComponent.setDoubleVar((float)-200.0);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}

		// LOW OFF
		m_logger.info("Test: LOW OFF");
		testComponent.setDoubleVar((float)0.0);
		try {
			Thread.sleep(20000);
		} catch (Exception e) {}
	}
}
