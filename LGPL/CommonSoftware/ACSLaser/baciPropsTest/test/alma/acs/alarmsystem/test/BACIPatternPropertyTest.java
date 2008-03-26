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
 * A class to test if baci sends BACI pattern property alarms.
 * 
 * The test is performed by getting TEST_COMPONENT component and executing
 * CORBA calls to change the value of its property. 
 * All the alarms received are written in the stdout and then checked by tat
 * 
 * @author acaproni
 *
 */
public class BACIPatternPropertyTest extends BACITest {
	
	/**
	 * Constructor
	 */
	public BACIPatternPropertyTest() throws Exception {
		super("BACIPatternPropertyTest");
	}
	
	/**
	 * Test the sending of alarms for a pattern property
	 */
	public void testROPattern() throws Exception {
		// No alarm
		testComponent.setPatternVar(0);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}
		// No alarm
		testComponent.setPatternVar(1);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}
		// Alarm
		testComponent.setPatternVar(2);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}
		// No alarm
		testComponent.setPatternVar(3);
		try {
			Thread.sleep(5000);
		} catch (Exception e) {}
		// No alarm
		testComponent.setPatternVar(4);
		try {
			Thread.sleep(20000);
		} catch (Exception e) {}
	}
}
