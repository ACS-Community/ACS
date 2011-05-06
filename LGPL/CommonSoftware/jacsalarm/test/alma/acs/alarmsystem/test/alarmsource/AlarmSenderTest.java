/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2011
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
package alma.acs.alarmsystem.test.alarmsource;

import java.util.Properties;

import alma.acs.alarmsystem.source.AlarmSender;
import alma.acs.component.client.ComponentClientTestCase;

/**
 * Test the {@link AlarmSender} class checking if the class sends
 * the alarms.
 * <P>
 * <EM>Note</EM>: in this module it is possible to send the alarm sending only 
 * with the ACS implementation of the alarm service.
 * 
 * @author acaproni
 *
 */
public class AlarmSenderTest extends ComponentClientTestCase {
	
	/**
	 * The object to test
	 */
	private AlarmSender alarmSender;
	
	/**
	 * Constructor
	 * 
	 * @throws Exception
	 */
	public AlarmSenderTest() throws Exception {
		super(AlarmSenderTest.class.getName());
	}
	
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		alarmSender=new AlarmSender(getContainerServices());
		assertNotNull(alarmSender);
	}

	@Override
	protected void tearDown() throws Exception {
		super.tearDown();
		alarmSender.close();
	}
	
	/**
	 * Test the sending of an alarm
	 * 
	 * @throws Exception
	 */
	public void testAlarmSending() throws Exception {
		alarmSender.sendAlarm("TestFF", "TestFM1", 0, true);
		Properties testProps = new Properties();
		testProps.put("Key", "PropertyValue");
		alarmSender.sendAlarm("TestFF", "TestFM2", 1, testProps, false);
		
	}
	
}
