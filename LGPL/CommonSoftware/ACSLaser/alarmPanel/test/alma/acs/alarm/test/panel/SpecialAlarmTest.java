/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2010
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acs.alarm.test.panel;

import java.util.Properties;

import cern.laser.client.data.Alarm;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acsplugins.alarmsystem.gui.specialalarm.SpecialAlarm;
/**
 * Test if the SpecialAlarm matches the specifications.
 * 
 * @author acaproni
 */
public class SpecialAlarmTest extends ComponentClientTestCase {
	
	// The fields to build/test the alarm
	private static final String family="FaultFamily";
	private static final String member="Component";
	private static final int code=1;
	private static final String action="The action to fix"; 
	private static final String description="Description of the alarm" ;
	private static final String cause="Possible cause";
	private static final String consequences="What else?";
	private static final int    priority=1;
	private static final String url="http://www.eso.org"; 
	private static final String email="person@host.org"; 
	private static final Properties props=new Properties();
	
	private static final String prop1Key="Property 1 - key";
	private static final String prop1Val="Value1";
	private static final String prop2Key="Property 2 - key";
	private static final String prop2Val="Value2";
	
	/**
	 * Constructor
	 * 
	 * @throws Exception
	 */
	public SpecialAlarmTest()throws Exception {
		super("SpecialAlarmTest");
	}
	
	
	/**
	 * @see alma.acs.component.client.ComponentClientTestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		
		props.put(prop1Key, prop1Val);
		props.put(prop2Key, prop2Val);
		assertEquals(props.size(),2);
	}


	/**
	 * Test the creation of the alarm when the URL is <code>null</code>.
	 */
	public void testCreateSpecialAlarmWronglURL() throws Exception {
		Alarm alarmNull = new SpecialAlarm(
				family, 
				member, 
				code, 
				true, 
				action, 
				description, 
				cause, 
				consequences, 
				priority, 
				null, 
				email, 
				props);
		assertNull(alarmNull.getHelpURL());
		Alarm alarmEmpty = new SpecialAlarm(
				family, 
				member, 
				code, 
				true, 
				action, 
				description, 
				cause, 
				consequences, 
				priority, 
				"", 
				email, 
				props);
		assertNull(alarmEmpty.getHelpURL());
	}
	
	public void testSpecialAlarmTest() throws Exception {
		Alarm alarm = new SpecialAlarm(
				family, 
				member, 
				code, 
				true, 
				action, 
				description, 
				cause, 
				consequences, 
				priority, 
				url, 
				email, 
				props);
		assertEquals(alarm.getTriplet().getFaultFamily(),family);
		assertEquals(alarm.getTriplet().getFaultMember(),member);
		assertEquals(alarm.getTriplet().getFaultCode(),Integer.valueOf(code));
		assertEquals(alarm.getHelpURL().toString(),url);
		assertEquals(alarm.getAction(),action);
		assertEquals(alarm.getCause(),cause);
		assertEquals(alarm.getConsequence(),consequences);
		assertEquals(alarm.getPiquetEmail(),email);
		assertEquals(alarm.getProblemDescription(),description);
		assertEquals(alarm.getPriority(),Integer.valueOf(priority));
		assertEquals(alarm.getStatus().isActive(),true);
		assertEquals(family+":"+member+":"+code,alarm.getAlarmId());
		Properties p=alarm.getStatus().getUserProperties();
		assertNotNull(p);
		assertTrue(p.size()==2);
		String v1=p.getProperty(prop1Key);
		assertEquals(prop1Val,v1);
		String v2=p.getProperty(prop2Key);
		assertEquals(prop2Val,v2);
	}
	
	public void testSetState() throws Exception {
		SpecialAlarm alarm = new SpecialAlarm(
				family, 
				member, 
				code, 
				true, 
				action, 
				description, 
				cause, 
				consequences, 
				priority, 
				url, 
				email, 
				props);
		alarm.setStatus(false);
		assertFalse(alarm.getStatus().isActive());
		alarm.setStatus(true);
		assertTrue(alarm.getStatus().isActive());
	}
	
	public void testSetProperties() throws Exception {
		SpecialAlarm alarm = new SpecialAlarm(
				family, 
				member, 
				code, 
				true, 
				action, 
				description, 
				cause, 
				consequences, 
				priority, 
				url, 
				email, 
				props);
		String k="TheKey";
		String v="ValVal";
		Properties newP = new Properties();
		newP.put(k, v);
		alarm.setUserProperties(newP);
		Properties p = alarm.getStatus().getUserProperties();
		assertNotNull(p);
		assertTrue(p.size()==1);
		Object val = p.get(k);
		assertNotNull(val);
		assertEquals(val.toString(), v);
	}
}
