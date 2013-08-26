/*
 *    ALMA - Atacama Large Millimeter Array
 *    (c) Universidad Tecnica Federico Santa Maria, 2009
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
package cl.utfsm.acs.acg.core;

import java.util.List;

import cern.laser.business.data.Alarm;
import cern.laser.business.data.AlarmImpl;
import junit.framework.TestCase;

public class ReductionRuleTest extends TestCase {

	public void testReductionRule() {
		Alarm alarm = new AlarmImpl();
		ReductionRule rr = new ReductionRule(alarm);
		assertNotNull(rr);
	}

	public void testAddGetChildren() {
		Alarm alarm  = new AlarmImpl();
		ReductionRule rr = new ReductionRule(alarm);

		Alarm[] alarms = new Alarm[10];
		for(int i=0;i!=10;i++) {
			alarms[i] = new AlarmImpl();
			rr.addChild(alarms[i]);
		}

		List<Alarm> children = rr.getChildren();
		assertEquals(children.size(), 10);
		for(int i=0;i!=10;i++) {
			assertTrue(children.contains(alarms[i]));
		}

	}

	public void testSetIsNodeReduction() {
		Alarm alarm = new AlarmImpl();
		ReductionRule rr = new ReductionRule(alarm);
		rr.setIsNodeReduction(true);
		boolean v = rr.getIsNodeReduction();
		assertEquals(v,true);
		rr.setIsNodeReduction(false);
		v = rr.getIsNodeReduction();
		assertEquals(v,false);
	}

	public void testSetTreshold() {
		Alarm alarm = new AlarmImpl();
		ReductionRule rr = new ReductionRule(alarm);
		rr.setThreshold(10);
		int v = rr.getThreshold();
		assertEquals(v,10);
		rr.setThreshold(2);
		v = rr.getThreshold();
		assertEquals(v,2);
	}

	public void testGetParent() {
		Alarm alarm = new AlarmImpl();
		ReductionRule rr = new ReductionRule(alarm);
		assertNotNull(rr);
		Alarm alarm2 = rr.getParent();
		assertNotNull(alarm2);
		assertEquals(alarm,alarm2);
	}

}