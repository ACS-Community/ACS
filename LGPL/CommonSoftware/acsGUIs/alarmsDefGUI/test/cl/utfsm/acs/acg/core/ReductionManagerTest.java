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

import java.util.Iterator;
import java.util.List;

import cern.laser.business.data.Alarm;
import cern.laser.business.data.AlarmImpl;
import cern.laser.business.data.Triplet;

import junit.framework.TestCase;

public class ReductionManagerTest extends TestCase {
	AcsInformation _acsInfo;
	DAOManager _daoManager;
	AlarmSystemManager _alarmSystemManager;

	public void setUp() throws Exception {
		_acsInfo = new AcsInformation("Test client");
		_daoManager = new DAOManager(_acsInfo.getContainerServices());
		_daoManager.connect();
		UserAuthenticator ua = new UserAuthenticator();
		_alarmSystemManager = AlarmSystemManager.getInstance(ua.authenticate("admin", "admin"));
		_alarmSystemManager.connectToManager();
		_alarmSystemManager.connectToDAL();
	}

	public void testGetInstance() {
		ReductionManager rm1 = ReductionManager.getInstance(_daoManager.getAlarmDAO());
		ReductionManager rm2 = ReductionManager.getInstance(_daoManager.getAlarmDAO());
		assertNotNull(rm1);
		assertNotNull(rm2);
		assertEquals(rm1,rm2);
	}

	public void testGetNodeReductionRules() {
		ReductionManager rm = ReductionManager.getInstance(_daoManager.getAlarmDAO());
		rm.loadFromCDB();
		assertNotNull(rm);
		List<ReductionRule> nrr = rm.getNodeReductionRules();
		assertNotNull(nrr);
		for (ReductionRule reductionRule : nrr) {
			assertNotNull(reductionRule);
		}
	}

	public void testGetMultiReductionRules() {
		ReductionManager rm = ReductionManager.getInstance(_daoManager.getAlarmDAO());
		assertNotNull(rm);
		rm.loadFromCDB();
		List<ReductionRule> mrr = rm.getMultiReductionRules();
		assertNotNull(mrr);
		for (ReductionRule reductionRule : mrr) {
			assertNotNull(reductionRule);
		}
	}

	public void testReloadFromCDB() {
		AlarmManager.getInstance(_daoManager.getAlarmDAO()).loadFromCDB();
		ReductionManager rm = ReductionManager.getInstance(_daoManager.getAlarmDAO());
		assertNotNull(rm);
		rm.loadFromCDB();
		List<ReductionRule> mrr1 = rm.getMultiReductionRules();
		assertNotNull(mrr1);
		rm.loadFromCDB();
		List<ReductionRule> mrr2 = rm.getMultiReductionRules();
		assertNotNull(mrr2);
		assertEquals(mrr1.size(), mrr2.size());
		Iterator<ReductionRule> iterator1 = mrr1.iterator();
		Iterator<ReductionRule> iterator2 = mrr1.iterator();
		ReductionRule r1;
		ReductionRule r2;
		for(;iterator1.hasNext();) {
			r1 = iterator1.next();
			r2 = iterator2.next();
			assertNotNull(r1);
			assertNotNull(r2);
			assertEquals(r1.getIsNodeReduction(), r2.getIsNodeReduction());
			assertEquals(r1.getParent().getTriplet().getFaultFamily(), r2.getParent().getTriplet().getFaultFamily());
			assertEquals(r1.getParent().getTriplet().getFaultMember(), r2.getParent().getTriplet().getFaultMember());
			assertEquals(r1.getParent().getTriplet().getFaultCode(), r2.getParent().getTriplet().getFaultCode());
		}
	}

	public void testSaveToCDB() {
		boolean exception;
		_alarmSystemManager.loadFromCDB();
		ReductionManager rm = _alarmSystemManager.getReductionManager();//ReductionManager.getInstance(_daoManager.getAlarmDAO());
		assertNotNull(rm);
		rm.loadFromCDB();
		exception = false;
		Alarm p = new AlarmImpl();
		Triplet tr = new Triplet("A1", "B1", 1);
		p.setTriplet(tr);
		Alarm c = new AlarmImpl();
		tr = new Triplet("A2", "B2", 2);
		c.setTriplet(tr);
		try {
			rm.addNodeReductionRule(p, c);
			//rm.deleteNodeReductionRule(p, c);
		} catch (IllegalOperationException e) {
			exception = true;
			e.printStackTrace();
		}
		assertFalse(exception);
		rm.saveToCDB();
	}

	public void tearDown() throws Exception {
		ReductionManager.destroy();
		_acsInfo.disconnect();
	}
}
