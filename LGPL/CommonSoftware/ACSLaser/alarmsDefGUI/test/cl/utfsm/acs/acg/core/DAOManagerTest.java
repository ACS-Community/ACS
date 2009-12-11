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

import cern.laser.business.dao.AlarmDAO;
import cern.laser.business.dao.CategoryDAO;
import cern.laser.business.dao.SourceDAO;

import junit.framework.TestCase;

public class DAOManagerTest extends TestCase {

	AcsInformation _acsInfo;
	DAOManager _daoManager;

	public void setUp() throws Exception {
		_acsInfo = new AcsInformation("Test client");
		_daoManager = new DAOManager(_acsInfo.getContainerServices());
		return;
	}

	public void testDAOManager() throws Exception {
 
		assertNotNull(_daoManager);
		return;
	}

	public void testConnect() throws Exception {

		assertNotNull(_daoManager);
		_daoManager.connect();

		boolean exception = false;
		try {
			_daoManager = new DAOManager(null);
			_daoManager.connect();
		} catch (Exception e) {
			exception = true;
		}
		assertTrue(exception);
	}

	public void testGetAlarmDAO() throws Exception {

		AlarmDAO tmp1;
		AlarmDAO tmp2;

		boolean exception = false;
		try {
			tmp1 = _daoManager.getAlarmDAO();
		} catch (IllegalStateException e) {
			exception = true;
		}
		assertTrue(exception);
		
		_daoManager.connect();
		tmp1 = _daoManager.getAlarmDAO();
		assertNotNull(tmp1);
		tmp2 = _daoManager.getAlarmDAO();
		assertNotNull(tmp2);
		assertEquals(tmp1, tmp2);

	}

	public void testGetSourceDAO() throws Exception {

		boolean exception = false;
		try {
			_daoManager.getSourceDAO();
		} catch (IllegalStateException e) {
			exception = true;
		}
		assertTrue(exception);
		
		_daoManager.connect();
		SourceDAO tmp1 = null;
		SourceDAO tmp2 = null;
		assertNotNull(_daoManager.getAlarmDAO());
		tmp1 = _daoManager.getSourceDAO();
		assertNotNull(tmp1);
		tmp2 = _daoManager.getSourceDAO();
		assertNotNull(tmp2);
		assertEquals(tmp1,tmp2);

	}

	public void testGetCategoryDAO() throws Exception {
		
		boolean exception = false;
		try {
			_daoManager.getCategoryDAO();
		} catch (IllegalStateException e) {
			exception = true;
		}
		assertTrue(exception);
		
		exception = false;
		_daoManager.connect();
		try {
			_daoManager.getCategoryDAO();
		} catch(IllegalStateException e) {
			exception = true;
		}
		assertFalse(exception);

		CategoryDAO tmp1;
		CategoryDAO tmp2;
		tmp1 = _daoManager.getCategoryDAO();
		assertNotNull(tmp1);
		tmp2 = _daoManager.getCategoryDAO();
		assertNotNull(tmp2);

	}
	
	public void testBackupCDB(){
		_daoManager.backupCDB();
	}

	public void tearDown() throws Exception {
		_acsInfo.disconnect();
	}
}
