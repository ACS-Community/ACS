package cl.utfsm.acs.acg.core;

import com.cosylab.acs.laser.dao.ACSAlarmDAOImpl;

import cern.laser.business.data.Source;
import junit.framework.TestCase;

public class SourceManagerTest extends TestCase {


	AlarmSystemManager _alarmSystemManager;
	
	protected void setUp() throws Exception {
		_alarmSystemManager = AlarmSystemManager.getInstance(UserAuthenticator.Role.Administrator);
		_alarmSystemManager.connectToManager();
		_alarmSystemManager.connectToDAL();
		_alarmSystemManager.getAlarmManager().loadFromCDB();
	}

	public void testGetInstance() throws Exception {

		_alarmSystemManager.disconnectFromManager();
		AlarmSystemManager.destroy();

		AcsInformation _acsInfo;
		DAOManager _daoManager;
		_acsInfo = new AcsInformation("Test client");
		_daoManager = new DAOManager(_acsInfo.getContainerServices());
		_daoManager.connect();
		((ACSAlarmDAOImpl)_daoManager.getAlarmDAO()).loadAlarms();
		
		
		SourceManager s1 = SourceManager.getInstance(_daoManager.getSourceDAO());
		SourceManager s2 = SourceManager.getInstance(_daoManager.getSourceDAO());
		assertNotNull(s1);
		assertNotNull(s2);
		assertEquals(s1, s2);
		
		_acsInfo.disconnect();
		_alarmSystemManager = AlarmSystemManager.getInstance(UserAuthenticator.Role.Administrator);
		_alarmSystemManager.connectToManager();
		_alarmSystemManager.connectToDAL();
		
		
	}

	public void testGetAllSources() {
		SourceManager s1 = _alarmSystemManager.getSourceManager();
		assertNotNull(s1);
		s1.loadFromCDB();
		Source [] sources = s1.getAllSources();
		for (int i = 0; i < sources.length; i++) {
			assertNotNull(sources[i]);
			assertNotNull(sources[i].getSourceId());
			assertNotNull(sources[i].getName());
		}
	}

	public void testGetSource() {
		SourceManager s1 = _alarmSystemManager.getSourceManager();
		assertNotNull(s1);
		s1.loadFromCDB();
		Source [] sources = s1.getAllSources();
		for (int i = 0; i < sources.length; i++) {
			Source s = s1.getSource(sources[i].getName());
			assertNotNull(s);
			assertEquals(s.getName(), sources[i].getName());
			assertEquals(s.getSourceId(), sources[i].getSourceId());
		}
	}

	public void testReloadFromCDB() {
		SourceManager s1 = _alarmSystemManager.getSourceManager();
		assertNotNull(s1);
		s1.loadFromCDB();
		Source []sources1 = s1.getAllSources();
		s1.loadFromCDB();
		Source []sources2 = s1.getAllSources();
		assertNotNull(sources1);
		assertNotNull(sources2);
		
		for (int i = 0; i < sources1.length; i++) {
			Source source1 = sources1[i];
			Source source2 = sources1[i];
			assertNotNull(source1);
			assertNotNull(source2);
			assertEquals(source1.getName(), source2.getName());
			assertEquals(source1.getSourceId(), source2.getSourceId());
		}
	}
	
	public void testAddSource() throws Exception {
		SourceManager sm = _alarmSystemManager.getSourceManager();
		assertNotNull(sm);
		
		Source[] sources1 = sm.getAllSources();
		assertNotNull(sources1);
		
		Source newSource = new Source();
		newSource.setSourceId("NEW SOURCE");
		sm.addSource(newSource);
		
		Source[] sources2 = sm.getAllSources();
		assertNotNull(sources2);
		assertEquals(sources1.length + 1, sources2.length);

		boolean check = false;
		for (int i = 0; i < sources2.length; i++) {
			if (sources2[i].getName().compareTo("NEW SOURCE") == 0){
				check = true;
			}
		}
		assertTrue(check);

		check = false;
		try {
			sm.addSource(newSource);
		} catch (IllegalOperationException e) {
			check = true;
		}
		assertTrue(check);

	}
	
	public void testDeleteSource() throws Exception {
		SourceManager sm = _alarmSystemManager.getSourceManager();
		assertNotNull(sm);
		sm.loadFromCDB();
		
		Source[] sources1 = sm.getAllSources();
		assertNotNull(sources1);

		// Check if we cannot delete an not existent source
		Source newSource = new Source();
		newSource.setSourceId("NEW SOURCE");
		boolean deleted = false;
		
		deleted = sm.deleteSource(newSource);
		assertFalse(deleted);
		
		Source[] sources2 = sm.getAllSources();
		assertNotNull(sources2);
		assertEquals(sources1.length,  sources2.length);

		// Check if we can delete an existing source
		boolean check = false;
		deleted = false;
		sources1 = sm.getAllSources();
		Source deletedSource = sources1[0];
		try {
			deleted = sm.deleteSource(deletedSource);
		} catch(IllegalOperationException e){
			check = true;
		}
		assertFalse(check);
		assertTrue(deleted);
		sources2 = sm.getAllSources();
		assertNotNull(sources2);
		assertEquals(sources1.length - 1,  sources2.length);

		check = false;
		for (int i = 0; i < sources2.length; i++) {
			assertNotNull(sources2[i]);
			if (sources2[i].getName().compareTo(deletedSource.getName()) == 0){
				check = true;
			}
		}
		assertFalse(check);
		
		// Check if the source is already in use by a FaultFamily
		
		newSource = new Source();
		newSource.setSourceId("NEW SOURCE");
		sm.addSource(newSource);
		check = false;
		deleted = false;
		
		try {
			deleted =  sm.deleteSource(newSource);
		} catch(IllegalOperationException e){
			check = true;
		}
		assertFalse(check);
		assertTrue(deleted);
		
				
	}
	

	protected void tearDown() throws Exception {
		//_acsInfo.disconnect();
		//SourceManager.destroy();
		
		//_alarmSystemManager.disconnectFromManager();
		AlarmSystemManager.destroy();
		CategoryManager.destroy();
		
		
		
	}
}
