package cl.utfsm.acs.acg.dao;

import alma.acs.alarmsystem.generated.Contact;
import alma.acs.alarmsystem.generated.FaultCode;
import alma.acs.alarmsystem.generated.FaultFamily;
import alma.acs.alarmsystem.generated.FaultMember;
import alma.acs.alarmsystem.generated.FaultMemberDefault;
import alma.acs.alarmsystem.generated.Location;
import alma.alarmsystem.alarmmessage.generated.ReductionDefinitions;
import cern.laser.business.data.Alarm;

import alma.alarmsystem.alarmmessage.generated.AlarmDefinition;
import alma.alarmsystem.alarmmessage.generated.Child;
import alma.alarmsystem.alarmmessage.generated.Parent;
import alma.alarmsystem.alarmmessage.generated.ReductionLinkType;
import cl.utfsm.acs.acg.core.AcsInformation;
import cl.utfsm.acs.acg.core.AlarmManager;
import cl.utfsm.acs.acg.core.DAOManager;
import junit.framework.TestCase;

public class ACSAlarmDAOImplTest extends TestCase{
	AcsInformation _acsInfo;
	DAOManager _daoManager;
	AlarmManager _am;
	ACSAlarmDAOImpl _alarmDAO;

	public void setUp() throws Exception {
		_acsInfo = new AcsInformation("Test client");
		_daoManager = new DAOManager(_acsInfo.getContainerServices());
		_daoManager.connect();
		_alarmDAO = (ACSAlarmDAOImpl)_daoManager.getAlarmDAO();
	}
	
	public void testAddFaultFamily(){
		boolean exception;
		Alarm al1;
		cern.laser.business.data.Location l;
		
		//Delete Entry if it exists due to an error in previous test
		{
			FaultFamily ff = new FaultFamily();
			ff.setName("ffTest1");
			try{
				_alarmDAO.removeFaultFamily(ff);
			}catch(Exception e){
				//This happens when FaultFamily "ffTest1" doesn't exist
			}
		}
		
		//Check Null Argument
		exception = false;
		try{
			_alarmDAO.addFaultFamily(null);
		}catch(Exception e){
			exception = true;
		}
		assertTrue(exception);
		
		//Check new FaultFamily
		FaultFamily ff = new FaultFamily();
		ff.setName("ffTest1");
		ff.setAlarmSource("ALARM_SYSTEM_SOURCES");
		ff.setHelpUrl("http://www.test.com");
		Contact ct = new Contact();
		ct.setEmail("em1");
		ct.setGsm("gsm1");
		ct.setName("cont1");
		ff.setContact(ct);
		FaultCode fc = new FaultCode();
		fc.setValue(1);
		fc.setPriority(0);
		fc.setAction("action1");
		fc.setCause("cause1");
		fc.setConsequence("conseq1");
		fc.setProblemDescription("problem1");
		ff.addFaultCode(fc);
		FaultMemberDefault fmd = new FaultMemberDefault();
		Location lc = new Location();
		lc.setBuilding("b1");
		lc.setFloor("f1");
		lc.setMnemonic("m1");
		lc.setPosition("p1");
		lc.setRoom("r1");
		fmd.setLocation(lc);
		ff.setFaultMemberDefault(fmd);
		FaultMember fm = new FaultMember();
		fm.setName("fmTest1");
		lc = new Location();
		lc.setBuilding("b2");
		lc.setFloor("f2");
		lc.setMnemonic("m2");
		lc.setPosition("p2");
		lc.setRoom("r2");
		fm.setLocation(lc);
		ff.addFaultMember(fm);
		exception = false;
		try{
			_alarmDAO.addFaultFamily(ff);
		}catch(Exception e){
			exception = true;
		}
		assertFalse(exception);
		al1 = _alarmDAO.getAlarm("ffTest1:fmTest1:1");
		assertNotNull(al1);
		assertEquals("ALARM_SYSTEM_SOURCES",al1.getSource().getName());
		assertEquals("http://www.test.com",al1.getHelpURL().toString());
		assertEquals("em1",al1.getPiquetEmail());
		assertEquals("gsm1",al1.getPiquetGSM());
		assertEquals("cont1",al1.getResponsiblePerson().getFamilyName());
		assertEquals(0,al1.getPriority().intValue());
		assertEquals("action1",al1.getAction());
		assertEquals("cause1",al1.getCause());
		assertEquals("conseq1",al1.getConsequence());
		assertEquals("problem1",al1.getProblemDescription());
		l = al1.getLocation();
		assertNotNull(l);
		//assertEquals("b2",l.getBuilding()); //Null
		assertEquals("f2",l.getFloor());
		assertEquals("m2",l.getMnemonic());
		assertEquals("p2",l.getPosition());
		assertEquals("r2",l.getRoom());
		
		//Check if it already exists
		exception = false;
		try{
			_alarmDAO.addFaultFamily(ff);
		}catch(Exception e){
			exception = true;
		}
		assertTrue(exception);
	}
	
	public void testUpdateFaultFamily(){
		boolean exception;
		Alarm al1;
		cern.laser.business.data.Location l;
		
		//Delete Entry if it exists due to an error in previous test
		{
			FaultFamily ff = new FaultFamily();
			ff.setName("ffTest1");
			try{
				_alarmDAO.removeFaultFamily(ff);
			}catch(Exception e){
				//This happens when FaultFamily "ffTest1" doesn't exist
			}
		}
		
		//Check Null Argument
		exception = false;
		try{
			_alarmDAO.updateFaultFamily(null);
		}catch(Exception e){
			exception = true;
		}
		assertTrue(exception);
		
		//Update a FaultFamily that doesn't exist
		FaultFamily ff = new FaultFamily();
		ff.setName("ffTest1");
		exception = false;
		try{
			_alarmDAO.updateFaultFamily(ff);
		}catch(Exception e){
			exception = true;
		}
		assertTrue(exception);
		
		//Check updating FaultFamily
		ff.setAlarmSource("ALARM_SYSTEM_SOURCES");
		ff.setHelpUrl("http://www.test.com");
		Contact ct = new Contact();
		ct.setEmail("em1");
		ct.setGsm("gsm1");
		ct.setName("cont1");
		ff.setContact(ct);
		FaultCode fc = new FaultCode();
		fc.setValue(1);
		fc.setPriority(0);
		fc.setAction("action1");
		fc.setCause("cause1");
		fc.setConsequence("conseq1");
		fc.setProblemDescription("problem1");
		ff.addFaultCode(fc);
		FaultMember fm = new FaultMember();
		fm.setName("fmTest1");
		Location lc = new Location();
		lc.setBuilding("b2");
		lc.setFloor("f2");
		lc.setMnemonic("m2");
		lc.setPosition("p2");
		lc.setRoom("r2");
		fm.setLocation(lc);
		ff.addFaultMember(fm);
		exception = false;
		try{
			_alarmDAO.addFaultFamily(ff);
		}catch(Exception e){
			exception = true;
		}
		assertFalse(exception);
		
		//Update FF information
		//ff.setAlarmSource("ALARM_SYSTEM_SOURCES2");//Can't be changed at the moment.
		ff.setHelpUrl("http://www.test2.com");
		ct.setEmail("em2");
		ct.setGsm("gsm2");
		ct.setName("cont2");
		try{
			_alarmDAO.updateFaultFamily(ff);
		}catch(Exception e){
			exception = true;
		}
		assertFalse(exception);
		al1 = _alarmDAO.getAlarm("ffTest1:fmTest1:1");
		assertNotNull(al1);
		assertEquals("ALARM_SYSTEM_SOURCES",al1.getSource().getName());
		//assertEquals("ALARM_SYSTEM_SOURCES2",al1.getSource().getName());
		assertEquals("http://www.test2.com",al1.getHelpURL().toString());
		assertEquals("em2",al1.getPiquetEmail());
		assertEquals("gsm2",al1.getPiquetGSM());
		assertEquals("cont2",al1.getResponsiblePerson().getFamilyName());
		assertEquals(0,al1.getPriority().intValue());
		assertEquals("action1",al1.getAction());
		assertEquals("cause1",al1.getCause());
		assertEquals("conseq1",al1.getConsequence());
		assertEquals("problem1",al1.getProblemDescription());
		l = al1.getLocation();
		//assertEquals("b2",l.getBuilding());
		assertEquals("f2",l.getFloor());
		assertEquals("m2",l.getMnemonic());
		assertEquals("p2",l.getPosition());
		assertEquals("r2",l.getRoom());
		
		//Update FM Information
		fm.setName("fmTest2");
		lc.setBuilding("b3");
		lc.setFloor("f3");
		lc.setMnemonic("m3");
		lc.setPosition("p3");
		lc.setRoom("r3");
		try{
			_alarmDAO.updateFaultFamily(ff);
		}catch(Exception e){
			exception = true;
		}
		assertFalse(exception);
		al1 = _alarmDAO.getAlarm("ffTest1:fmTest2:1");
		assertNotNull(al1);
		//assertEquals("ALARM_SYSTEM_SOURCES2",al1.getSource().getName());
		assertEquals("ALARM_SYSTEM_SOURCES",al1.getSource().getName());
		assertEquals("http://www.test2.com",al1.getHelpURL().toString());
		assertEquals("em2",al1.getPiquetEmail());
		assertEquals("gsm2",al1.getPiquetGSM());
		assertEquals("cont2",al1.getResponsiblePerson().getFamilyName());
		assertEquals(0,al1.getPriority().intValue());
		assertEquals("action1",al1.getAction());
		assertEquals("cause1",al1.getCause());
		assertEquals("conseq1",al1.getConsequence());
		assertEquals("problem1",al1.getProblemDescription());
		l = al1.getLocation();
		//assertEquals("b3",l.getBuilding());
		assertEquals("f3",l.getFloor());
		assertEquals("m3",l.getMnemonic());
		assertEquals("p3",l.getPosition());
		assertEquals("r3",l.getRoom());
		
		//Update FC Information
		fc.setValue(2);
		fc.setPriority(1);
		fc.setAction("action2");
		fc.setCause("cause2");
		fc.setConsequence("conseq2");
		fc.setProblemDescription("problem2");
		try{
			_alarmDAO.updateFaultFamily(ff);
		}catch(Exception e){
			exception = true;
		}
		assertFalse(exception);
		al1 = _alarmDAO.getAlarm("ffTest1:fmTest2:2");
		assertNotNull(al1);
		//assertEquals("ALARM_SYSTEM_SOURCES2",al1.getSource().getName());
		assertEquals("ALARM_SYSTEM_SOURCES",al1.getSource().getName());
		assertEquals("http://www.test2.com",al1.getHelpURL().toString());
		assertEquals("em2",al1.getPiquetEmail());
		assertEquals("gsm2",al1.getPiquetGSM());
		assertEquals("cont2",al1.getResponsiblePerson().getFamilyName());
		assertEquals(1,al1.getPriority().intValue());
		assertEquals("action2",al1.getAction());
		assertEquals("cause2",al1.getCause());
		assertEquals("conseq2",al1.getConsequence());
		assertEquals("problem2",al1.getProblemDescription());
		l = al1.getLocation();
		//assertEquals("b3",l.getBuilding());
		assertEquals("f3",l.getFloor());
		assertEquals("m3",l.getMnemonic());
		assertEquals("p3",l.getPosition());
		assertEquals("r3",l.getRoom());
		
		//Update Add FM
		FaultMember fm2 = new FaultMember();
		fm2.setName("fmTest3");
		Location lc2 = new Location();
		lc2.setBuilding("b4");
		lc2.setFloor("f4");
		lc2.setMnemonic("m4");
		lc2.setPosition("p4");
		lc2.setRoom("r4");
		fm2.setLocation(lc2);
		ff.addFaultMember(fm2);
		exception = false;
		try{
			_alarmDAO.updateFaultFamily(ff);
		}catch(Exception e){
			exception = true;
		}
		assertFalse(exception);
		al1 = _alarmDAO.getAlarm("ffTest1:fmTest3:2");
		assertNotNull(al1);
		//assertEquals("ALARM_SYSTEM_SOURCES2",al1.getSource().getName());
		assertEquals("ALARM_SYSTEM_SOURCES",al1.getSource().getName());
		assertEquals("http://www.test2.com",al1.getHelpURL().toString());
		assertEquals("em2",al1.getPiquetEmail());
		assertEquals("gsm2",al1.getPiquetGSM());
		assertEquals("cont2",al1.getResponsiblePerson().getFamilyName());
		assertEquals(1,al1.getPriority().intValue());
		assertEquals("action2",al1.getAction());
		assertEquals("cause2",al1.getCause());
		assertEquals("conseq2",al1.getConsequence());
		assertEquals("problem2",al1.getProblemDescription());
		l = al1.getLocation();
		//assertEquals("b4",l.getBuilding());
		assertEquals("f4",l.getFloor());
		assertEquals("m4",l.getMnemonic());
		assertEquals("p4",l.getPosition());
		assertEquals("r4",l.getRoom());
		
		//Update Add FC
		FaultCode fc2 = new FaultCode();
		fc2.setValue(3);
		fc2.setPriority(2);
		fc2.setAction("action3");
		fc2.setCause("cause3");
		fc2.setConsequence("conseq3");
		fc2.setProblemDescription("problem3");
		ff.addFaultCode(fc2);
		exception = false;
		try{
			_alarmDAO.updateFaultFamily(ff);
		}catch(Exception e){
			exception = true;
		}
		assertFalse(exception);
		al1 = _alarmDAO.getAlarm("ffTest1:fmTest3:3");
		assertNotNull(al1);
		//assertEquals("ALARM_SYSTEM_SOURCES2",al1.getSource().getName());
		assertEquals("ALARM_SYSTEM_SOURCES",al1.getSource().getName());
		assertEquals("http://www.test2.com",al1.getHelpURL().toString());
		assertEquals("em2",al1.getPiquetEmail());
		assertEquals("gsm2",al1.getPiquetGSM());
		assertEquals("cont2",al1.getResponsiblePerson().getFamilyName());
		assertEquals(2,al1.getPriority().intValue());
		assertEquals("action3",al1.getAction());
		assertEquals("cause3",al1.getCause());
		assertEquals("conseq3",al1.getConsequence());
		assertEquals("problem3",al1.getProblemDescription());
		l = al1.getLocation();
		//assertEquals("b4",l.getBuilding());
		assertEquals("f4",l.getFloor());
		assertEquals("m4",l.getMnemonic());
		assertEquals("p4",l.getPosition());
		assertEquals("r4",l.getRoom());
		
		//Update Remove FM
		ff.removeFaultMember(fm);
		exception = false;
		try{
			_alarmDAO.updateFaultFamily(ff);
		}catch(Exception e){
			exception = true;
		}
		assertFalse(exception);
		al1 = _alarmDAO.getAlarm("ffTest1:fmTest2:2");
		assertNull(al1);
		al1 = _alarmDAO.getAlarm("ffTest1:fmTest2:3");
		assertNull(al1);
		al1 = _alarmDAO.getAlarm("ffTest1:fmTest3:2");
		assertNotNull(al1);
		al1 = _alarmDAO.getAlarm("ffTest1:fmTest3:3");
		assertNotNull(al1);
		
		//Update Remove FC
		ff.removeFaultCode(fc);
		exception = false;
		try{
			_alarmDAO.updateFaultFamily(ff);
		}catch(Exception e){
			exception = true;
		}
		assertFalse(exception);
		al1 = _alarmDAO.getAlarm("ffTest1:fmTest3:2");
		assertNull(al1);
		al1 = _alarmDAO.getAlarm("ffTest1:fmTest3:3");
		assertNotNull(al1);
		
		//Update Add Default FM
		FaultMemberDefault fmd = new FaultMemberDefault();
		lc = new Location();
		lc.setBuilding("b1");
		lc.setFloor("f1");
		lc.setMnemonic("m1");
		lc.setPosition("p1");
		lc.setRoom("r1");
		fmd.setLocation(lc);
		ff.setFaultMemberDefault(fmd);
		exception = false;
		try{
			_alarmDAO.updateFaultFamily(ff);
		}catch(Exception e){
			exception = true;
		}
		assertFalse(exception);
		al1 = _alarmDAO.getAlarm("ffTest1:any:3");
		assertNotNull(al1);
		al1 = _alarmDAO.getAlarm("ffTest1:fmTest3:3");
		assertNotNull(al1);
		
		//Update Remove Default FM
		//ff.removeFaultMemberDefault(); Missing method?
		/*
		exception = false;
		try{
			_alarmDAO.updateFaultFamily(ff);
		}catch(Exception e){
			exception = true;
		}
		assertFalse(exception);
		al1 = _alarmDAO.getAlarm("ffTest1:any:3");
		assertNull(al1);
		al1 = _alarmDAO.getAlarm("ffTest1:fmTest3:3");
		assertNotNull(al1);
		*/
	}

	public void testRemoveFaultFamily(){
		Alarm al1;
		boolean exception;
		
		//Delete Entry if it exists due to an error in previous test
		{
			FaultFamily ff = new FaultFamily();
			ff.setName("ffTest1");
			try{
				_alarmDAO.removeFaultFamily(ff);
			}catch(Exception e){
				//This happens when FaultFamily "ffTest1" doesn't exist
			}
		}
		
		//Check null argument
		exception = false;
		try{
			_alarmDAO.removeFaultFamily(null);
		}catch(Exception e){
			exception = true;
		}
		assertTrue(exception);
		
		//Check Remove FF that doesn't exist
		FaultFamily ff = new FaultFamily();
		ff.setName("ffTest1");
		exception = false;
		try{
			_alarmDAO.removeFaultFamily(ff);
		}catch(Exception e){
			exception = true;
		}
		assertTrue(exception);
		
		//Check Remove FF
		ff.setAlarmSource("ALARM_SYSTEM_SOURCES");
		ff.setHelpUrl("http://www.test.com");
		Contact ct = new Contact();
		ct.setEmail("em1");
		ct.setGsm("gsm1");
		ct.setName("cont1");
		ff.setContact(ct);
		FaultCode fc = new FaultCode();
		fc.setValue(1);
		fc.setPriority(0);
		fc.setAction("action1");
		fc.setCause("cause1");
		fc.setConsequence("conseq1");
		fc.setProblemDescription("problem1");
		ff.addFaultCode(fc);
		FaultMemberDefault fmd = new FaultMemberDefault();
		Location lc = new Location();
		lc.setBuilding("b1");
		lc.setFloor("f1");
		lc.setMnemonic("m1");
		lc.setPosition("p1");
		lc.setRoom("r1");
		fmd.setLocation(lc);
		ff.setFaultMemberDefault(fmd);
		FaultMember fm = new FaultMember();
		fm.setName("fmTest1");
		lc = new Location();
		lc.setBuilding("b2");
		lc.setFloor("f2");
		lc.setMnemonic("m2");
		lc.setPosition("p2");
		lc.setRoom("r2");
		fm.setLocation(lc);
		ff.addFaultMember(fm);
		exception = false;
		try{
			_alarmDAO.addFaultFamily(ff);
		}catch(Exception e){
			exception = true;
		}
		assertFalse(exception);
		al1 = _alarmDAO.getAlarm("ffTest1:fmTest1:1");
		assertNotNull(al1);
		try{
			_alarmDAO.removeFaultFamily(ff);
		}catch(Exception e){
			exception = true;
		}
		assertFalse(exception);
		al1 = _alarmDAO.getAlarm("ffTest1:fmTest1:1");
		assertNull(al1);
	}
	
	public void testGetReductionDefinitions() {
		Parent p = new Parent();
		Child c = new Child();
		AlarmDefinition ad_p = new AlarmDefinition();
		AlarmDefinition ad_c = new AlarmDefinition();
		ad_p.setFaultFamily("A1");
		ad_p.setFaultMember("B1");
		ad_p.setFaultCode(1);
		ad_c.setFaultFamily("A2");
		ad_c.setFaultMember("B2");
		ad_c.setFaultCode(2);
		p.setAlarmDefinition(ad_p);
		c.setAlarmDefinition(ad_c);
		ReductionLinkType rl = new ReductionLinkType();
		rl.setParent(p);
		rl.setChild(c);
		rl.setType("NODE");
		ReductionDefinitions rds = _alarmDAO.getReductionRules();
		_alarmDAO.addReductionRule(rds, rl);
		//_alarmDAO.flushReductionRules(rds);
	}
	
	public void tearDown() throws Exception {
		_acsInfo.disconnect();
	}
}
