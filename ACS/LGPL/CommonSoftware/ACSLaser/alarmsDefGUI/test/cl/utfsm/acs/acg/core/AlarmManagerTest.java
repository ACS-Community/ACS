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

//import alma.acs.alarmsystem.generated.Category;
import alma.acs.alarmsystem.generated.Contact;
import alma.acs.alarmsystem.generated.Location;
import alma.acs.alarmsystem.generated.FaultCode;
import alma.acs.alarmsystem.generated.FaultFamily;
import alma.acs.alarmsystem.generated.FaultMember;

import junit.framework.TestCase;

public class AlarmManagerTest extends TestCase {

	AcsInformation _acsInfo;
	DAOManager _daoManager;
	AlarmManager _am;

	public void setUp() throws Exception {
		_acsInfo = new AcsInformation("Test client");
		_daoManager = new DAOManager(_acsInfo.getContainerServices());
		_daoManager.connect();
		//_am = AlarmManager.getInstance(_daoManager.getAlarmDAO());
		//_am.loadFromCDB();
	}

	public void testGetInstance() {

		AlarmManager am1 = AlarmManager.getInstance(_daoManager.getAlarmDAO());
		AlarmManager am2 = AlarmManager.getInstance(_daoManager.getAlarmDAO());
		assertNotNull(am1);
		assertNotNull(am2);
		assertEquals(am1,am2);
	}

	public void testLoadFromCDB() {
		_am = AlarmManager.getInstance(_daoManager.getAlarmDAO());
		_am.loadFromCDB();
		assertNotNull(_am.getAllAlarms());
	}

	public void testGetAllAlarms() {
		_am = AlarmManager.getInstance(_daoManager.getAlarmDAO());
		_am.loadFromCDB();
		List<FaultFamily> ffList = _am.getAllAlarms();
		assertNotNull(ffList);
		for (FaultFamily faultFamily : ffList) {
			assertNotNull(faultFamily);
		}
	}

	public void testGetFaultFamily() {
		_am = AlarmManager.getInstance(_daoManager.getAlarmDAO());
		_am.loadFromCDB();
		List<FaultFamily> ffList = _am.getAllAlarms();
		assertNotNull(ffList);
		for (FaultFamily faultFamily : ffList) {
			FaultFamily ff = _am.getFaultFamily(faultFamily.getName());
			assertNotNull(ff);
			assertEquals(ff.getName(), faultFamily.getName());
			assertEquals(ff.getFaultCodeCount(), faultFamily.getFaultCodeCount());
			assertEquals(ff.getFaultMemberCount(), faultFamily.getFaultMemberCount());
			assertEquals(ff.getHelpUrl(), faultFamily.getHelpUrl());
			assertEquals(ff.getContact().getName(), faultFamily.getContact().getName());
			assertEquals(ff.getContact().getEmail(), faultFamily.getContact().getEmail());
			assertEquals(ff.getContact().getGsm(), faultFamily.getContact().getGsm());
		}
	}

	public void testGetFaultCode() {
		_am = AlarmManager.getInstance(_daoManager.getAlarmDAO());
		_am.loadFromCDB();
		List<FaultFamily> ffList = _am.getAllAlarms();
		assertNotNull(ffList);
		for (FaultFamily faultFamily : ffList) {
			FaultCode[] codes = faultFamily.getFaultCode();
			assertNotNull(codes);
			for (int i = 0; i < codes.length; i++) {
				FaultCode code = _am.getFaultCode(faultFamily.getName(), codes[i].getValue());
				assertNotNull(code);
				assertEquals(code.getAction(), codes[i].getAction());
				assertEquals(code.getCause(),  codes[i].getCause());
				assertEquals(code.getConsequence(),  codes[i].getConsequence());
				assertEquals(code.getInstant(),  codes[i].getInstant());
				assertEquals(code.getPriority(),  codes[i].getPriority());
				assertEquals(code.getProblemDescription(),  codes[i].getProblemDescription());
				assertEquals(code.getValue(),  codes[i].getValue());
			}
		}
	}

	public void testGetFaultMember() {
		_am = AlarmManager.getInstance(_daoManager.getAlarmDAO());
		_am.loadFromCDB();
		List<FaultFamily> ffList = _am.getAllAlarms();
		assertNotNull(ffList);
		for (FaultFamily faultFamily : ffList) {
			FaultMember[] members = faultFamily.getFaultMember();
			assertNotNull(members);
			for (int i = 0; i < members.length; i++) {
				FaultMember member = _am.getFaultMember(faultFamily.getName(), members[i].getName());
				assertNotNull(member);
				assertEquals(member.getName(),  members[i].getName());
				if( member.getLocation() != null ) {
					assertEquals(member.getLocation().getBuilding(),  members[i].getLocation().getBuilding());
					assertEquals(member.getLocation().getFloor(),  members[i].getLocation().getFloor());
					assertEquals(member.getLocation().getRoom(),  members[i].getLocation().getRoom());
					assertEquals(member.getLocation().getMnemonic(),  members[i].getLocation().getMnemonic());
					assertEquals(member.getLocation().getPosition(),  members[i].getLocation().getPosition());
				}
			}
		}
	}
	
	public void testDeleteFaultFamily() throws Exception {
		_am = AlarmManager.getInstance(_daoManager.getAlarmDAO());
		_am.loadFromCDB();
		List<FaultFamily> ff1 = _am.getAllAlarms();
		assertNotNull(ff1);
		int size1 = ff1.size();		
		assertTrue(_am.deleteFaultFamily(ff1.get(0)));
		int size2 = ff1.size();
		assertEquals(size1,size2+1);

		boolean exception = false;
		try {
			_am.deleteFaultFamily(null);
		} catch(NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);

		FaultFamily ff = new FaultFamily();
		exception = false;
		try {
			_am.deleteFaultFamily(ff);
		} catch (NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);

		ff.setName("foobar");
		assertFalse(_am.deleteFaultFamily(ff));
	}
	
	public void testDeleteFaultMember() throws Exception {
		_am = AlarmManager.getInstance(_daoManager.getAlarmDAO());
		_am.loadFromCDB();
		List<FaultFamily> ff1 = _am.getAllAlarms();
		assertNotNull(ff1);
		int size1 = ff1.get(0).getFaultMember().length;		
		assertTrue(_am.deleteFaultMember(ff1.get(0),ff1.get(0).getFaultMember(0)));
		int size2 = ff1.get(0).getFaultMember().length;
		assertEquals(size1,size2+1);

		boolean exception = false;
		try {
			_am.deleteFaultMember(null,ff1.get(0).getFaultMember(0));
		} catch(NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);
		
		exception = false;
		try {
			_am.deleteFaultMember(ff1.get(0),null);
		} catch(NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);
		
		exception = false;
		try {
			_am.deleteFaultMember(null,null);
		} catch(NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);

		FaultFamily ff = new FaultFamily();
		ff.addFaultMember(new FaultMember());
		exception = false;
		try {
			_am.deleteFaultMember(ff, ff.getFaultMember(0));
		} catch (NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);

		ff.setName("foobar");
		ff.getFaultMember(0).setName("foobar");
		assertFalse(_am.deleteFaultMember(ff,ff.getFaultMember(0)));
	}

	public void testDeleteFaultCode() throws Exception {
		_am = AlarmManager.getInstance(_daoManager.getAlarmDAO());
		_am.loadFromCDB();
		List<FaultFamily> ff1 = _am.getAllAlarms();
		assertNotNull(ff1);
		int size1 = ff1.get(0).getFaultCode().length;		
		assertTrue(_am.deleteFaultCode(ff1.get(0),ff1.get(0).getFaultCode(0)));
		int size2 = ff1.get(0).getFaultCode().length;
		assertEquals(size1,size2+1);

		boolean exception = false;
		try {
			_am.deleteFaultCode(null,ff1.get(0).getFaultCode(0));
		} catch(NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);
		
		exception = false;
		try {
			_am.deleteFaultCode(ff1.get(0),null);
		} catch(NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);
		
		exception = false;
		try {
			_am.deleteFaultCode(null,null);
		} catch(NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);

		FaultFamily ff = new FaultFamily();
		ff.addFaultCode(new FaultCode());
		exception = false;
		try {
			_am.deleteFaultCode(ff, ff.getFaultCode(0));
		} catch (NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);

		ff.setName("foobar");
		ff.getFaultCode(0).setValue(10);
		assertFalse(_am.deleteFaultCode(ff,ff.getFaultCode(0)));
	}
	
	public void testAddFaultFamily() throws Exception {
		boolean exception = false;
		_am = AlarmManager.getInstance(_daoManager.getAlarmDAO());
		_am.loadFromCDB();
		List<FaultFamily> ff1 = _am.getAllAlarms();
		assertNotNull(ff1);
		int size1 = ff1.size();
		int size2;

		size1 = ff1.size();
		try {
			_am.addFaultFamily(null);
		} catch(NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.size();
		assertEquals(size1,size2);

		FaultFamily ff = new FaultFamily();
		exception = false;
		size1 = ff1.size();
		try {
			_am.addFaultFamily(ff);
		} catch (NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.size();
		assertEquals(size1,size2);

		exception = false;
		size1 = ff1.size();
		ff.setName("foobar");
		try{
			_am.addFaultFamily(ff);
		}catch(Exception e){
			exception = true;
		}
		assertFalse(exception);
		size2 = ff1.size();
		assertEquals(size1,size2-1);
		
		exception = false;
		size1 = ff1.size();
		ff = new FaultFamily();
		ff.setName("foobar");
		try{
			_am.addFaultFamily(ff);
		}catch(Exception e){
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.size();
		assertEquals(size1,size2);
		
		exception = false;
		size1 = ff1.size();
		ff = new FaultFamily();
		ff.setName("foobar2");
		try{
			_am.addFaultFamily(ff);
		}catch(Exception e){
			exception = true;
		}
		assertFalse(exception);
		size2 = ff1.size();
		assertEquals(size1,size2-1);
	}
	
	public void testAddFaultCode() throws Exception {
		boolean exception = false;
		_am = AlarmManager.getInstance(_daoManager.getAlarmDAO());
		_am.loadFromCDB();
		List<FaultFamily> ff1 = _am.getAllAlarms();
		assertNotNull(ff1);
		int size1,size2;

		size1 = ff1.get(0).getFaultCodeCount();
		try {
			_am.addFaultCode(ff1.get(0),null);
		} catch(NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.get(0).getFaultCodeCount();
		assertEquals(size1,size2);

		FaultCode fc = new FaultCode();

		exception = false;
		size1 = ff1.get(0).getFaultCodeCount();
		fc.setValue(10233);
		try{
			_am.addFaultCode(ff1.get(0),fc);
		}catch(Exception e){
			exception = true;
		}
		assertFalse(exception);
		size2 = ff1.get(0).getFaultCodeCount();
		assertEquals(size1,size2-1);
		
		exception = false;
		size1 = ff1.get(0).getFaultCodeCount();
		fc = new FaultCode();
		fc.setValue(10233);
		try{
			_am.addFaultCode(ff1.get(0),fc);
		}catch(Exception e){
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.get(0).getFaultCodeCount();
		assertEquals(size1,size2);
		
		exception = false;
		size1 = ff1.get(0).getFaultCodeCount();
		fc = new FaultCode();
		fc.setValue(10234);
		try{
			_am.addFaultCode(ff1.get(0),fc);
		}catch(Exception e){
			exception = true;
		}
		assertFalse(exception);
		size2 = ff1.get(0).getFaultCodeCount();
		assertEquals(size1,size2-1);
	}
	
	public void testAddFaultMember() throws Exception {
		boolean exception = false;
		_am = AlarmManager.getInstance(_daoManager.getAlarmDAO());
		_am.loadFromCDB();
		List<FaultFamily> ff1 = _am.getAllAlarms();
		assertNotNull(ff1);
		int size1,size2;

		size1 = ff1.get(0).getFaultMemberCount();
		try {
			_am.addFaultMember(ff1.get(0),null);
		} catch(NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.get(0).getFaultMemberCount();
		assertEquals(size1,size2);

		FaultMember fm = new FaultMember();
		exception = false;
		size1 = ff1.get(0).getFaultMemberCount();
		try {
			_am.addFaultMember(ff1.get(0),fm);
		} catch (NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.get(0).getFaultMemberCount();
		assertEquals(size1,size2);

		exception = false;
		size1 = ff1.get(0).getFaultMemberCount();
		fm.setName("foobar");
		try{
			_am.addFaultMember(ff1.get(0),fm);
		}catch(Exception e){
			exception = true;
		}
		assertFalse(exception);
		size2 = ff1.get(0).getFaultMemberCount();
		assertEquals(size1,size2-1);
		
		exception = false;
		size1 = ff1.get(0).getFaultMemberCount();
		fm = new FaultMember();
		fm.setName("foobar");
		try{
			_am.addFaultMember(ff1.get(0),fm);
		}catch(Exception e){
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.get(0).getFaultMemberCount();
		assertEquals(size1,size2);
		
		exception = false;
		size1 = ff1.get(0).getFaultMemberCount();
		fm = new FaultMember();
		fm.setName("foobar2");
		try{
			_am.addFaultMember(ff1.get(0),fm);
		}catch(Exception e){
			exception = true;
		}
		assertFalse(exception);
		size2 = ff1.get(0).getFaultMemberCount();
		assertEquals(size1,size2-1);
	}

	public void testUpdateFaultFamily() throws Exception {
		boolean exception = false;
		_am = AlarmManager.getInstance(_daoManager.getAlarmDAO());
		_am.loadFromCDB();
		List<FaultFamily> ff1 = _am.getAllAlarms();
		assertNotNull(ff1);
		FaultFamily ff,ffm;
		int size1;
		int size2;

		size1 = ff1.size();
		try {
			_am.updateFaultFamily(null,null);
		} catch(NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.size();
		assertEquals(size1,size2);

		ff = new FaultFamily();
		exception = false;
		size1 = ff1.size();
		try {
			_am.updateFaultFamily(ff,null);
		} catch (NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.size();
		assertEquals(size1,size2);

		exception = false;
		size1 = ff1.size();
		try {
			_am.updateFaultFamily(null,ff);
		} catch (NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.size();
		assertEquals(size1,size2);

		ff.setName("foobar");
		
		exception = false;
		size1 = ff1.size();
		try{
			_am.updateFaultFamily(ff,null);
		}catch(Exception e){
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.size();
		assertEquals(size1,size2);
		
		exception = false;
		size1 = ff1.size();
		try{
			_am.updateFaultFamily(null,ff);
		}catch(Exception e){
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.size();
		assertEquals(size1,size2);
		
		exception = false;
		size1 = ff1.size();
		ff = new FaultFamily();
		ffm = new FaultFamily();
		ff.setName("foobar");
		ffm.setName("foobar2");
		try{			
			_am.updateFaultFamily(ff,ffm);
		}catch(IllegalOperationException e){
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.size();
		assertEquals(size1,size2);
		
		exception = false;
		size1 = ff1.size();
		try{			
			_am.addFaultFamily(ff);
		}catch(IllegalOperationException e){
			exception = true;
		}
		assertFalse(exception);
		size2 = ff1.size();
		assertEquals(size1,size2-1);
		
		exception = false;
		size1 = ff1.size();
		try{			
			_am.updateFaultFamily(ff,ffm);
		}catch(IllegalOperationException e){
			exception = true;
		}
		assertFalse(exception);
		size2 = ff1.size();
		assertEquals(size1,size2);
		assertNotNull(_am.getFaultFamily("foobar2"));
	}
	
	public void testUpdateFaultMember() throws Exception {
		boolean exception = false;
		_am = AlarmManager.getInstance(_daoManager.getAlarmDAO());
		_am.loadFromCDB();
		List<FaultFamily> ff1 = _am.getAllAlarms();
		assertNotNull(ff1);
		FaultFamily ff;
		FaultMember fm,fmm;
		int size1;
		int size2;
		
		ff = new FaultFamily();

		size1 = ff1.get(0).getFaultMemberCount();
		try {
			_am.updateFaultMember(null,null,null);
		} catch(NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.get(0).getFaultMemberCount();
		assertEquals(size1,size2);

		exception = false;
		size1 = ff1.get(0).getFaultMemberCount();
		try {
			_am.updateFaultMember(ff,null,null);
		} catch (NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.get(0).getFaultMemberCount();
		assertEquals(size1,size2);

		ff.setName("foo");
		fm = new FaultMember();
		exception = false;
		size1 = ff1.get(0).getFaultMemberCount();
		try {
			_am.updateFaultMember(ff,fm,null);
		} catch (NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.get(0).getFaultMemberCount();
		assertEquals(size1,size2);
		
		fmm = new FaultMember();
		exception = false;
		size1 = ff1.get(0).getFaultMemberCount();
		try {
			_am.updateFaultMember(ff,null,fmm);
		} catch (NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.get(0).getFaultMemberCount();
		assertEquals(size1,size2);

		exception = false;
		size1 = ff1.get(0).getFaultMemberCount();
		try {
			_am.updateFaultMember(ff,fm,fmm);
		} catch (NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.get(0).getFaultMemberCount();
		assertEquals(size1,size2);

		exception = false;
		ff.setName(ff1.get(0).getName());
		fm.setName("foobar");
		fmm.setName("foobar2");
		size1 = ff1.get(0).getFaultMemberCount();
		try{			
			_am.updateFaultMember(ff,fm,fmm);
		}catch(IllegalOperationException e){
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.get(0).getFaultMemberCount();
		assertEquals(size1,size2);
		
		exception = false;
		size1 = ff1.get(0).getFaultMemberCount();
		try{			
			_am.addFaultMember(ff,fm);
		}catch(IllegalOperationException e){
			exception = true;
		}
		assertFalse(exception);
		size2 = ff1.get(0).getFaultMemberCount();
		assertEquals(size1,size2-1);
		
		exception = false;
		size1 = ff1.get(0).getFaultMemberCount();
		try{			
			_am.updateFaultMember(ff,fm,fmm);
		}catch(IllegalOperationException e){
			exception = true;
		}
		assertFalse(exception);
		size2 = ff1.get(0).getFaultMemberCount();
		assertEquals(size1,size2);
		assertNotNull(_am.getFaultMember(ff1.get(0).getName(), "foobar2"));
	}
	
	public void testUpdateFaultCode() throws Exception {
		boolean exception = false;
		_am = AlarmManager.getInstance(_daoManager.getAlarmDAO());
		_am.loadFromCDB();
		List<FaultFamily> ff1 = _am.getAllAlarms();
		assertNotNull(ff1);
		FaultFamily ff;
		FaultCode fc,fcm;
		int size1;
		int size2;
		
		ff = new FaultFamily();

		size1 = ff1.get(0).getFaultCodeCount();
		try {
			_am.updateFaultCode(null,null,null);
		} catch(NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.get(0).getFaultCodeCount();
		assertEquals(size1,size2);

		exception = false;
		size1 = ff1.get(0).getFaultCodeCount();
		try {
			_am.updateFaultCode(ff,null,null);
		} catch (NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.get(0).getFaultCodeCount();
		assertEquals(size1,size2);

		ff.setName("foo");
		fc = new FaultCode();
		exception = false;
		size1 = ff1.get(0).getFaultCodeCount();
		try {
			_am.updateFaultCode(ff,fc,null);
		} catch (NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.get(0).getFaultCodeCount();
		assertEquals(size1,size2);
		
		fcm = new FaultCode();
		exception = false;
		size1 = ff1.get(0).getFaultCodeCount();
		try {
			_am.updateFaultCode(ff,null,fcm);
		} catch (NullPointerException e) {
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.get(0).getFaultCodeCount();
		assertEquals(size1,size2);

		exception = false;
		ff.setName(ff1.get(0).getName());
		fc.setValue(10342);
		fcm.setValue(10343);
		size1 = ff1.get(0).getFaultCodeCount();
		try{			
			_am.updateFaultCode(ff,fc,fcm);
		}catch(IllegalOperationException e){
			exception = true;
		}
		assertTrue(exception);
		size2 = ff1.get(0).getFaultCodeCount();
		assertEquals(size1,size2);
		
		exception = false;
		size1 = ff1.get(0).getFaultCodeCount();
		try{			
			_am.addFaultCode(ff,fc);
		}catch(IllegalOperationException e){
			exception = true;
		}
		assertFalse(exception);
		size2 = ff1.get(0).getFaultCodeCount();
		assertEquals(size1,size2-1);
		
		exception = false;
		size1 = ff1.get(0).getFaultCodeCount();
		try{			
			_am.updateFaultCode(ff,fc,fcm);
		}catch(IllegalOperationException e){
			exception = true;
		}
		assertFalse(exception);
		size2 = ff1.get(0).getFaultCodeCount();
		assertEquals(size1,size2);
		assertNotNull(_am.getFaultCode(ff1.get(0).getName(), 10343));
	}
	
	public void testSaveToCDB() {
		boolean exception;
		_am = AlarmManager.getInstance(_daoManager.getAlarmDAO());
		_am.loadFromCDB();
		
		List<FaultFamily> ff1 = _am.getAllAlarms();
		assertNotNull(ff1);
		int size1 = ff1.size();
		int size2;

		exception = false;
		FaultFamily ff = new FaultFamily();
		size1 = ff1.size();
		ff.setName("foobar");
		ff.setHelpUrl("http://www.foobar.cl/");
		Contact ct = new Contact();
		ct.setEmail("test@foobar.cl");
		ct.setGsm("da");
		ct.setName("Foobar");
		ff.setContact(ct);
		FaultCode fc = new FaultCode();
		fc.setValue(1);
		fc.setAction("a1");
		fc.setCause("c1");
		fc.setConsequence("co1");
		fc.setPriority(1);
		fc.setProblemDescription("pd1");
		ff.addFaultCode(fc);
		FaultMember fm = new FaultMember();
		fm.setName("fmFoobar");
		Location lc = new Location();
		lc.setBuilding("b1");
		lc.setFloor("f1");
		lc.setMnemonic("m1");
		lc.setPosition("p1");
		lc.setRoom("r1");
		fm.setLocation(lc);
		try{
			_am.addFaultFamily(ff);
		}catch(Exception e){
			exception = true;
		}
		assertFalse(exception);
		size2 = ff1.size();
		assertEquals(size1,size2-1);
		
		_am.saveToCDB();
		_am.loadFromCDB();
		FaultFamily fft = _am.getFaultFamily("foobar");
		assertNotNull(fft);
		assertEquals(fft.getName(),ff.getName());
		assertEquals(fft.getHelpUrl(),ff.getHelpUrl());
		
		exception = false;
		try {
			_am.deleteFaultFamily(fft);
		} catch (Exception e) {
			exception = true;
		}
		assertFalse(exception);
		_am.saveToCDB();
		_am.loadFromCDB();
		fft = _am.getFaultFamily("foobar");
		assertNull(fft);
	}
	
	public void tearDown() {
		_acsInfo.disconnect();
		AlarmManager.destroy();
	}
}