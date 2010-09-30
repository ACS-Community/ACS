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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Arrays;
import java.util.Set;
import java.util.Vector;

import org.apache.xerces.util.URI;
import org.apache.xerces.util.URI.MalformedURIException;
import org.exolab.castor.xml.ValidationException;

import cl.utfsm.acs.acg.dao.ACSAlarmDAOImpl;

import cern.laser.business.dao.AlarmDAO;
import cern.laser.business.data.Alarm;

import alma.acs.alarmsystem.generated.Alarms;
import alma.acs.alarmsystem.generated.Category;
import alma.acs.alarmsystem.generated.FaultCode;
import alma.acs.alarmsystem.generated.FaultFamily;
import alma.acs.alarmsystem.generated.FaultMember;
import alma.acs.alarmsystem.generated.FaultMemberDefault;

/**
 * The AlarmManager class is responsible of handling the references
 * to the Alarm definitions that the ACG does. It also should validate
 * if the set of alarms defined in the ACG are consistent with the
 * other parts of the system (e.g., the Categories and the Sources)
 * @author rtobar
 */
public class AlarmManager implements EntityManager {

	/**
	 * The singleton instance shared across the project
	 */
	private static AlarmManager _instance;

	
	private AlarmDAO _alarmDAO;
	private List<FaultFamily> _ffList;
	private HashMap<String, ObjectState> _objState;
	
	private ReductionManager _reductionManager;
	private CategoryManager _categoryManager;

	private AlarmManager(AlarmDAO alarmDAO) {
		_alarmDAO = alarmDAO;
		_ffList = new ArrayList<FaultFamily>();
		_objState = new HashMap<String, ObjectState>();
		_reductionManager = AlarmSystemManager.getInstance().getReductionManager();
		_categoryManager = AlarmSystemManager.getInstance().getCategoryManager();
	}

	/**
	 * Method to retrieve the singleton instance of the AlarmManager
	 * @return The singleton instance of the AlarmManager
	 */
	public static AlarmManager getInstance(AlarmDAO alarmDAO) {
		if( _instance == null ) {
			_instance = new AlarmManager(alarmDAO);
		}
		return _instance;
	}

	public void loadFromCDB() {
		try {
			_ffList = ((ACSAlarmDAOImpl)_alarmDAO).loadAlarms();
			_objState.clear();
			for (FaultFamily ff : _ffList)
				_objState.put(ff.getName(), new ObjectState(false));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	public String checkCDB() {
		List<FaultFamily> ffs = _ffList;
		String error = "";
		for(FaultFamily ff: ffs) {
			if(ff.getName() == null || ff.getName().length() == 0)
				error += "Fault Family "+ff.getName()+" doesn't have a name.\n";
			try {
				new URI(ff.getHelpUrl());
			} catch (MalformedURIException e1) {
				error += "Fault Family "+ff.getName()+" has malformed URL.\n";
			}
			if(ff.getContact() == null)
				error += "Fault Family "+ff.getName()+" doesn't define Contact entity.\n";
			if(ff.getContact().getName() == null || ff.getContact().getName().length() == 0)
				error += "Fault Family "+ff.getName()+" doesn't define Contact name.\n";
			if(ff.getAlarmSource().compareTo("ALARM_SYSTEM_SOURCES") != 0)
				error += "Fault Family "+ff.getName()+" has wrong Source defined.\n";
			if(ff.getFaultMemberCount() == 0 && ff.getFaultMemberDefault() == null)
				error += "Fault Family "+ff.getName()+" doesn't define any Fault Member and doesn't have a Default Fault Member.\n";
			FaultMember[] fms = ff.getFaultMember();
			for(FaultMember fm : fms) {
				if(fm.getName() == null || fm.getName().length() == 0)
					error += "Fault Member "+fm.getName()+" defined in Fault Family "+ff.getName()+" doesn't have a name.\n";
			}
			FaultCode[] fcs = ff.getFaultCode();
			for(FaultCode fc : fcs) {
				if(fc.getValue() <= 0)
					error += "Fault Code "+fc.getValue()+" defined in Fault Family "+ff.getName()+" has an invalid value (Must be > 0).\n";
				if(!fc.hasPriority())
					error += "Fault Code "+fc.getValue()+" defined in Fault Family "+ff.getName()+" doesn't define a priority.\n";
				if(fc.getPriority() < 0 || fc.getPriority() > 3)
					error += "Fault Code "+fc.getValue()+" defined in Fault Family "+ff.getName()+" has an invalid a priority (Must be in [0;3]).\n";
				if(fc.getProblemDescription() == null || fc.getProblemDescription().length() == 0)
					error += "Fault Code "+fc.getValue()+" defined in Fault Family "+ff.getName()+" doesn't define a problem description.\n";
			}
		}
		return error;
	}

	/**
	 * Returns a list of all the fault families that this manager
	 * currently handles
	 * @return The list of all the fault families that this manager
	 * currently handles
	 */
	public List<FaultFamily> getAllAlarms() {
		return _ffList;
	}
	
	public List<Alarm> getAlarms(){
		Vector<Alarm> alarms = new Vector<Alarm>();
		String[] ids = ((ACSAlarmDAOImpl)_alarmDAO).getAllAlarmIDs();
		for (int i = 0; i < ids.length; i++) {
			alarms.add(((ACSAlarmDAOImpl)_alarmDAO).getAlarm(ids[i]));
		}
		return alarms;
	}
	
	public Alarm getAlarm(String id){
		return ((ACSAlarmDAOImpl)_alarmDAO).getAlarm(id);
	}

	/**
	 * Retrieves a fault family description searching by the fault
	 * family name.
	 * @param name The name of the fault family to retrieve
	 * @return The fault family description (null if not found) 
	 */
	public FaultFamily getFaultFamily(String name) {
		for (FaultFamily family : _ffList) {
			if( family.getName().trim().compareTo(name.trim()) == 0 ) {
				return family;
			}
		}
		return null;
	}

	
	/**
	 * Searches and returns a fault code, based on the fault family
	 * name and the fault code value
	 * @param ffName The Fault Family name
	 * @param value  The Fault Code value 
	 * @return The referenced fault code
	 */
	public FaultCode getFaultCode(String ffName, int value) {
		for (FaultFamily family : _ffList) {
			if( family.getName().trim().compareTo(ffName.trim()) == 0 ) {
				FaultCode [] codes = family.getFaultCode();
				for (int i = 0; i < codes.length; i++) {
					if( codes[i].getValue() == value )
						return codes[i];
				}
			}
		}
		return null;
	}

	/**
	 * Searches and returns a fault member, based on the fault family
	 * name and the fault code name
	 * @param ffName The Fault Family name
	 * @param fmName The Fault Member name
	 * @return The associated Fault Member
	 */
	public FaultMember getFaultMember(String ffName, String fmName) {
		for (FaultFamily family : _ffList) {
			if( family.getName().trim().compareTo(ffName.trim()) == 0 ) {
				FaultMember [] members = family.getFaultMember();
				for (int i = 0; i < members.length; i++) {
					if( members[i].getName().compareTo(fmName) == 0 )
						return members[i];
				}
			}
		}
		return null;
	}
	
	public FaultMemberDefault getFaultMemberDefault(String ffName) {
		for (FaultFamily family : _ffList)
			if( family.getName().trim().compareTo(ffName.trim()) == 0 )
				return family.getFaultMemberDefault();
		return null;
	}

	/**
	 * Destroys the singleton instance of this class. This is needed to renew the internal reference to
	 * the AlarmDAO if a new connection to the DAL and the ACS Manager has been performed
	 */
	public static void destroy() {
		_instance = null;
	}
	
	/**
	 * Deletes a Fault Member of a given Fault Family. The Fault Member to be 
	 * deleted is checked against the existing Reduction Rules in order to 
	 * preserve the consistency of the application (i.e., a Fault Member cannot 
	 * be deleted if it is currently present in a Reduction Rule).
	 * @param ff The Fault Family of the Fault Member to be deleted
	 * @param fm The Fault Member to be deleted
	 * @return True if it deletes the given Fault Member, false otherwise
	 * @throws NullPointerException If the given Fault Family or Fault Member 
	 * is null
	 * @throws IllegalOperationException If the Fault Member is part of an 
	 * existing Reduction Rule
	 */
	public boolean deleteFaultMember(FaultFamily ff, FaultMember fm) throws NullPointerException, IllegalOperationException {
		if( ff == null || ff.getName() == null)
			throw new NullPointerException("The Fault Family (or its name) owner of the Fault Member to be deleted is null");
		if( fm == null || fm.getName() == null )
			throw new NullPointerException("The Fault Member to be deleted (or its name) is null");
		
		List<ReductionRule> rrL = _reductionManager.getNodeReductionRules();
		for (ReductionRule rr : rrL) {
			String[] tr = rr.getParent().getAlarmId().split(":");
			if(tr[0].compareTo(ff.getName()) == 0 && tr[1].compareTo(fm.getName()) == 0)
				throw new IllegalOperationException("The Fault Member is currently associated to a Reduction Rule");
		}

		for (Iterator<FaultFamily> iterator = _ffList.iterator(); iterator.hasNext();) {
			FaultFamily fft = (FaultFamily) iterator.next();
			if( fft.getName().compareTo(ff.getName()) == 0 ) {
				for (Iterator<FaultMember> iterator2 = Arrays.asList(fft.getFaultMember()).iterator(); iterator2.hasNext();) {
					FaultMember fmt = (FaultMember) iterator2.next();
					if(fmt.getName().compareTo(fm.getName()) == 0){
						fft.removeFaultMember(fm);
						ObjectState os = _objState.get(fft.getName());
						if(os == null)
							throw new IllegalOperationException("There is no ObjectState associated with the given Fault Family");
						os.update();
						Vector<FaultFamily> ffs = new Vector<FaultFamily>();
						ffs.add(ff);
						((ACSAlarmDAOImpl)_alarmDAO).removeAlarmsMap(ffs);
						((ACSAlarmDAOImpl)_alarmDAO).generateAlarmsMap(ffs);
						return true;
					}
				}
			}
		}
		return false;
	}
	
	public boolean setFaultMemberDefault(FaultFamily ff, FaultMemberDefault fmd) throws NullPointerException, IllegalOperationException {
		if( ff == null || ff.getName() == null)
			throw new NullPointerException("The Fault Family (or its name) owner of the Fault Member to be deleted is null");
		
		List<ReductionRule> rrL = _reductionManager.getNodeReductionRules();
		for (ReductionRule rr : rrL) {
			String[] tr = rr.getParent().getAlarmId().split(":");
			if(tr[0].compareTo(ff.getName()) == 0 && tr[1].compareTo("*") == 0)
				throw new IllegalOperationException("The Fault Member is currently associated to a Reduction Rule");
		}

		for (Iterator<FaultFamily> iterator = _ffList.iterator(); iterator.hasNext();) {
			FaultFamily fft = (FaultFamily) iterator.next();
			if( fft.getName().compareTo(ff.getName()) == 0 ) {
				FaultMemberDefault tfmd = fft.getFaultMemberDefault();
				fft.setFaultMemberDefault(fmd);
				ObjectState os = _objState.get(fft.getName());
				if(os == null)
					throw new IllegalOperationException("There is no ObjectState associated with the given Fault Family");
				os.update();
				if((tfmd == null && fmd != null) || (tfmd != null && fmd == null)) { 
					Vector<FaultFamily> ffs = new Vector<FaultFamily>();
					ffs.add(ff);
					((ACSAlarmDAOImpl)_alarmDAO).removeAlarmsMap(ffs);
					((ACSAlarmDAOImpl)_alarmDAO).generateAlarmsMap(ffs);
				}
				return true;
			}
		}
		return false;
	}

	/**
	 * Deletes a Fault Code of a given Fault Family. The Fault Code to be 
	 * deleted is checked against the existing Reduction Rules in order to 
	 * preserve the consistency of the application (i.e., a Fault Code cannot 
	 * be deleted if it is currently present in a Reduction Rule).
	 * @param ff The Fault Family of the Fault Code to be deleted
	 * @param fc The Fault Code to be deleted
	 * @return True if it deletes the given Fault Code, false otherwise
	 * @throws NullPointerException If the given Fault Family or Fault Code 
	 * is null
	 * @throws IllegalOperationException If the Fault Code is part of an 
	 * existing Reduction Rule
	 */
	public boolean deleteFaultCode(FaultFamily ff, FaultCode fc) throws NullPointerException, IllegalOperationException {
		if( ff == null || ff.getName() == null)
			throw new NullPointerException("The Fault Family (or its name) owner of the Fault Code to be deleted is null");
		if( fc == null )
			throw new NullPointerException("The Fault Code to be deleted is null");

		List<ReductionRule> rrL = _reductionManager.getNodeReductionRules();
		for (ReductionRule rr : rrL) {
			String[] tr = rr.getParent().getAlarmId().split(":");
			if(tr[0].compareTo(ff.getName()) == 0 && Integer.parseInt(tr[2]) == fc.getValue())
				throw new IllegalOperationException("The Fault Code is currently associated to a Reduction Rule");
		}

		for (Iterator<FaultFamily> iterator = _ffList.iterator(); iterator.hasNext();) {
			FaultFamily fft = (FaultFamily) iterator.next();
			if( fft.getName().compareTo(ff.getName()) == 0 ) {
				for (Iterator<FaultCode> iterator2 = Arrays.asList(fft.getFaultCode()).iterator(); iterator2.hasNext();) {
					FaultCode fct = (FaultCode) iterator2.next();
					if(fct.getValue() == fc.getValue()){
						fft.removeFaultCode(fc);
						ObjectState os = _objState.get(fft.getName());
						if(os == null)
							throw new IllegalOperationException("There is no ObjectState associated with the given Fault Family");
						os.update();
						Vector<FaultFamily> ffs = new Vector<FaultFamily>();
						ffs.add(ff);
						((ACSAlarmDAOImpl)_alarmDAO).removeAlarmsMap(ffs);
						((ACSAlarmDAOImpl)_alarmDAO).generateAlarmsMap(ffs);
						return true;
					}
				}
			}
		}
		return false;
	}
	
	/**
	 * Deletes a Fault Family. The Fault Family to be deleted is checked 
	 * against the existing Reduction Rules in order to preserve the 
	 * consistency of the application (i.e., a Fault Code cannot be 
	 * deleted if it is currently present in a Reduction Rule).
	 * @param ff The Fault Family to be deleted
	 * @return True if it deletes the given Fault Family, false otherwise
	 * @throws NullPointerException If the given Fault Family (or its name)
	 * is null
	 * @throws IllegalOperationException If the Fault Family is part of an 
	 * existing Reduction Rule
	 */
	public boolean deleteFaultFamily(FaultFamily ff) throws NullPointerException, IllegalOperationException {
		if( ff == null || ff.getName() == null)
			throw new NullPointerException("The Fault Family (or its name) to be deleted is null");

		//Check Reduction Rules
		List<ReductionRule> rrL = _reductionManager.getNodeReductionRules();
		for (ReductionRule rr : rrL) {
			String[] tr = rr.getParent().getAlarmId().split(":");
			if(tr[0].compareTo(ff.getName()) == 0)
				throw new IllegalOperationException("The Fault Family is currently associated to a Reduction Rule");
		}
		
		//Check Categories
		List<Category> catL = _categoryManager.getAllCategories();
		for (Category c : catL) {
			String[] sFFL = c.getAlarms().getFaultFamily();
			if(sFFL.length > 1)
				continue;
			for (String sFF : sFFL) {
				if(sFF.compareTo(ff.getName()) == 0)
					throw new IllegalOperationException("There is a category that only has this FaultFamily");
			}
		}

		for (Iterator<FaultFamily> iterator = _ffList.iterator(); iterator.hasNext();) {
			FaultFamily fft = (FaultFamily) iterator.next();
			if( fft.getName().compareTo(ff.getName()) == 0 ) {
				iterator.remove();
				ObjectState os = _objState.get(fft.getName());
				if(os == null)
					throw new IllegalOperationException("There is no ObjectState associated with the given Fault Family");
				os.delete();
				for (Category c : catL) {
					Alarms als = c.getAlarms();
					if(als.removeFaultFamily(ff.getName())) {
						c.setAlarms(als);
						_categoryManager.updateCategory(c, c);
					}
				}
				Vector<FaultFamily> ffs = new Vector<FaultFamily>();
				ffs.add(ff);
				((ACSAlarmDAOImpl)_alarmDAO).removeAlarmsMap(ffs);
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Adds a new Fault Family.
	 * @param ff The Fault Family to be added
	 * @throws NullPointerException If the given Fault Family (or its name)
	 * is null
	 * @throws IllegalOperationException If the Fault Family already exists
	 */
	public void addFaultFamily(FaultFamily ff) throws NullPointerException, IllegalOperationException {
		if( ff == null || ff.getName() == null)
			throw new NullPointerException("The Fault Family (or its name) to be added is null");

		for (Iterator<FaultFamily> iterator = _ffList.iterator(); iterator.hasNext();) {
			FaultFamily fft = (FaultFamily) iterator.next();
			if( fft.getName().compareTo(ff.getName()) == 0 ) {
				throw new IllegalOperationException("The Fault Family " + ff.getName() + " already exists");
			}
		}
		_ffList.add(ff);
		ObjectState os = _objState.get(ff.getName());
		if(os == null) {
			os = new ObjectState(true);
			os.create();
			_objState.put(ff.getName(), os);
		}
		else
			os.update();
		Vector<FaultFamily> ffs = new Vector<FaultFamily>();
		ffs.add(ff);
		((ACSAlarmDAOImpl)_alarmDAO).generateAlarmsMap(ffs);
	}
	
	/**
	 * Adds a new Fault Member to a given Fault Family.
	 * @param ff The Fault Family to which the Fault Member will be added
	 * @param fm The Fault Member to be added
	 * @throws NullPointerException If the given Fault Family (or its name),
	 * the given Fault Member (or its name) or both are null
	 * @throws IllegalOperationException If the Fault Member already exists
	 * or the Fault Family doesn't exist
	 */
	public void addFaultMember(FaultFamily ff, FaultMember fm) throws NullPointerException, IllegalOperationException {
		if( ff == null || ff.getName() == null)
			throw new NullPointerException("The Fault Family (or its name) to which the Fault Member will be added is null");
		if( fm == null || fm.getName() == null)
			throw new NullPointerException("The Fault Member (or its name) to be added is null");

		for (Iterator<FaultFamily> iterator = _ffList.iterator(); iterator.hasNext();) {
			FaultFamily fft = (FaultFamily) iterator.next();
			if( fft.getName().compareTo(ff.getName()) == 0 ) {
				for (Iterator<FaultMember> iterator2 = Arrays.asList(fft.getFaultMember()).iterator(); iterator2.hasNext();) {
					FaultMember fmt = (FaultMember) iterator2.next();
					if(fmt.getName().compareTo(fm.getName()) == 0){
						throw new IllegalOperationException("The Fault Member " + fm.getName() + " already exists");
					}
				}
				fft.addFaultMember(fm);
				ObjectState os = _objState.get(fft.getName());
				if(os == null)
					throw new IllegalOperationException("There is no ObjectState associated with the given Fault Family");
				os.update();
				Vector<FaultFamily> ffs = new Vector<FaultFamily>();
				ffs.add(ff);
				((ACSAlarmDAOImpl)_alarmDAO).removeAlarmsMap(ffs);
				((ACSAlarmDAOImpl)_alarmDAO).generateAlarmsMap(ffs);
				return;
			}
		}
		throw new IllegalOperationException("The Fault Family " + ff.getName() + " doesn't exists");
	}
	
	/**
	 * Adds a new Fault Code to a given Fault Family.
	 * @param ff The Fault Family to which the Fault Code will be added
	 * @param fm The Fault Code to be added
	 * @throws NullPointerException If the given Fault Family (or its name),
	 * the given Fault Code or both are null
	 * @throws IllegalOperationException If the Fault Code already exists
	 * or the Fault Family doesn't exist
	 */
	public void addFaultCode(FaultFamily ff, FaultCode fc) throws NullPointerException, IllegalOperationException {
		if( ff == null || ff.getName() == null)
			throw new NullPointerException("The Fault Family (or its name) to which the Fault Code will be added is null");
		if( fc == null)
			throw new NullPointerException("The Fault Code to be added is null");
		
		for (Iterator<FaultFamily> iterator = _ffList.iterator(); iterator.hasNext();) {
			FaultFamily fft = (FaultFamily) iterator.next();
			if( fft.getName().compareTo(ff.getName()) == 0 ) {
				for (Iterator<FaultCode> iterator2 = Arrays.asList(fft.getFaultCode()).iterator(); iterator2.hasNext();) {
					FaultCode fct = (FaultCode) iterator2.next();
					if(fct.getValue() == fc.getValue()){
						throw new IllegalOperationException("The Fault Code " + fc.getValue() + " already exists");
					}
				}
				fft.addFaultCode(fc);
				ObjectState os = _objState.get(fft.getName());
				if(os == null)
					throw new IllegalOperationException("There is no ObjectState associated with the given Fault Family");
				os.update();
				Vector<FaultFamily> ffs = new Vector<FaultFamily>();
				ffs.add(ff);
				((ACSAlarmDAOImpl)_alarmDAO).removeAlarmsMap(ffs);
				((ACSAlarmDAOImpl)_alarmDAO).generateAlarmsMap(ffs);
				return;
			}
		}
		throw new IllegalOperationException("The Fault Family " + ff.getName() + " doesn't exists");
	}
	
	/**
	 * Modifies a Fault Family.
	 * @param ff The Fault Family to be changed
	 * @param ffi The Fault Family with the new values
	 * @throws NullPointerException If any (or both) of the given Fault Families (or their names)
	 * are null
	 * @throws IllegalOperationException If the Fault Family doesn't exists
	 */
	public void updateFaultFamily(FaultFamily ff, FaultFamily ffi) throws NullPointerException, IllegalOperationException {
		if( ff == null || ff.getName() == null)
			throw new NullPointerException("The Fault Family (or its name) to be changed is null");
		if( ffi == null || ffi.getName() == null)
			throw new NullPointerException("The Fault Family (or its name) with the new values is null");

		//TODO: If the name changes, check integrity..

		for (Iterator<FaultFamily> iterator = _ffList.iterator(); iterator.hasNext();) {
			FaultFamily fft = (FaultFamily) iterator.next();
			if( fft.getName().compareTo(ffi.getName()) == 0 ) {
				if(ff.getName().compareTo(ffi.getName()) == 0)
					continue;
				throw new IllegalOperationException("The Fault Family " + ffi.getName() + " already exists");
			}
		}
		for (Iterator<FaultFamily> iterator = _ffList.iterator(); iterator.hasNext();) {
			FaultFamily fft = (FaultFamily) iterator.next();
			if( fft.getName().compareTo(ff.getName()) == 0 ) {
				ObjectState os = _objState.get(ff.getName());
				if(os == null)
					throw new IllegalOperationException("There is no ObjectState associated with the given Fault Family");
				if(ff.getName().compareTo(ffi.getName()) == 0)
					os.update();
				else {
					os.delete();
					os = _objState.get(ffi.getName());
					if(os == null){
						os = new ObjectState(true);
						os.create();
						_objState.put(ffi.getName(), os);
					}
					else
						os.update();
					List<Category> catL = _categoryManager.getAllCategories();
					for (Category c : catL) {
						Alarms als = c.getAlarms();
						if(als.removeFaultFamily(ff.getName())) {
							c.setAlarms(als);
							als.addFaultFamily(ffi.getName());
							_categoryManager.updateCategory(c, c);
						}
					}
				}
				String name = ff.getName();
				fft.setName(ffi.getName());
				fft.setAlarmSource(ffi.getAlarmSource());
				fft.setHelpUrl(ffi.getHelpUrl());
				fft.setContact(ffi.getContact());
				if(name.compareTo(ffi.getName()) != 0) {
					Vector<FaultFamily> ffs = new Vector<FaultFamily>();
					ffs.add(ff);
					((ACSAlarmDAOImpl)_alarmDAO).removeAlarmsMap(ffs);
					((ACSAlarmDAOImpl)_alarmDAO).generateAlarmsMap(ffs);
				}
				return;
			}
		}
		throw new IllegalOperationException("The Fault Family " + ff.getName() + " doesn't exists");
	}
	
	/**
	 * Modifies a Fault Member of a given Fault Family.
	 * @param ff The Fault Family to which belongs the Fault Member to be changed
	 * @param fm The Fault Member to be changed
	 * @param fmi The Fault Member with the new values
	 * @throws NullPointerException If any (or both) of the given Fault Members (or their names)
	 * or the Fault Family (or its name) or both are null
	 * @throws IllegalOperationException If the Fault Family doesn't exists
	 */

	public void updateFaultMember(FaultFamily ff, FaultMember fm, FaultMember fmi) throws NullPointerException, IllegalOperationException {
		if( ff == null || ff.getName() == null)
			throw new NullPointerException("The Fault Family (or its name) is null");
		if( fm == null || fm.getName() == null)
			throw new NullPointerException("The Fault Member (or its name) to be changed is null");
		if( fmi == null || fmi.getName() == null)
			throw new NullPointerException("The Fault Member (or its name) with the new values is null");

		//TODO: If the name changes, check integrity..

		for (Iterator<FaultFamily> iterator = _ffList.iterator(); iterator.hasNext();) {
			FaultFamily fft = (FaultFamily) iterator.next();
			if( fft.getName().compareTo(ff.getName()) == 0 ) {
				for (Iterator<FaultMember> iterator2 = Arrays.asList(fft.getFaultMember()).iterator(); iterator2.hasNext();) {
					FaultMember fmt = (FaultMember) iterator2.next();
					if(fmt.getName().compareTo(fmi.getName()) == 0){
						if(fm.getName().compareTo(fmi.getName()) == 0)
							continue;
						throw new IllegalOperationException("The Fault Member " + fmi.getName() + " already exists");
					}
				}
				for (Iterator<FaultMember> iterator2 = Arrays.asList(fft.getFaultMember()).iterator(); iterator2.hasNext();) {
					FaultMember fmt = (FaultMember) iterator2.next();
					if(fmt.getName().compareTo(fm.getName()) == 0){
						String name = fm.getName();
						fmt.setName(fmi.getName());
						fmt.setLocation(fmi.getLocation());
						ObjectState os = _objState.get(fft.getName());
						if(os == null)
							throw new IllegalOperationException("There is no ObjectState associated with the given Fault Family");
						os.update();
						if(name.compareTo(fmi.getName()) != 0) {
							Vector<FaultFamily> ffs = new Vector<FaultFamily>();
							ffs.add(ff);
							((ACSAlarmDAOImpl)_alarmDAO).removeAlarmsMap(ffs);
							((ACSAlarmDAOImpl)_alarmDAO).generateAlarmsMap(ffs);
						}
						return;
					}
				}
				throw new IllegalOperationException("The Fault Member " + fm.getName() + " doesn't exists");
			}
		}
		throw new IllegalOperationException("The Fault Family " + ff.getName() + " doesn't exists");
	}
	
	/**
	 * Modifies a Fault Code of a given Fault Family.
	 * @param ff The Fault Family to which belongs the Fault Code to be changed
	 * @param fc The Fault Code to be changed
	 * @param fci The Fault Code with the new values
	 * @throws NullPointerException If any (or both) of the given Fault Codes
	 * or the Fault Family (or its name) or both are null
	 * @throws IllegalOperationException If the Fault Family doesn't exists
	 */
	public void updateFaultCode(FaultFamily ff, FaultCode fc, FaultCode fci) throws NullPointerException, IllegalOperationException {
		if( ff == null || ff.getName() == null)
			throw new NullPointerException("The Fault Family (or its name) is null");
		if( fc == null)
			throw new NullPointerException("The Fault Code to be changed is null");
		if( fci == null)
			throw new NullPointerException("The Fault Code with the new values is null");

		//TODO: If the value changes, check integrity..

		for (Iterator<FaultFamily> iterator = _ffList.iterator(); iterator.hasNext();) {
			FaultFamily fft = (FaultFamily) iterator.next();
			if( fft.getName().compareTo(ff.getName()) == 0 ) {
				for (Iterator<FaultCode> iterator2 = Arrays.asList(fft.getFaultCode()).iterator(); iterator2.hasNext();) {
					FaultCode fct = (FaultCode) iterator2.next();
					if(fct.getValue() == fci.getValue()){
						if(fc.getValue() == fci.getValue())
							continue;
						throw new IllegalOperationException("The Fault Code " + fci.getValue() + " already exists");
					}
				}
				for (Iterator<FaultCode> iterator2 = Arrays.asList(fft.getFaultCode()).iterator(); iterator2.hasNext();) {
					FaultCode fct = (FaultCode) iterator2.next();
					if(fct.getValue() == fc.getValue()){
						int value = fc.getValue();
						fct.setValue(fci.getValue());
						fct.setPriority(fci.getPriority());
						fct.setCause(fci.getCause());
						fct.setAction(fci.getAction());
						fct.setConsequence(fci.getConsequence());
						fct.setProblemDescription(fci.getProblemDescription());
						ObjectState os = _objState.get(fft.getName());
						if(os == null)
							throw new IllegalOperationException("There is no ObjectState associated with the given Fault Family");
						os.update();
						if(value != fci.getValue()) {
							Vector<FaultFamily> ffs = new Vector<FaultFamily>();
							ffs.add(ff);
							((ACSAlarmDAOImpl)_alarmDAO).removeAlarmsMap(ffs);
							((ACSAlarmDAOImpl)_alarmDAO).generateAlarmsMap(ffs);
						}
						return;
					}
				}
				throw new IllegalOperationException("The Fault Value " + fc.getValue() + " doesn't exists");
			}
		}
		throw new IllegalOperationException("The Fault Family " + ff.getName() + " doesn't exists");
	}
	
	public void saveToCDB(){
		Set<String> keyset = _objState.keySet();
		String[] objs = new String[keyset.size()];
		keyset.toArray(objs);
		for (int i = 0; i < objs.length; i++) {
			try {
				ObjectState os = _objState.get(objs[i]);
				FaultFamily ff = getFaultFamily(objs[i]);
				if(ff != null)
					ff.validate();
				switch(os.getAction()){
				case -1: //Error, no state assigned.
					break;
				case 0:
					break;
				case 1:
					((ACSAlarmDAOImpl)_alarmDAO).addFaultFamily(ff);
					break;
				case 2:
					((ACSAlarmDAOImpl)_alarmDAO).updateFaultFamily(ff);
					break;
				case 3:
					ff = new FaultFamily();
					ff.setName(objs[i]);
					((ACSAlarmDAOImpl)_alarmDAO).removeFaultFamily(ff);
					break;
				default: //Shouldn't happen.
					break;
				}
				_objState.remove(objs[i]);
				_objState.put(objs[i], new ObjectState(false));
			} catch (ValidationException e) {
				e.printStackTrace();
			}
		}
	}
}