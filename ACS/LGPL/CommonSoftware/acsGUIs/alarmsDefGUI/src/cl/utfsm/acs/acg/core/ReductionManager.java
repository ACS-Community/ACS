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
import java.util.Set;

import org.exolab.castor.xml.ValidationException;

import alma.alarmsystem.alarmmessage.generated.AlarmDefinition;
import alma.alarmsystem.alarmmessage.generated.Child;
import alma.alarmsystem.alarmmessage.generated.Parent;
import alma.alarmsystem.alarmmessage.generated.ReductionDefinitions;
import alma.alarmsystem.alarmmessage.generated.ReductionLinkType;
import alma.alarmsystem.alarmmessage.generated.Threshold;

import cl.utfsm.acs.acg.dao.ACSAlarmDAOImpl;

import cern.laser.business.dao.AlarmDAO;
import cern.laser.business.data.Alarm;
import cern.laser.business.data.Triplet;

/**
 * @author rtobar
 *
 */
public class ReductionManager implements EntityManager {

	/**
	 * The singleton instance shared across the project
	 */
	private static ReductionManager _instance;

	private List<ReductionRule> _nodeReductionRules;
	private List<ReductionRule> _multiReductionRules;
	private HashMap<String, ObjectState> _objState;
	private HashMap<String, ObjectState> _thrState;

	private AlarmDAO _alarmDAO;
	private ReductionManager(AlarmDAO alarmDAO) {
		_alarmDAO = alarmDAO;
		_nodeReductionRules = new ArrayList<ReductionRule>();
		_multiReductionRules = new ArrayList<ReductionRule>();
		_objState = new HashMap<String, ObjectState>();
		_thrState = new HashMap<String, ObjectState>();
	}

	public static ReductionManager getInstance(AlarmDAO alarmDAO) {
		if( _instance == null ) {
			_instance = new ReductionManager(alarmDAO);
		}
		return _instance;
	}

	public List<ReductionRule> getNodeReductionRules() {
		return _nodeReductionRules;
	}

	public List<ReductionRule> getMultiReductionRules() {
		return _multiReductionRules;
	}

	/* (non-Javadoc)
	 * @see cl.utfsm.acs.acg.core.EntityManager#loadFromCDB()
	 */
	public void loadFromCDB() {
		String[] ids = ((ACSAlarmDAOImpl)_alarmDAO).getAllAlarmIDs();
		String[] children = null;
		Alarm alarm = null;
		ReductionRule rr = null;

		_nodeReductionRules.clear();
		_multiReductionRules.clear();
		_objState.clear();
		_thrState.clear();

		/* Add the reduction rules defined in the CDB */
		for (int i = 0; i < ids.length; i++) {
			alarm = ((ACSAlarmDAOImpl)_alarmDAO).getAlarm(ids[i]);

			/* First the Node Reduction rules */
			children = alarm.getNodeChildren();
			if( children.length > 0 ) {
				rr = new ReductionRule(alarm);
				rr.setIsNodeReduction(true);
			}
			for (int j = 0; j < children.length; j++) {
				rr.addChild(((ACSAlarmDAOImpl)_alarmDAO).getAlarm(children[j]));
				_objState.put(new String(ids[i] + "," + children[j]+",n"), new ObjectState(false));
			}
			if( children.length > 0 )
				_nodeReductionRules.add(rr);

			/* And the Multiplicity Reduction rules */
			children = alarm.getMultiplicityChildren();
			if( children.length > 0 ) {
				rr = new ReductionRule(((ACSAlarmDAOImpl)_alarmDAO).getAlarm(ids[i]));
				rr.setIsNodeReduction(false);
				if(alarm.getMultiplicityThreshold() == null) {
					System.out.println("Skipping Multi Reduction Rule: No Threshold set.");
					continue;
				}
				rr.setThreshold(alarm.getMultiplicityThreshold().intValue());
			}
			for (int j = 0; j < children.length; j++) {
				rr.addChild(((ACSAlarmDAOImpl)_alarmDAO).getAlarm(children[j]));
				_objState.put(new String(ids[i] + "," + children[j]+",m"), new ObjectState(false));
				_thrState.put(ids[i], new ObjectState(false));
			}
			if( children.length > 0 )
				_multiReductionRules.add(rr);
		}
	}
	
	public String checkCDB() {
		String error = "";
		List<ReductionRule> nrrs = _nodeReductionRules;
		List<ReductionRule> mrrs = _multiReductionRules;
		for(ReductionRule rr: nrrs) {
			Alarm p = rr.getParent();
			List<Alarm> chs = rr.getChildren();
			if(_alarmDAO.getAlarm(p.getAlarmId()) == null)
				error += "Node Reduction Rule Parent Alarm "+p.getAlarmId()+" doesn't exist.\n";
			for(Alarm ch: chs)
				if(_alarmDAO.getAlarm(ch.getAlarmId()) == null)
					error += "Node Reduction Rule Chid Alarm "+ch.getAlarmId()+" with Parent "+p.getAlarmId()+" doesn't exist.\n";
		}
		for(ReductionRule rr: mrrs) {
			Alarm p = rr.getParent();
			List<Alarm> chs = rr.getChildren();
			if(_alarmDAO.getAlarm(p.getAlarmId()) == null)
				error += "Multi Reduction Rule Parent Alarm "+p.getAlarmId()+" doesn't exist.\n";
			for(Alarm ch: chs)
				if(_alarmDAO.getAlarm(ch.getAlarmId()) == null)
					error += "Multi Reduction Rule Chid Alarm "+ch.getAlarmId()+" with Parent "+p.getAlarmId()+" doesn't exist.\n";
			if(rr.getThreshold() <= 0)
				error += "Multi Reduction Rule Threshold for Parent "+p.getAlarmId()+" is invalid (Must be > 0).\n";
		}
		return error;
	}

	/**
	 * Destroys the singleton instance of this class. This is needed to renew the internal reference to
	 * the AlarmDAO if a new connection to the DAL and the ACS Manager has been performed
	 */
	public static void destroy() {
		_instance = null;
	}

	/**
	 * Returns an Alarm which is parent in a Node Reduction Rule, constrained to the given triplet values. 
	 * @param ff The Fault Family of the Alarm
	 * @param fm The Fault Member of the Alarm
	 * @param fc The Fault Code of the Alarm
	 * @return The Alarm described by the triplet, and which is a parent alarm in a Node Reduction Rule
	 */
	public ReductionRule getNRParentByTriplet(String ff, String fm, int fc) {

		for (Iterator<ReductionRule> iterator = _nodeReductionRules.iterator(); iterator.hasNext();) {
			ReductionRule nrr = (ReductionRule) iterator.next();
			Triplet triplet = nrr.getParent().getTriplet();
			
			if( triplet.getFaultFamily().compareTo(ff) == 0 &&
				triplet.getFaultMember().compareTo(fm) == 0 &&
				triplet.getFaultCode().intValue() == fc ) {
				return nrr;
			}
		}
		return null;
	}

	/**
	 * Returns an Alarm which is parent in a Multiplicity Reduction Rule, constrained to the given triplet values. 
	 * @param ff The Fault Family of the Alarm
	 * @param fm The Fault Member of the Alarm
	 * @param fc The Fault Code of the Alarm
	 * @return The Alarm described by the triplet, and which is a parent alarm in a Multiplicity Reduction Rule
	 */
	public ReductionRule getMRParentByTriplet(String ff, String fm, int fc) {

		for (Iterator<ReductionRule> iterator = _multiReductionRules.iterator(); iterator.hasNext();) {
			ReductionRule mrr = (ReductionRule) iterator.next();
			Triplet triplet = mrr.getParent().getTriplet();
			
			if( triplet.getFaultFamily().compareTo(ff) == 0 &&
				triplet.getFaultMember().compareTo(fm) == 0 &&
				triplet.getFaultCode().intValue() == fc ) {
				return mrr;
			}
		}
		return null;
	}
	
	/**
     * Deletes a Reduction Rule
     * @param rrL List of Reduction Rules to remove from.
     * @param rr Reduction Rule to remove from.
     * @param p Parent of the Rule to be deleted
     * @param c Child of the Rule to be deleted
     */
	private boolean deleteReductionRule(List<ReductionRule> rrL, ReductionRule rr, Alarm p, Alarm c){
		if(rr == null)
			throw new NullPointerException("The Reduction Rule to be deleted is null");
		if(!rr.removeChild(c))
			return false;
		ObjectState os;
		if(rr.getIsNodeReduction())
			os = _objState.get(new String(p.getAlarmId()+","+c.getAlarmId()+",n"));
		else
			os = _objState.get(new String(p.getAlarmId()+","+c.getAlarmId()+",m"));
		if(os != null)
			os.delete();
		if(rr.getChildrenCount() > 0)
			return true;
		for (Iterator<ReductionRule> iterator = rrL.iterator(); iterator.hasNext();) {
			ReductionRule trr = iterator.next();
			if(p.getAlarmId().compareTo(trr.getParent().getAlarmId()) == 0){
				iterator.remove();
				if(!rr.getIsNodeReduction()) {
					ObjectState ts = _thrState.get(p.getAlarmId());
					if(ts != null)
						ts.delete();
				}
				break;
			}
		}
		return true;
	}
	
	 /**
     * Deletes a Node Reduction Rule
     * @param p Parent of the Rule to be deleted
     * @param c Child of the Rule to be deleted
     */
    public boolean deleteNodeReductionRule(Alarm p, Alarm c) throws IllegalOperationException {
    	ReductionRule rr = getNRParentByTriplet(p.getTriplet().getFaultFamily(), p.getTriplet().getFaultMember(), p.getTriplet().getFaultCode());
    	if(rr == null)
    		throw new IllegalOperationException("The Reduction Rule to be deleted doesn't exist");
    	return deleteReductionRule(_nodeReductionRules, rr, p, c);
    }
    
    /**
     * Deletes a Node Reduction Rule
     * @param p Parent of the Rule to be deleted
     * @param c Child of the Rule to be deleted
     */
    public boolean deleteMultiReductionRule(Alarm p, Alarm c) throws IllegalOperationException {
    	ReductionRule rr = getMRParentByTriplet(p.getTriplet().getFaultFamily(), p.getTriplet().getFaultMember(), p.getTriplet().getFaultCode());
    	if(rr == null)
    		throw new IllegalOperationException("The Reduction Rule to be deleted doesn't exist");
    	return deleteReductionRule(_multiReductionRules, rr, p, c);
    }
    
    /**
     * Adds a Reduction Rule
     * @param rrL List of Reduction Rules where the rule will be added (if it is null).
     * @param rr Reduction Rule to add to (if not null).
     * @param p Parent of the Rule to be added
     * @param c Child of the Rule to be added
     */
    private void addReductionRule(List<ReductionRule> rrL, ReductionRule rr, Alarm p, Alarm c, boolean isNodeReductionRule) throws IllegalOperationException, NullPointerException{
    	if(rr == null){
    		rr = new ReductionRule(p);
    		rr.setIsNodeReduction(isNodeReductionRule);
        	rrL.add(rr);
    	}
    	List<Alarm> chL = rr.getChildren();
    	for (Alarm alarm : chL) {
			if(alarm.getAlarmId().compareTo(c.getAlarmId()) == 0){
				throw new IllegalOperationException("The reduction rule already exists");
			}
		}
    	rr.addChild(c);
    	ObjectState os;
    	if(rr.getIsNodeReduction())
			os = _objState.get(new String(p.getAlarmId()+","+c.getAlarmId()+",n"));
		else
			os = _objState.get(new String(p.getAlarmId()+","+c.getAlarmId()+",m"));
		if(os == null) {
			os = new ObjectState(true);
			os.create();
			if(rr.getIsNodeReduction())
				_objState.put(new String(p.getAlarmId()+","+c.getAlarmId()+",n"),os);
			else
				_objState.put(new String(p.getAlarmId()+","+c.getAlarmId()+",m"),os);
		}
		else
			os.update();
    }
    
    /**
     * Adds a Node Reduction Rule
     * @param p Alarm parent of this Node Reduction Rule 
     * @param a Alarm child of this Node Reduction Rule
     * @throws IllegalOperationException if is not a Node Reduction Rule 
     */
    public void addNodeReductionRule(Alarm p, Alarm c) throws IllegalOperationException, NullPointerException {
    	ReductionRule rr = getNRParentByTriplet(p.getTriplet().getFaultFamily(), p.getTriplet().getFaultMember(), p.getTriplet().getFaultCode());
    	addReductionRule(_nodeReductionRules, rr, p, c,true);
    	if(rr == null)
    		rr = getNRParentByTriplet(p.getTriplet().getFaultFamily(), p.getTriplet().getFaultMember(), p.getTriplet().getFaultCode());
		rr.setIsNodeReduction(true);
    }
    
    /**
     * Adds a Multiplicity Reduction Rule
     * @param p Alarm parent of this Multi Reduction Rule 
     * @param a Alarm child of this Multi Reduction Rule
     * @throws IllegalOperationException if is not a Multi Reduction Rule 
     */
    public void addMultiReductionRule(Alarm p, Alarm c) throws IllegalOperationException, NullPointerException {
    	ReductionRule rr = getMRParentByTriplet(p.getTriplet().getFaultFamily(), p.getTriplet().getFaultMember(), p.getTriplet().getFaultCode());
    	addReductionRule(_multiReductionRules, rr, p, c,false);
    	if(rr == null) {
    		rr = getMRParentByTriplet(p.getTriplet().getFaultFamily(), p.getTriplet().getFaultMember(), p.getTriplet().getFaultCode());
    		rr.setThreshold(0);
    		ObjectState ts = _thrState.get(p.getAlarmId());
			if(ts == null) {
				ts = new ObjectState(true);
				ts.create();
				_thrState.put(p.getAlarmId(), ts);
			} 
			else
				ts.update();
    	}
		rr.setIsNodeReduction(false);
    }
    
    public void updateMultiThreshold(Alarm p, int v) throws IllegalOperationException, NullPointerException {
    	if(p == null)
    		throw new NullPointerException("Null Alarm argument");
    	ReductionRule rr = getMRParentByTriplet(p.getTriplet().getFaultFamily(), p.getTriplet().getFaultMember(), p.getTriplet().getFaultCode());
    	if(rr == null)
    		throw new IllegalOperationException("The Reduction Rule doesn't exist");
    	rr.setThreshold(v);
    	ObjectState ts = _thrState.get(p.getAlarmId());
		if(ts == null)
			throw new IllegalOperationException("There is no threshold ObjectState for this Alarm");
		ts.update();
    }
    
	public void saveToCDB(){
		Set<String> keyset = _objState.keySet();
		String[] objs = new String[keyset.size()];
		keyset.toArray(objs);
		ReductionDefinitions rds = ((ACSAlarmDAOImpl)_alarmDAO).getReductionRules();
		boolean flush = false;
		try {
			for (int i = 0; i < objs.length; i++) {
				ObjectState os = _objState.get(objs[i]);
				String[] spl = objs[i].split(",");
				String[] p = spl[0].split(":");
				String[] c = spl[1].split(":");
				ReductionLinkType rl = new ReductionLinkType();
				AlarmDefinition ad;
				Parent gp = new Parent();
				ad = new AlarmDefinition();
				ad.setFaultFamily(p[0]);
				ad.setFaultMember(p[1]);
				ad.setFaultCode(Integer.parseInt(p[2]));
				gp.setAlarmDefinition(ad);
				Child gc = new Child();
				ad = new AlarmDefinition();
				ad.setFaultFamily(c[0]);
				ad.setFaultMember(c[1]);
				ad.setFaultCode(Integer.parseInt(c[2]));
				gc.setAlarmDefinition(ad);
				rl.setParent(gp);
				rl.setChild(gc);
				if(spl[2].compareTo("n") == 0)
					rl.setType("NODE");
				else
					rl.setType("MULTIPLICITY");
				rl.validate();
				switch(os.getAction()){
				case -1: //Error, no state assigned.
					break;
				case 0:
					break;
				case 1:
					((ACSAlarmDAOImpl)_alarmDAO).addReductionRule(rds, rl);
					flush = true;
					break;
				case 2:
					((ACSAlarmDAOImpl)_alarmDAO).updateReductionRule(rds, rl);
					flush = true;
					break;
				case 3:
					((ACSAlarmDAOImpl)_alarmDAO).deleteReductionRule(rds, rl);
					flush = true;
					break;
				default: //Shouldn't happen.
					break;
				}
			}
			
			keyset = _thrState.keySet();
			objs = new String[keyset.size()];
			keyset.toArray(objs);
			for (int i = 0; i < objs.length; i++) {
				AlarmDefinition al;
				String[] p = objs[i].split(":");
				al = new AlarmDefinition();
				al.setFaultFamily(p[0]);
				al.setFaultMember(p[1]);
				al.setFaultCode(Integer.parseInt(p[2]));
				ReductionRule rr = getMRParentByTriplet(p[0], p[1], Integer.parseInt(p[2]));
				Threshold th = new Threshold();
				th.setAlarmDefinition(al);
				if(rr == null)
					th.setValue(0);
				else
					th.setValue(rr.getThreshold());
				ObjectState ts = _thrState.get(objs[i]);
				th.validate();
				switch(ts.getAction()){
				case -1: //Error, no state assigned.
					break;
				case 0:
					break;
				case 1:
					((ACSAlarmDAOImpl)_alarmDAO).addThreshold(rds, th);
					flush = true;
					break;
				case 2:
					((ACSAlarmDAOImpl)_alarmDAO).updateThreshold(rds, th);
					flush = true;
					break;
				case 3:
					((ACSAlarmDAOImpl)_alarmDAO).deleteThreshold(rds, th);
					flush = true;
					break;
				default: //Shouldn't happen.
					break;
				}
			}
			_objState.clear();
			_thrState.clear();
			if(flush)
				((ACSAlarmDAOImpl)_alarmDAO).flushReductionRules(rds);
			for (ReductionRule rr : _nodeReductionRules)
				for(Alarm al : rr.getChildren())
					if(rr.getIsNodeReduction())
						_objState.put(new String(rr.getParent().getAlarmId()+","+al.getAlarmId()+",n"), new ObjectState(false));
			for (ReductionRule rr : _multiReductionRules) {
				for(Alarm al : rr.getChildren())
					if(!rr.getIsNodeReduction())
						_objState.put(new String(rr.getParent().getAlarmId()+","+al.getAlarmId()+",m"), new ObjectState(false));
				_thrState.put(rr.getParent().getAlarmId(), new ObjectState(false));
			}
		} catch (ValidationException e) {
			e.printStackTrace();
		}
	}
}
