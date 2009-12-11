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
import java.util.Iterator;
import java.util.List;

import com.cosylab.acs.laser.dao.ACSAlarmDAOImpl;
//import com.ximpleware.NodeRecorder;

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

	private AlarmDAO _alarmDAO;
	private ReductionManager(AlarmDAO alarmDAO) {
		
		_alarmDAO = alarmDAO;
		_nodeReductionRules = new ArrayList<ReductionRule>();
		_multiReductionRules = new ArrayList<ReductionRule>();

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

		/* Add the reduction rules defined in the CDB */
		for (int i = 0; i < ids.length; i++) {
			alarm = ((ACSAlarmDAOImpl)_alarmDAO).getAlarm(ids[i]);
			
			/* First the Node Reduction rules */
			children = alarm.getNodeChildren();
			if( children.length > 0 ) {
				rr = new ReductionRule(alarm);
				rr.setIsNodeReduction(true);
			}
			for (int j = 0; j < children.length; j++)
				rr.addChild(((ACSAlarmDAOImpl)_alarmDAO).getAlarm(children[j]));
			if( children.length > 0 )
				_nodeReductionRules.add(rr);

			/* And the Multiplicity Reduction rules */
			children = alarm.getMultiplicityChildren();
			if( children.length > 0 ) {
				rr = new ReductionRule(((ACSAlarmDAOImpl)_alarmDAO).getAlarm(ids[i]));
				rr.setIsNodeReduction(false);
				rr.setThreshold(alarm.getMultiplicityThreshold().intValue());
			}
			for (int j = 0; j < children.length; j++)
				rr.addChild(((ACSAlarmDAOImpl)_alarmDAO).getAlarm(children[j]));
			if( children.length > 0 )
				_multiReductionRules.add(rr);
		}

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
    		return false;
    	if(!rr.removeChild(c))
    		return false;
    	if(rr.getChildrenCount() > 0)
    		return true;
    	for (Iterator<ReductionRule> iterator = rrL.iterator(); iterator.hasNext();) {
    		ReductionRule trr = iterator.next();
    		if(p.getAlarmId().compareTo(trr.getParent().getAlarmId()) == 0){
    			iterator.remove();
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
    public boolean deleteNodeReductionRule(Alarm p, Alarm c){
    	ReductionRule rr = getNRParentByTriplet(p.getTriplet().getFaultFamily(), p.getTriplet().getFaultMember(), p.getTriplet().getFaultCode());
    	return deleteReductionRule(_nodeReductionRules, rr, p, c);
    }
    
    /**
     * Deletes a Node Reduction Rule
     * @param p Parent of the Rule to be deleted
     * @param c Child of the Rule to be deleted
     */
    public boolean deleteMultiReductionRule(Alarm p, Alarm c){
    	ReductionRule rr = getMRParentByTriplet(p.getTriplet().getFaultFamily(), p.getTriplet().getFaultMember(), p.getTriplet().getFaultCode());
    	return deleteReductionRule(_multiReductionRules, rr, p, c);
    }
    
    /**
     * Adds a Reduction Rule
     * @param rrL List of Reduction Rules where the rule will be added (if it is null).
     * @param rr Reduction Rule to add to (if not null).
     * @param p Parent of the Rule to be added
     * @param c Child of the Rule to be added
     */
    private void addReductionRule(List<ReductionRule> rrL, ReductionRule rr, Alarm p, Alarm c){
    	if(rr == null){
    		rr = new ReductionRule(p);
        	rrL.add(rr);
    	}
    	List<Alarm> chL = rr.getChildren();
    	for (Alarm alarm : chL) {
			if(alarm.getAlarmId().compareTo(c.getAlarmId()) == 0){
				return;
			}
		}
    	rr.addChild(c);    	
    }
    
    /**
     * Adds a Node Reduction Rule
     * @param p Alarm parent of this Node Reduction Rule 
     * @param a Alarm child of this Node Reduction Rule
     * @throws IllegalOperationException if is not a Node Reduction Rule 
     */
    public void addNodeReductionRule(Alarm p, Alarm c) {
    	ReductionRule rr = getNRParentByTriplet(p.getTriplet().getFaultFamily(), p.getTriplet().getFaultMember(), p.getTriplet().getFaultCode());
    	addReductionRule(_nodeReductionRules, rr, p, c);
		rr.setIsNodeReduction(true);
    }
    
    /**
     * Adds a Multiplicity Reduction Rule
     * @param p Alarm parent of this Multi Reduction Rule 
     * @param a Alarm child of this Multi Reduction Rule
     * @throws IllegalOperationException if is not a Multi Reduction Rule 
     */
    public void addMultiReductionRule(Alarm p, Alarm c) {
    	ReductionRule rr = getMRParentByTriplet(p.getTriplet().getFaultFamily(), p.getTriplet().getFaultMember(), p.getTriplet().getFaultCode());
    	addReductionRule(_multiReductionRules, rr, p, c);
		rr.setIsNodeReduction(false);
    }
    
    public void updateMultiThreshold(Alarm p, int v){
    	ReductionRule rr = getMRParentByTriplet(p.getTriplet().getFaultFamily(), p.getTriplet().getFaultMember(), p.getTriplet().getFaultCode());
    	rr.setThreshold(v);
    }
}