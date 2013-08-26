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

import cern.laser.business.data.Alarm;

/**
 * Class used to represent a Reduction Rule. A reduction rule contains:
 * <ul>
 * 	<li>A parent alarm</li>
 *  <li>One or more children alarms</li>
 * </ul>
 * The reduction rule can be also of Node type of Multiplicity type.
 * For the last one, a threshold must be defined.
 * @author rtobar
 *
 */
public class ReductionRule {

	private boolean _isNodeReduction;
	private Alarm _parent;
	private List<Alarm> _children;
	private int _threshold;

	public ReductionRule(Alarm parent) {
		_parent = parent;
		_children = new ArrayList<Alarm>();
	}

	public void addChild(Alarm alarm) {
		_children.add(alarm);
		if(_isNodeReduction)
			_parent.addNodeChild(alarm);
		else
			_parent.addMultiplicityChild(alarm);
	}
	
	public boolean removeChild(Alarm alarm){
		for (Iterator<Alarm> iterator = _children.iterator(); iterator.hasNext();) {
    		Alarm al = iterator.next();
    		if(al.getAlarmId().compareTo(alarm.getAlarmId()) == 0){
    			iterator.remove();
    			if(_isNodeReduction)
    				_parent.removeNodeChild(alarm);
    			else
    				_parent.removeMultiplicityChild(alarm);
    			return true;
    		}
    	}
		return false;
	}

	public void setIsNodeReduction(boolean v) {
		_isNodeReduction = v;
	}

	public void setThreshold(int v) {
		_threshold = v;
	}

	public Alarm getParent() {
		return _parent;
	}

	public List<Alarm> getChildren() {
		return _children;
	}
	
	public Alarm getChild(String ff, String fm, int fc){
		for (Alarm alarm : _children) {
			if(alarm.getTriplet().getFaultFamily().compareTo(ff) == 0 && alarm.getTriplet().getFaultMember().compareTo(fm) == 0 && alarm.getTriplet().getFaultCode() == fc){
				return alarm;
			}
		}
		return null;
	}
	
	public Alarm getChild(String alarmId){
		for (Alarm alarm : _children) {
			if(alarm.getAlarmId().compareTo(alarmId) == 0){
				return alarm;
			}
		}
		return null;
	}
	
	public int getChildrenCount() {
		return _children.size();
	}

	public int getThreshold() {
		return _threshold;
	}

	public boolean getIsNodeReduction() {
		return _isNodeReduction;
	}
}
