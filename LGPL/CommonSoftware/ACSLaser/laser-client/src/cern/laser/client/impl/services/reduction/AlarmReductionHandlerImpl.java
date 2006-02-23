/*
 * $Id: AlarmReductionHandlerImpl.java,v 1.2 2005/08/19 23:24:53 kzagar Exp $
 *
 * $Date: 2005/08/19 23:24:53 $ 
 * $Revision: 1.2 $ 
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.impl.services.reduction;

import java.util.Collection;

import alma.alarmsystem.AlarmService;


import cern.laser.client.LaserConnectionException;
import cern.laser.client.LaserException;
import cern.laser.client.data.Alarm;
import cern.laser.client.impl.common.AlarmServiceSingleton;
import cern.laser.client.impl.services.browsing.AlarmBrowsingHandlerImpl;
import cern.laser.client.services.reduction.AlarmReductionHandler;

public class AlarmReductionHandlerImpl extends AlarmReductionHandler {

  // The AlarmService component
  private AlarmService m_laser;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  public AlarmReductionHandlerImpl() throws LaserConnectionException {
    this.m_laser = AlarmServiceSingleton.getInstance();
  }

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  //
  // -- extends AlarmReductionHandler -------------------------------
  //

  public Collection getNodeChildren(Alarm parent) throws LaserException {
    if (parent == null) { throw new IllegalArgumentException("parameter cannot be null"); }

    return getNodeChildren(parent.getAlarmId());
  }

  public Collection getNodeChildren(String parentId) throws LaserException {
  	if (parentId == null) { throw new IllegalArgumentException("parameter cannot be null"); }
    try {
      	if (m_laser!=null) {
            alma.alarmsystem.Alarm[] as = m_laser.getNodeChildren(parentId);
            return AlarmBrowsingHandlerImpl.toBusinessAlarmCollection(as);
        } else {
            throw new NullPointerException("AlarmSystem component is null");
        }
    } catch (Exception e) {
      throw new LaserException("unable to get the node children for parent " + parentId, e);
    }
  }

  public Collection getActiveNodeChildren(Alarm parent) throws LaserException {
    if (parent == null) { throw new IllegalArgumentException("parameter cannot be null"); }

    return getActiveNodeChildren(parent.getAlarmId());
  }

  public Collection getActiveNodeChildren(String parentId) throws LaserException {
   if (parentId == null) { throw new IllegalArgumentException("parameter cannot be null"); }
    try {
      	if (m_laser!=null) {
            alma.alarmsystem.Alarm[] as = m_laser.getActiveNodeChildren(parentId);
            return AlarmBrowsingHandlerImpl.toBusinessAlarmCollection(as);
        } else {
            throw new NullPointerException("AlarmSystem component is null");
        }
    } catch (Exception e) {
      throw new LaserException("unable to get the active node children for parent " + parentId, e);
    }
  }

  public Collection getNodeParents(Alarm child) throws LaserException {
    if (child == null) { throw new IllegalArgumentException("parameter cannot be null"); }

    return getNodeParents(child.getAlarmId());
  }

  public Collection getNodeParents(String childId) throws LaserException {
  	if (childId == null) { throw new IllegalArgumentException("parameter cannot be null"); }
    try {
      	if (m_laser!=null) {
            alma.alarmsystem.Alarm[] as = m_laser.getNodeParents(childId);
            return AlarmBrowsingHandlerImpl.toBusinessAlarmCollection(as);
        } else {
            throw new NullPointerException("AlarmSystem component is null");
        }
    } catch (Exception e) {
      throw new LaserException("unable to get the node parents for child " + childId, e);
    }
  }

  public Collection getMultiplicityChildren(Alarm parent) throws LaserException {
    if (parent == null) { throw new IllegalArgumentException("parameter cannot be null"); }

    return getMultiplicityChildren(parent.getAlarmId());
  }

  public Collection getMultiplicityChildren(String parentId) throws LaserException {
  	if (parentId == null) { throw new IllegalArgumentException("parameter cannot be null"); }
    try {
      	if (m_laser!=null) {
            alma.alarmsystem.Alarm[] as = m_laser.getMultiplicityChildren(parentId);
            return AlarmBrowsingHandlerImpl.toBusinessAlarmCollection(as);
        } else {
            throw new NullPointerException("AlarmSystem component is null");
        }
    } catch (Exception e) {
      throw new LaserException("unable to get the multiplicity children for parent " + parentId, e);
    }
  }

  public Collection getActiveMultiplicityChildren(Alarm parent) throws LaserException {
    if (parent == null) { throw new IllegalArgumentException("parameter cannot be null"); }

    return getActiveMultiplicityChildren(parent.getAlarmId());
  }

  public Collection getActiveMultiplicityChildren(String parentId) throws LaserException {
  	if (parentId == null) { throw new IllegalArgumentException("parameter cannot be null"); }
    try {
      	if (m_laser!=null) {
            alma.alarmsystem.Alarm[] as = m_laser.getActiveMultiplicityChildren(parentId);
            return AlarmBrowsingHandlerImpl.toBusinessAlarmCollection(as);
        } else {
            throw new NullPointerException("AlarmSystem component is null");
        }
    } catch (Exception e) {
      throw new LaserException("unable to get the active multiplicity children for parent " + parentId, e);
    }
  }

  public Collection getMultiplicityParents(Alarm child) throws LaserException {
    if (child == null) { throw new IllegalArgumentException("parameter cannot be null"); }

    return getMultiplicityParents(child.getAlarmId());
  }

  public Collection getMultiplicityParents(String childId) throws LaserException {
  	if (childId == null) { throw new IllegalArgumentException("parameter cannot be null"); }
    try {
      	if (m_laser!=null) {
            alma.alarmsystem.Alarm[] as = m_laser.getMultiplicityParents(childId);
            return AlarmBrowsingHandlerImpl.toBusinessAlarmCollection(as);
        } else {
            throw new NullPointerException("AlarmSystem component is null");
        }
    } catch (Exception e) {
      throw new LaserException("unable to get the multiplicity parents for child " + childId, e);
    }
  }

  public Integer getMultiplicityThreshold(Alarm parent) throws LaserException {
    if (parent == null) { throw new IllegalArgumentException("parameter cannot be null"); }

    return getMultiplicityThreshold(parent.getAlarmId());
  }

  public Integer getMultiplicityThreshold(String parentId) throws LaserException {
  	try {
      	if (m_laser!=null) {
            return new Integer(m_laser.getMultiplicityThreshold(parentId));
        } else {
            throw new NullPointerException("AlarmSystem component is null");
        }
    } catch (Exception e) {
      throw new LaserException("unable to get the multiplicity threshold for parent "+parentId, e);
    }
  }

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //


 
}