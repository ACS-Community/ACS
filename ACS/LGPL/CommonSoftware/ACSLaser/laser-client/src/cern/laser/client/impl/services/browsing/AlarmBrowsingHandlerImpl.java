/*
 * $Id: AlarmBrowsingHandlerImpl.java,v 1.7 2011/04/13 15:45:42 acaproni Exp $
 *
 * $Date: 2011/04/13 15:45:42 $ 
 * $Revision: 1.7 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.impl.services.browsing;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Properties;

import org.omg.CORBA.ORB;

import alma.acs.logging.AcsLogger;
import alma.alarmsystem.CERNAlarmService;

import cern.laser.business.definition.data.SourceDefinition;
import cern.laser.client.LaserConnectionException;
import cern.laser.client.LaserException;
import cern.laser.client.data.Alarm;
import cern.laser.client.data.Category;
import cern.laser.client.data.ResponsiblePerson;
import cern.laser.client.data.Source;
import cern.laser.client.data.Triplet;
import cern.laser.client.impl.common.AlarmServiceSingleton;
import cern.laser.client.impl.data.AlarmImpl;
import cern.laser.client.impl.data.ResponsiblePersonImpl;
import cern.laser.client.impl.data.SourceImpl;
import cern.laser.client.services.browsing.AlarmBrowsingHandler;

public class AlarmBrowsingHandlerImpl extends AlarmBrowsingHandler {

  // The AlarmService 
  private CERNAlarmService m_laser;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  public AlarmBrowsingHandlerImpl(ORB orb, AcsLogger logger) throws LaserConnectionException {
	  try {
		  this.m_laser = AlarmServiceSingleton.getInstance(orb,logger);
	  } catch (Throwable t) {
		  throw new LaserConnectionException("Error getting the alarm service",t);
	  }
  }

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  //
  // -- extends AlarmBrowsingHandler --------------------------------
  //

  public Alarm getAlarmById(String alarmId) throws LaserException {
    try {
      if (m_laser!=null) {
          alma.alarmsystem.Alarm a = m_laser.getAlarmById(alarmId);
          cern.laser.business.data.Alarm alarmBusiness = toBusinessAlarm(a);
          return new AlarmImpl(alarmBusiness);
      } else {
          throw new NullPointerException("AlarmSystem component is null");
      }
    } catch (Exception e) {
        throw new LaserException("unable to get alarm by id " + alarmId, e);
      }
  }

  public Alarm getAlarmByTriplet(Triplet triplet) throws LaserException {
    if (triplet == null) { throw new IllegalArgumentException("parameter can not be null"); }
    return getAlarmByTriplet(triplet.getFaultFamily(), triplet.getFaultMember(), triplet.getFaultCode());
  }

  public Alarm getAlarmByTriplet(String faultFamily, String faultMember, Integer faultCode) throws LaserException {
  	if ((faultFamily == null) || (faultMember == null) || (faultCode == null)) { throw new IllegalArgumentException(
        "parameter cannot be null"); }
    try {
      	if (m_laser!=null) {
            alma.alarmsystem.Alarm a = m_laser.getAlarmByTriplet(faultFamily, faultMember, faultCode.intValue());
            cern.laser.business.data.Alarm alarmBusiness = toBusinessAlarm(a);
            return new AlarmImpl(alarmBusiness);
        } else {
            throw new NullPointerException("AlarmSystem component is null");
        }
    } catch (Exception e) {
      throw new LaserException("unable to get alarm by triplet " + faultFamily + ":" + faultMember + ":" + faultCode, e);
    }
  }

  public Collection getAlarmsByCategory(Integer categoryId) throws LaserException {
  	if (categoryId == null) { throw new IllegalArgumentException("parameter can not be null"); }
    try {
      	if (m_laser!=null) {
            alma.alarmsystem.Alarm[] as = m_laser.getAlarmsByCategory(categoryId.intValue());
            return toBusinessAlarmCollection(as);
        } else {
            throw new NullPointerException("AlarmSystem component is null");
        }
    } catch (Exception e) {
      throw new LaserException("unable to get the alarms by category " + categoryId, e);
    }
  }

  public Collection getAlarmsByCategory(Category category) throws LaserException {
    if (category == null) { throw new IllegalArgumentException("parameter cannot be null"); }

    return getAlarmsByCategory(category.getCategoryId());
  }

  public Collection getAlarmsBySource(String sourceId) throws LaserException {
  	if (sourceId == null) { throw new IllegalArgumentException("parameter cannot be null"); }
    try {
      	if (m_laser!=null) {
            alma.alarmsystem.Alarm[] as = m_laser.getAlarmsBySource(sourceId);
            return toBusinessAlarmCollection(as);
        } else {
            throw new NullPointerException("AlarmSystem component is null");
        }
    } catch (Exception e) {
      throw new LaserException("unable to get the alarms by source " + sourceId, e);
    }
  }

  public Collection getAlarmsBySource(Source source) throws LaserException {
    if (source == null) { throw new IllegalArgumentException("parameter cannot be null"); }

    return getAlarmsBySource(source.getSourceId());
  }

  public Collection getAlarmsByResponsiblePerson(Integer responsibleId) throws LaserException {
  	if (responsibleId == null) { throw new IllegalArgumentException("parameter cannot be null"); }
    try {
      	if (m_laser!=null) {
            alma.alarmsystem.Alarm[] as = m_laser.getAlarmsByResponsiblePerson(responsibleId.intValue());
            return toBusinessAlarmCollection(as);
        } else {
            throw new NullPointerException("AlarmSystem component is null");
        }
    } catch (Exception e) {
      throw new LaserException("unable to get the alarms by responsible person " + responsibleId, e);
    }
  }

  public Collection getAlarmsByResponsiblePerson(ResponsiblePerson responsible) throws LaserException {
    if (responsible == null) { throw new IllegalArgumentException("parameter cannot be null"); }

    return getAlarmsByResponsiblePerson(responsible.getResponsibleId());
  }

  public Collection getAlarmsByPriority(Integer priority) throws LaserException {
  	if (priority == null) { throw new IllegalArgumentException("parameter cannot be null"); }
    try {
      	if (m_laser!=null) {
            alma.alarmsystem.Alarm[] as = m_laser.getAlarmsByPriority(priority.intValue());
            return toBusinessAlarmCollection(as);
        } else {
            throw new NullPointerException("AlarmSystem component is null");
        }
    } catch (Exception e) {
      throw new LaserException("unable to get the alarms by priority " + priority, e);
    }
  }

  public Collection getSources() throws LaserException {
  	try {
      	if (m_laser!=null) {
            alma.alarmsystem.Source[] ss = m_laser.getSources();
            return toBusinessSourceCollection(ss);
        } else {
            throw new NullPointerException("AlarmSystem component is null");
        }
    } catch (Exception e) {
      throw new LaserException("unable to get the alarm sources", e);
    }
  }

  public Collection getResponsiblePersons() throws LaserException {
  	try {
      	if (m_laser!=null) {
            alma.alarmsystem.ResponsiblePerson[] ps = m_laser.getResponsiblePersons();
            return toBusinessResponsiblePersonCollection(ps);
        } else {
            throw new NullPointerException("AlarmSystem component is null");
        }
    } catch (Exception e) {
      throw new LaserException("unable to get the alarm responsible persons", e);
    }
  }

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //

  /**
   * Helper method converting CORBA struct to EJB business class.
   * @param p
   * @return
   */
  private static cern.laser.business.data.ResponsiblePerson	toBusinessResponsiblePerson(
  		alma.alarmsystem.ResponsiblePerson p)
  {
  		return new cern.laser.business.data.ResponsiblePerson(
  				new Integer(p.responsibleId), p.familyName, p.firstName,
				p.eMail, p.gsmNumber, p.phoneNumber);
  }

  /**
   * Helper method converting CORBA struct to EJB business class.
   * @param t
   * @return
   */
  private static cern.laser.business.data.Triplet toBusinessTriplet(
  		alma.alarmsystem.Triplet t)
  {
  		return new cern.laser.business.data.Triplet(
  				t.faultFamily, t.faultMember, new Integer(t.faultCode));
  }

  /**
   * Helper method converting CORBA struct to SQL Timestamp.
   * @param t
   * @return
   */
  private static java.sql.Timestamp toSQLTimeStamp(
  		alma.alarmsystem.Timestamp t)
  {
  		java.sql.Timestamp st = new java.sql.Timestamp(t.miliseconds);
  		st.setNanos(t.nanos);
  		return st;
  }

  /**
   * Helper method converting CORBA struct to EJB business class.
   * @param s
   * @return
   */
  private static cern.laser.business.data.Status toBusinessStatus(
  		alma.alarmsystem.Status s)
  {
  		return new cern.laser.business.data.StatusImpl(
  					new Boolean(s.active), new Boolean(s.masked), new Boolean(s.reduced),
					null, null, s.sourceHostname, toSQLTimeStamp(s.sourceTimestamp),
					toSQLTimeStamp(s.userTimestamp), toSQLTimeStamp(s.systemTimestamp),
					new Properties()	// TODO !!! implement
  				);
  }

  /**
   * Helper method converting CORBA struct to EJB business class.
   * @param s
   * @return
   */
  private static cern.laser.business.data.Source toBusinessSource(
  		alma.alarmsystem.Source s)
  {
  		return new cern.laser.business.data.Source(
  				new SourceDefinition(s.sourceId, s.description, null, null, null),
				toBusinessResponsiblePerson(s.sourceResponsiblePerson)
  				);
  }

  /**
   * Helper method converting CORBA struct to EJB business class.
   * @param l
   * @return
   */
  private static cern.laser.business.data.Location toBusinessLocation(
  		alma.alarmsystem.Location l)
  {
  		cern.laser.business.data.Location bl =
  			new cern.laser.business.data.Location(
  					null, l.floor, l.mnemonic,
					l.position, l.room);
  		
  		bl.setBuilding(new cern.laser.business.data.Building(
  							l.buildingNb, l.site, new Integer(l.zone), l.map));
  		return bl;
  }

  /**
   * Helper method converting CORBA struct to EJB business class.
   * @param a
   * @return
   */
  private static cern.laser.business.data.Alarm toBusinessAlarm(alma.alarmsystem.Alarm a)
  {
	   return new cern.laser.business.data.AlarmImpl(
	   				a.alarmId, a.systemName, a.identifier,
					a.problemDescription, new Integer(a.priority), a.cause,
					a.action, a.consequence, a.piquetGSM, a.piquetEmail,
					a.helpURL, new Boolean(a.instant),
					toBusinessSource(a.alarmSource),
					toBusinessLocation(a.alarmLocation),
					toBusinessResponsiblePerson(a.alarmResponsiblePerson),
					new HashSet(CategoryBrowsingHandlerImpl.toBusinessCategoryCollection(a.categories)),
					toBusinessStatus(a.alarmStatus),
					toBusinessTriplet(a.alarmTriplet),
					a.nodeParent, a.multiplicityParent,
					a.nodeChild, a.multiplicityChild);
  }

  /**
   * Helper method.
   * @param as
   * @return
   */
  public static Collection toBusinessAlarmCollection(alma.alarmsystem.Alarm[] as) {
      if (as == null || as.length == 0)
      	return new ArrayList(0);

    Collection result = new ArrayList(as.length);
  	for (int i = 0; i < as.length; i++) {
  	    cern.laser.business.data.Alarm alarmBusiness = toBusinessAlarm(as[i]);
  		result.add(new AlarmImpl(alarmBusiness));
  	}
  	return result;
  }

  /**
   * Helper method.
   * @param ss
   * @return
   */
  public static Collection toBusinessSourceCollection(alma.alarmsystem.Source[] ss) {
      if (ss == null || ss.length == 0)
      	return new ArrayList(0);

    Collection result = new ArrayList(ss.length);
  	for (int i = 0; i < ss.length; i++) {
  	    cern.laser.business.data.Source sourceBusiness = toBusinessSource(ss[i]);
  		result.add(new SourceImpl(sourceBusiness));
  	}
  	return result;
  }
  
  /**
   * Helper method.
   * @param ps
   * @return
   */
  public static Collection toBusinessResponsiblePersonCollection(alma.alarmsystem.ResponsiblePerson[] ps) {
      if (ps == null || ps.length == 0)
      	return new ArrayList(0);

    Collection result = new ArrayList(ps.length);
  	for (int i = 0; i < ps.length; i++) {
  	    cern.laser.business.data.ResponsiblePerson personBusiness = toBusinessResponsiblePerson(ps[i]);
  		result.add(new ResponsiblePersonImpl(personBusiness));
  	}
  	return result;
  }

}