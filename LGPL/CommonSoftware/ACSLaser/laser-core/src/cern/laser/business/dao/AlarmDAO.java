/*
 * $Id: AlarmDAO.java,v 1.2 2005/04/18 08:48:50 kzagar Exp $
 *
 * $Date: 2005/04/18 08:48:50 $ 
 * $Revision: 1.2 $ 
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.dao;

import java.util.Collection;

import cern.laser.business.data.Alarm;
import cern.laser.business.data.Building;
import cern.laser.business.data.Status;

/**
 * 
 * 
 * @version $Revision: 1.2 $ $Date: 2005/04/18 08:48:50 $
 * @author Katarina Sigerud
 */
public interface AlarmDAO {
  /**
   * Returns the alarm corresponding to the given id.
   * 
   * @param identifier the id of the alarm to find
   * @return the alarm
   */
  public Alarm findAlarm(String alarmId);

  /**
   * @param identifier
   * @return
   */
  //public Alarm findAlarmInitialized(String identifier);

  /**
   * @param alarmId
   * @return
   */
  public Alarm getAlarm(String alarmId);

  /**
   * @param priority
   */
  public String[] findAlarmIdsByPriority(Integer priority);

  /**
   * Returns all alarms.
   * 
   * @return all alarms
   */
  //public Alarm[] findAllAlarms();

  public String findLaserSurveillanceAlarmId();

  /**
   * @param category
   * @throws HibernateException
   */
  public void deleteAlarm(Alarm alarm);

  /**
   * @param parent
   */
  public void saveAlarm(Alarm alarm);

  /**
   * @param parent
   */
  public void updateAlarm(Alarm alarm);

  public void updateStatus(Status status);

  /**
   * @param select_sql
   * @return
   */
  public Collection search(String select_sql);

  /**
   * @param select_sql
   * @return
   */
  public Collection archiveSearch(String select_sql);

  /**
   * @param building
   * @return
   */
  public Building findBuilding(String building);
}