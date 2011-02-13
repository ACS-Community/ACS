/*
 * $Id: AlarmBrowsingHandler.java,v 1.3 2011/02/13 15:37:17 acaproni Exp $
 *
 * $Date: 2011/02/13 15:37:17 $ 
 * $Revision: 1.3 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.services.browsing;

import java.util.Collection;

import alma.acs.container.ContainerServicesBase;

import cern.laser.client.LaserConnectionException;
import cern.laser.client.LaserException;
import cern.laser.client.data.Alarm;
import cern.laser.client.data.Category;
import cern.laser.client.data.ResponsiblePerson;
import cern.laser.client.data.Source;
import cern.laser.client.data.Triplet;
import cern.laser.client.impl.services.browsing.AlarmBrowsingHandlerImpl;

/**
 * Provides the service to handle alarm definitions browsing.
 * 
 * @see cern.laser.client.services.browsing.CategoryBrowsingHandler
 * @see cern.laser.client.services.reduction.AlarmReductionHandler
 * @see cern.laser.client.services.selection.AlarmSelectionHandler
 */
public abstract class AlarmBrowsingHandler {
  private static final ThreadLocal alarmBrowsingHandler = new ThreadLocal();

  /**
   * Factory method.
   * 
   * @return an instance of the implementation class
   * @throws LaserException if the request can not be served
   */
  public static AlarmBrowsingHandler get(ContainerServicesBase contSvcs) throws LaserConnectionException {
    AlarmBrowsingHandler instance = (AlarmBrowsingHandler) alarmBrowsingHandler.get();
    if (instance == null) {
      instance = new AlarmBrowsingHandlerImpl(contSvcs);
      alarmBrowsingHandler.set(instance);
    }

    return instance;
  }

  /**
   * Get an alarm.
   * 
   * @param alarmId the alarm identifier
   * @throws LaserException if the request can not be served
   * @return the AlarmImpl instance, null if the alarm does not exist
   */
  public abstract Alarm getAlarmById(String alarmId) throws LaserException;

  /**
   * Get an alarm.
   * 
   * @param triplet the alarm public identifier
   * @throws LaserException if the request can not be served
   * @return the AlarmImpl instance, null if the alarm does not exist
   */
  public abstract Alarm getAlarmByTriplet(Triplet triplet) throws LaserException;

  /**
   * Get an alarm.
   * 
   * @param faultFamily the fault family
   * @param faultMember the fault member
   * @param faultCode the fault code
   * @throws LaserException if the request can not be served
   * @return the AlarmImpl instance, null if the alarm does not exist
   */
  public abstract Alarm getAlarmByTriplet(String faultFamily, String faultMember, Integer faultCode)
      throws LaserException;

  /**
   * Get the alarms attached to the category.
   * 
   * @param categoryId the category identifier
   * @throws LaserException if the request can not be served
   * @return the alarms attached to the category, an empty collection if none
   */
  public abstract Collection getAlarmsByCategory(Integer categoryId) throws LaserException;

  /**
   * Get the alarms attached to the category.
   * 
   * @param category the category
   * @throws LaserException if the request can not be served
   * @return the alarms attached to the category, an empty collection if none
   */
  public abstract Collection getAlarmsByCategory(Category category) throws LaserException;

  /**
   * Get the alarms attached to the source.
   * 
   * @param sourceId the source identifier
   * @throws LaserException if the request can not be served
   * @return the alarms attached to the source, an empty collection if none
   */
  public abstract Collection getAlarmsBySource(String sourceId) throws LaserException;

  /**
   * Get the alarms attached to the source.
   * 
   * @param source the source
   * @throws LaserException if the request can not be served
   * @return the alarms attached to the source, an empty collection if none
   */
  public abstract Collection getAlarmsBySource(Source source) throws LaserException;

  /**
   * Get the alarms attached to the responsible person.
   * 
   * @param responsibleId the responsible person identifier
   * @throws LaserException if the request can not be served
   * @return the alarms attacched to the responsible person, an empty collection if none
   */
  public abstract Collection getAlarmsByResponsiblePerson(Integer responsibleId) throws LaserException;

  /**
   * Get the alarms attached to the responsible person.
   * 
   * @param responsible the responsible person
   * @throws LaserException if the request can not be served
   * @return the alarms attacched to the responsible person, an empty collection if none
   */
  public abstract Collection getAlarmsByResponsiblePerson(ResponsiblePerson responsible) throws LaserException;

  /**
   * Get the alarms attached to the priority.
   * 
   * @param priority the priority (1..4)
   * @throws LaserException if the request can not be served
   * @return the alarms attached to the priority, an empty collection if none
   */
  public abstract Collection getAlarmsByPriority(Integer priority) throws LaserException;

  /**
   * Get the defined alarm sources.
   * 
   * @throws LaserException if the request can not be served
   * @return the defined alarm sources
   */
  public abstract Collection getSources() throws LaserException;

  /**
   * Get the defined responsible persons.
   * 
   * @throws LaserException if the request can not be served
   * @return the defined responsible persons
   * @throws LaserConnectionException
   */
  public abstract Collection getResponsiblePersons() throws LaserException;
}