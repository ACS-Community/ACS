/*
 * $Id: AlarmReductionHandler.java,v 1.4 2011/04/13 15:45:42 acaproni Exp $
 *
 * $Date: 2011/04/13 15:45:42 $ 
 * $Revision: 1.4 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.services.reduction;

import java.util.Collection;

import org.omg.CORBA.ORB;

import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogger;

import cern.laser.client.LaserConnectionException;
import cern.laser.client.LaserException;
import cern.laser.client.data.Alarm;
import cern.laser.client.impl.services.reduction.AlarmReductionHandlerImpl;

/**
 * Provides the service to handle alarm reduction.
 * 
 * @author F.Calderini
 * @see cern.laser.client.services.browsing.CategoryBrowsingHandler
 * @see cern.laser.client.services.browsing.AlarmBrowsingHandler
 * @see cern.laser.client.services.selection.AlarmSelectionHandler
 */
public abstract class AlarmReductionHandler {
  private static final ThreadLocal alarmReductionHandler = new ThreadLocal();

  /**
   * Factory method.
   * 
   * @return an instance of the implementation class
   * @throws LaserConnectionException if the request can not be served
   */
  public static AlarmReductionHandler get(ORB orb, AcsLogger logger) throws LaserConnectionException {
    AlarmReductionHandler instance = (AlarmReductionHandler) alarmReductionHandler.get();
    if (instance == null) {
      instance = new AlarmReductionHandlerImpl(orb,logger);
      alarmReductionHandler.set(instance);
    }

    return instance;
  }

  /**
   * Get the node children of a node parent alarm.
   * 
   * @return the children collection
   * @param parent the parent alarm
   * @throws LaserException if the request can not be served
   */
  public abstract Collection getNodeChildren(Alarm parent) throws LaserException;

  /**
   * Get the node children of a node parent alarm.
   * 
   * @return the children collection
   * @param parentId the parent alarm identifier
   * @throws LaserException if the request can not be served
   */
  public abstract Collection getNodeChildren(String parentId) throws LaserException;

  /**
   * Get the node children of a node parent alarm currently active.
   * 
   * @return the children collection
   * @param parent the parent alarm
   * @throws LaserException if the request can not be served
   */
  public abstract Collection getActiveNodeChildren(Alarm parent) throws LaserException;

  /**
   * Get the node children of a node parent alarm currently active.
   * 
   * @return the children collection
   * @param parentId the parent alarm identifier
   * @throws LaserException if the request can not be served
   */
  public abstract Collection getActiveNodeChildren(String parentId) throws LaserException;

  /**
   * Get the node parents.
   * 
   * @return the node parent collection
   * @param child the child alarm
   * @throws LaserException if the request can not be served
   */
  public abstract Collection getNodeParents(Alarm child) throws LaserException;

  /**
   * Get the node parent.
   * 
   * @return the node parent collection
   * @param childId the child alarm identifier
   * @throws LaserException if the request can not be served
   */
  public abstract Collection getNodeParents(String childId) throws LaserException;

  /**
   * Get the multiplicity children of a multiplicity parent alarm.
   * 
   * @return the children collection
   * @param parent the parent alarm
   * @throws LaserException if the request can not be served
   */
  public abstract Collection getMultiplicityChildren(Alarm parent) throws LaserException;

  /**
   * Get the multiplicity children of a multiplicity parent alarm.
   * 
   * @return the children collection
   * @param parentId the parent alarm identifier
   * @throws LaserException if the request can not be served
   */
  public abstract Collection getMultiplicityChildren(String parentId) throws LaserException;

  /**
   * Get the multiplicity children of a multiplicity parent alarm currently active.
   * 
   * @return the children collection
   * @param parent the parent alarm
   * @throws LaserException if the request can not be served
   */
  public abstract Collection getActiveMultiplicityChildren(Alarm parent) throws LaserException;

  /**
   * Get the multiplicity children of a multiplicity parent alarm currently active.
   * 
   * @return the children collection
   * @param parentId the parent alarm identifier
   * @throws LaserException if the request can not be served
   */
  public abstract Collection getActiveMultiplicityChildren(String parentId) throws LaserException;

  /**
   * Get the multiplicity parents.
   * 
   * @return the multiplicity parent collection
   * @param child the child alarm
   * @throws LaserException if the request can not be served
   */
  public abstract Collection getMultiplicityParents(Alarm child) throws LaserException;

  /**
   * Get the multiplicity parents.
   * 
   * @return the multiplicity parent collection
   * @param childid the parent alarm identifier
   * @throws LaserException if the request can not be served
   */
  public abstract Collection getMultiplicityParents(String childid) throws LaserException;

  /**
   * Get the multiplicity threshold.
   * 
   * @return the multiplicity threshold
   * @param parent the parent alarm
   * @throws LaserException if the request can not be served
   */
  public abstract Integer getMultiplicityThreshold(Alarm parent) throws LaserException;
}