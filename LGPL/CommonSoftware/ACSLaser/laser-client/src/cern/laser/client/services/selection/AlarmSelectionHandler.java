/*
 * $Id: AlarmSelectionHandler.java,v 1.4 2011/04/13 15:45:42 acaproni Exp $
 *
 * $Date: 2011/04/13 15:45:42 $ 
 * $Revision: 1.4 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.services.selection;

import java.util.Map;

import org.apache.log4j.Logger;
import org.omg.CORBA.ORB;

import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogger;

import cern.laser.client.LaserConnectionException;
import cern.laser.client.LaserException;
import cern.laser.client.LaserTimeOutException;
import cern.laser.client.impl.services.selection.AlarmSelectionHandlerImpl;

/**
 * Provides the services to define and perform an alarm selection.
 * 
 * @author F.Calderini
 * @see cern.laser.client.services.browsing.AlarmBrowsingHandler
 * @see cern.laser.client.services.browsing.CategoryBrowsingHandler
 * @see cern.laser.client.services.reduction.AlarmReductionHandler
 */
public abstract class AlarmSelectionHandler {
  private static final Logger LOGGER = Logger.getLogger(AlarmSelectionHandler.class.getName());
  
  private static final ThreadLocal alarmSelectionHandler = new ThreadLocal();

  /**
   * Factory method.
   * 
   * @return an instance of the implementation class
   * @throws LaserException if the request can not be served
   */
  public static AlarmSelectionHandler get(ORB orb, AcsLogger logger) throws LaserConnectionException, LaserException {
    AlarmSelectionHandler instance = (AlarmSelectionHandler) alarmSelectionHandler.get();
    if (instance == null) {
      if (LOGGER.isDebugEnabled()) LOGGER.debug("AlarmSelectionHandler instance is null, creating...");
      instance = new AlarmSelectionHandlerImpl(orb,logger);
      alarmSelectionHandler.set(instance);
    }

    if (LOGGER.isDebugEnabled()) LOGGER.debug("returning AlarmSelectionHandler instance "+instance);
    return instance;
  }

  /**
   * Creates a new selection
   * 
   * @return the selection instance
   */
  public abstract Selection createSelection();

  /**
   * Starts the asynchronous alarm selection. The selected alarms which are currently active will be asynchronously
   * received as well as any change that may occur to their status.
   * 
   * @param selection the alarm selection instance
   * @param listener the selection listener instance
   * @return the selected alarms currently active
   * @throws LaserConnectionException if the client cannot connect to the BL
   * @throws LaserTimeOutException if the selection times out
   * @throws LaserException if the system is unable to perform the selection
   */
  public abstract Map select(Selection selection, AlarmSelectionListener listener) throws LaserException,
      LaserTimeOutException;

  /**
   * Reset the selection.
   * 
   * @throws LaserException if the system is unable to reset the selection
   */
  public abstract void resetSelection() throws LaserException;

  /**
   * Close and deallocate resources.
   * 
   * @throws LaserException if the system is unable to close properly
   */
  public abstract void close() throws LaserException;

  /**
   * Starts the asynchronous alarm search. The found alarms will be asynchronously received.
   * 
   * @param selection the alarm selection instance
   * @param nbOfRows the number of rows to return
   * @param searchListener the search listener instance
   * 
   * @throws LaserConnectionException if the client cannot connect to the BL
   * @throws LaserTimeOutException if the selection times out
   * @throws LaserException if the system is unable to perform the selection
   */
  public abstract void search(Selection selection, int nbOfRows, AlarmSearchListener searchListener) throws LaserException,
      LaserTimeOutException;
}