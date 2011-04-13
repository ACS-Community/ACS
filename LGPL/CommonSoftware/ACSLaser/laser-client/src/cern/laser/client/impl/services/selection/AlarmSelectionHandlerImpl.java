/*
 * $Id: AlarmSelectionHandlerImpl.java,v 1.6 2011/04/13 15:45:42 acaproni Exp $
 *
 * $Date: 2011/04/13 15:45:42 $ 
 * $Revision: 1.6 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.impl.services.selection;

import java.util.Collections;
import java.util.Map;

import org.apache.log4j.Logger;
import org.omg.CORBA.ORB;

import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogger;

import cern.laser.client.LaserConnectionException;
import cern.laser.client.LaserException;
import cern.laser.client.LaserTimeOutException;
import cern.laser.client.services.selection.AlarmSearchListener;
import cern.laser.client.services.selection.AlarmSelectionHandler;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.Selection;
import cern.laser.util.LogTimeStamp;

/**
 * DOCUMENT ME!
 * 
 * @author $author$
 * @version $Revision: 1.6 $
 */
public class AlarmSelectionHandlerImpl extends AlarmSelectionHandler {
  private static final Logger LOGGER = Logger.getLogger(AlarmSelectionHandlerImpl.class.getName());

  private HeartbeatHelper heartbeatHelper;
  private AlarmSelectionHelper alarmSelectionHelper;
  private AlarmSearchHelper alarmSearchHelper;
  
  /**
   * The ORB
   */
  private final ORB orb;
  
  /**
   * The logger
   */
  private final AcsLogger logger;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  /**
   * Creates a new AlarmSelectionHandlerImpl object.
   * 
   * @throws LaserException
   * 
   * @throws LaserException DOCUMENT ME!
   */
  public AlarmSelectionHandlerImpl(ORB orb, AcsLogger logger) {
	  this.orb=orb;
	  this.logger=logger;
  }

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  //
  // -- extends AlarmSelectionHandler -------------------------------
  //

  /**
   * DOCUMENT ME!
   * 
   * @throws LaserException DOCUMENT ME!
   */
  public void close() throws LaserException {
    resetSelection();
  }

  /**
   * returns a new selection
   * 
   * @return the selection instance
   */
  public Selection createSelection() {
    return new SelectionImpl();
  }

  /**
   * DOCUMENT ME!
   * 
   * @throws LaserException DOCUMENT ME!
   */
  public void resetSelection() throws LaserException {
    try {
      if (heartbeatHelper != null) {
        heartbeatHelper.stopHeartbeatCheck();
        heartbeatHelper = null;
      } else {
      }
      if (alarmSelectionHelper != null) {
        alarmSelectionHelper.resetSelection();
      }
      else {
      }
    } catch (Exception e) {
    	System.err.println("### Exception: "+e.getMessage());
    	e.printStackTrace();
      throw new LaserException("unable to reset the selection", e);
    }
  }

  /**
   * DOCUMENT ME!
   * 
   * @param selection DOCUMENT ME!
   * @param listener DOCUMENT ME!
   * 
   * @return DOCUMENT ME!
   * @throws LaserConnectionException
   * @throws LaserException DOCUMENT ME!
   * @throws LaserException
   * @throws LaserTimeOutException
   * @throws LaserConnectionException
   * @throws IllegalArgumentException DOCUMENT ME!
   */
  public Map select(Selection selection, AlarmSelectionListener selectionListener) throws LaserException,
      LaserTimeOutException {
    if (selection == null) {
    	throw new IllegalArgumentException("selection parameter is null"); 
    }
    if (selection.getCategorySelection() == null) { 
    	throw new IllegalArgumentException("no categories selected"); 
    }
    try {
      resetSelection();

      // setup the heartbeat reception and start checking
      startHeartbeatSubscription(selectionListener);

      // perform the subscriptions
      if (selection.getCategorySelection().list().length != 0) {
        return subscribe(selection, selectionListener);
      } else {
        return Collections.EMPTY_MAP;
      }
    } catch (LaserConnectionException e) {
      resetSelection();
      throw new LaserException("unable to connect to perform the selection", e);
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.client.services.selection.AlarmSelectionHandler#search(cern.laser.client.services.selection.Selection,
   *      cern.laser.client.services.selection.AlarmSearchListener)
   */
  public void search(Selection selection, int nbOfRows, AlarmSearchListener searchListener) throws LaserException,
      LaserTimeOutException {
    if (alarmSearchHelper == null) {
      alarmSearchHelper = new AlarmSearchHelper(searchListener,orb,logger);
    }
    alarmSearchHelper.search(selection, nbOfRows);
  }

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  protected void finalize() throws Throwable {
    close();
    super.finalize();
  }

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //

  private Map subscribe(Selection selection, AlarmSelectionListener selectionListener) throws LaserException {
    if (LOGGER.isDebugEnabled())
        LogTimeStamp.logMsg("subscribing to " + selection.getCategorySelection().list().length + " categories", true);

    if (alarmSelectionHelper == null) {
      alarmSelectionHelper = new AlarmSelectionHelper(selectionListener,orb,logger);
    }
    Map active_alarms = alarmSelectionHelper.subscribe(selection);
    if (LOGGER.isDebugEnabled()) LogTimeStamp.logMsg(active_alarms.size() + " active alarms returned");
    return active_alarms;
  }

  //
  // AlarmImpl Heartbeat subscription
  //
  
  /**
   * @param heartbeatListener
   * @throws LaserException
   * @throws LaserConnectionException
   */
  private void startHeartbeatSubscription(AlarmSelectionListener heartbeatListener) throws LaserException,
      LaserConnectionException {
    if (heartbeatHelper == null) {
      heartbeatHelper = new HeartbeatHelper(heartbeatListener,orb,logger);
    }
    heartbeatHelper.startHeartbeatCheck();
  }
}
