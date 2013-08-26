/*
 * $Id: AlarmSearchListener.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.services.selection;

import cern.laser.client.data.Alarm;

/** The asynchronous alarm search listener interface. */
public interface AlarmSearchListener {
  /**
   * Callback method. Called on alarm change arrival.
   * 
   * @param alarm the changed alarm
   */
  public void onSearchAlarm(Alarm alarm);

  /**
   * Callback method. Called on communication problems
   * 
   * @param e the LaserSelectionException carrying the exception code
   */
  public void onSearchException(LaserSearchException e);

  public boolean isSearchCancelled();

  public void searchFinished();
}