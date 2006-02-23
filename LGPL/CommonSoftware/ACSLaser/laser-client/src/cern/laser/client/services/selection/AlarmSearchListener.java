/*
 * $Id: AlarmSearchListener.java,v 1.1 2005/06/06 18:19:40 kzagar Exp $
 *
 * $Date: 2005/06/06 18:19:40 $ 
 * $Revision: 1.1 $ 
 * $Author: kzagar $
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