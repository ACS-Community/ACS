package cern.laser.business.cache;

import cern.laser.business.data.AlarmChange;


/**
 * AlarmImpl cache listener interface. An instance of the listener is created
 * when the alarm cache is created. Changes to the alarm cache fire the listener
 * method.
 *
 * @author fracalde
 */
public interface AlarmCacheListener {
  /**
   * Close and deallocate the resources..
   */
  public void close();

  /**
   * Callback method. Called on changes to the alarm cache.
   *
   * @param change the cached alarm that has changed.
   */
  public void onAlarmChange(AlarmChange change);
}
