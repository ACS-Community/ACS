package cern.laser.business.cache;

import java.util.Map;

import cern.laser.business.data.Alarm;
import cern.laser.business.data.CategoryActiveList;


/**
 * AlarmImpl cache service  class. It defines the interface for the alarm cache which
 * holds alarm plain objects loaded and persisted via the alarm entity beans. Changes to the objects
 * in the cache are propagated and published to the clients.
 *
 * @author fracalde
 */
public interface AlarmCache {

  /**
   * Initializes the alarm cache. Can only be called once.
   */
  public void initializeAlarmCache(Map alarms, Map activeLists);

  /**
   * Returns a clone of the cached alarm.
   *
   * @param identifier the alarm private identifier
   * @return the cloned alarm object.
   * @throws Exception if the operation can not be performed
   */
  public  Alarm getCopy(String identifier) throws AlarmCacheException;

  /**
   * Returns a reference to the cached alarm.
   *
   * @param identifier the alarm private identifier
   * @return the reference to the alarm object
   * @throws AlarmCacheException if the operation cannot be performed
   */
  public  Alarm getReference(String identifier) throws AlarmCacheException;

//  /**
//   * Returns a reference to the alarm specified. 
//   * The object is locked from modification by other cache users.
//   * @param identifier the id for the alarm
//   * @return a reference to the alarm
//   * @throws AlarmCacheException if the operation cannot be performed
//   */
//  public  Alarm acquire(String identifier) throws AlarmCacheException;
//  
//  /**
//   * Releases the lock held on an alarm.
//   * @param identifier the id for the alarm
//   * @throws AlarmCacheException if the operation cannot be performed.
//   */
//  public  void release(String identifier) throws AlarmCacheException;
  
  /**
   * Puts alarm in the cache. 
   * This method must be called after @link acquire as the alarm need to be locked.
   * @param alarm object to put in cache
   * @throws AlarmCacheException if the operation cannot be performed. However, the lock is released.
   */
  public  void replace(Alarm alarm) throws AlarmCacheException;

  /**
   * Puts an object in the cache. If that object was already in the cache it is replaced.
   * This method combines the methods @link acquire, @link replace, and @link release
   *
   * @param alarm the new alarm object
   *
   * @throws Exception if the operation can not be performed
   */
  public  void put(Alarm alarm) throws AlarmCacheException;

  /**
   * Invalidate the cached object. 
   * Subsequent accesses to that object will cause the object to be loaded again from the ejb.
   *
   * @param identifier the alarm private identifier
   *
   * @throws Exception if the operation can not be performed
   */
  public  void invalidate(String identifier) throws AlarmCacheException;

  /**
   * Returns a reference to the active list for a category.
   *
   * @param identifier the active list private identifier
   * @return the reference to the active list object.
   * @throws Exception if the operation can not be performed
   */
  public  CategoryActiveList getActiveListReference(Integer identifier) throws AlarmCacheException;

  /**
   * Close and deallocate the resources.
   */
  public  void close();

  /**
   * Remove the active list associated with the given category id. 
   *
   * @param identifier the id of the active list to destroy (=category id)
   *
   * @throws Exception if the operation cannot be performed
   */
  public  void removeActiveList(Integer identifier) throws AlarmCacheException;
}
