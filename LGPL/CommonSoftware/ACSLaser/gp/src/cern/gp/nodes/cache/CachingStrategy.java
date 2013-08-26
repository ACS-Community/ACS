/*
 * $Id: CachingStrategy.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.nodes.cache;

/**
 * An object implementing this interface defines a strategy of caching. This object does not keep 
 * track of the cached objects but rather provide the strategy associated to the caching of those objects.
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public interface CachingStrategy {
  
  /**
   * Checks whether the cache is still valid. If true is returned the cached value
   * will be used. If false is returned the cached value will be discarded and
   * the getter will be invoked to get a new value.
   * @return true if the cache is still valid
   */
  public boolean isCacheValid();
  
  /**
   * Validates the current cached value. This method is called to signal that
   * the cached value has just been updated. The strategy should take this event
   * into account for the implementation of isCached()
   */
  public void validateCache();
  
  /**
   * Invalidates the current cached value. This method is called to signal that
   * the cached value should not be used anymore.
   */
  public void invalidateCache();

}
