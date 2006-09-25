/*
 * $Id: Cacheable.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.nodes.cache;

/**
 * An object implements this interface to signal that it has a state that is cached.
 * <p>
 * The way the cache is implemented or the way the cache is invalidate is not specified here.
 * This interface is mainly a marker interface that signals the cached state and gives a hook
 * to force the invalidation of the cached state.
 * </p>
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public interface Cacheable {
  
  /**
   * Resets all cached information so that the previously cached state is not used anymore.
   * After this call, the cached information should be discarded and reinitialized with 
   * new data.
   */
  public void resetCache();

}
