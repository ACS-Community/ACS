/*
 * $Id: NoCachingStrategy.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.nodes.cache;

/**
 * A caching strategy that disables the cache.
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author    Lionel Mestre
 */
public class NoCachingStrategy implements CachingStrategy {
   
  //
  // -- CONSTRUCTORS -----------------------------------------------
  //
  
  /**
   * Constructs a new caching strategy that does not do caching
   */
  public NoCachingStrategy() {
  }
  
  

  //
  // -- PUBLIC METHODS -----------------------------------------------
  //
  
  //
  // -- implements CachingStrategy -----------------------------------------------
  //
  
  public boolean isCacheValid() {
    return false;
  }
  
  public void validateCache() {
  }
  
  public void invalidateCache() {
  }

}