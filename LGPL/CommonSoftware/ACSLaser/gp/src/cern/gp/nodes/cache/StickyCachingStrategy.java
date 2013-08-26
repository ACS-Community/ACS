/*
 * $Id: StickyCachingStrategy.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.nodes.cache;

/**
 * A caching strategy that keeps the cache valid until it is invalidated.
 *
 * @author  Lionel Mestre
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public class StickyCachingStrategy implements CachingStrategy {
   
  private boolean isValid; // is cache valid
  
  //
  // -- CONSTRUCTORS -----------------------------------------------
  //
  
  /**
   * Constructs a new caching strategy that keep the cache valid until told otherwise
   * @param timeoutMillis the timeout for the cache
   */
  public StickyCachingStrategy() {
  }
  
  

  //
  // -- PUBLIC METHODS -----------------------------------------------
  //
  
  //
  // -- implements CachingStrategy -----------------------------------------------
  //
  
  public boolean isCacheValid() {
    return isValid;
  }
  
  public void validateCache() {
    isValid = true;
  }
  
  public void invalidateCache() {
    isValid = false;
  }

}