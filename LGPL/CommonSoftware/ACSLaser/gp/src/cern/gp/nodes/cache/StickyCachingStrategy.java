/*
 * $Id: StickyCachingStrategy.java,v 1.1 2005/06/07 03:26:13 kzagar Exp $
 *
 * $Date: 2005/06/07 03:26:13 $
 * $Revision: 1.1 $
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.nodes.cache;

/**
 * A caching strategy that keeps the cache valid until it is invalidated.
 *
 * @author  Lionel Mestre
 * @version $Revision: 1.1 $ $Date: 2005/06/07 03:26:13 $
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