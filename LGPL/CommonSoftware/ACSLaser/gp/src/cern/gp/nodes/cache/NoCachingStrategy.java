/*
 * $Id: NoCachingStrategy.java,v 1.1 2005/06/07 03:26:13 kzagar Exp $
 *
 * $Date: 2005/06/07 03:26:13 $
 * $Revision: 1.1 $
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.nodes.cache;

/**
 * A caching strategy that disables the cache.
 *
 * @version $Revision: 1.1 $  $Date: 2005/06/07 03:26:13 $
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