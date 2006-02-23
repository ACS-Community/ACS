/*
 * $Id: AlarmCacheException.java,v 1.1.1.1 2005/03/30 13:37:50 acaproni Exp $
 *
 * $Date: 2005/03/30 13:37:50 $ 
 * $Revision: 1.1.1.1 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.cache;

/**
 * 
 * 
 * @version $Revision: 1.1.1.1 $ $Date: 2005/03/30 13:37:50 $
 * @author Katarina Sigerud
 */
public class AlarmCacheException extends Exception {
  public AlarmCacheException(String msg) {
    super(msg);
  }
  
  public AlarmCacheException(Throwable t) {
    super(t);
  }
  
  public AlarmCacheException(String msg, Throwable t) {
    super(msg, t);
  }
}
