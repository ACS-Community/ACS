/*
 * $Id: AlarmCacheException.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.cache;

/**
 * 
 * 
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
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
