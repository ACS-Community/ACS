/*
 * $Id: LaserTimeOutException.java,v 1.1 2005/06/06 18:19:40 kzagar Exp $
 *
 * $Date: 2005/06/06 18:19:40 $ 
 * $Revision: 1.1 $ 
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client;

/** Laser client time out exception. Thrown when a client application's communication with LASER BL times out. */
public class LaserTimeOutException extends LaserException 
{
  /** Constructor. Build a new LaserTimeOutException and set the message.
   * @param message the message
   */  
  public LaserTimeOutException(String message)
  {
    super(message);
  }

  /** Constructor. Build a new LaserTimeOutException and set the message and the root
   * exception.
   * @param message the message
   * @param cause the root exception
   */  
  public LaserTimeOutException(String message, Throwable cause)
  {
    super(message, cause);
  }

}