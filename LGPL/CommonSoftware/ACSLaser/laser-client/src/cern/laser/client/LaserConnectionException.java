/*
 * $Id: LaserConnectionException.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client;

/** Laser client connection exception. Thrown when client application can not
 * contact the LASER business tier. */
public class LaserConnectionException extends LaserException 
{
  /** Default constructor. */  
  public LaserConnectionException()
  {
    super();
  }


  /** Constructor. Build a new LaserConnectionException and set the message.
   * @param message the message
   */  
  public LaserConnectionException(String message)
  {
    super(message);
  }

  /** Constructor. Build a new LaserConnectionException and set the message and the root
   * exception.
   * @param message the message
   * @param cause the root exception
   */  
  public LaserConnectionException(String message, Throwable cause)
  {
    super(message, cause);
  }

}