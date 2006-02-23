/*
 * $Id: LaserConnectionException.java,v 1.1 2005/06/06 18:19:40 kzagar Exp $
 *
 * $Date: 2005/06/06 18:19:40 $ 
 * $Revision: 1.1 $ 
 * $Author: kzagar $
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