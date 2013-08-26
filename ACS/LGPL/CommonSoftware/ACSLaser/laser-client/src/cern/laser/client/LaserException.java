/*
 * $Id: LaserException.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client;

/** Laser client exception. */
public class LaserException extends Exception 
{
  private Throwable cause = null;
  
  /** Default constructor. */  
  public LaserException()
  {
    super();
  }

  /** Constructor. Build a new LaserException and set the message.
   * @param message the message
   */  
  public LaserException(String message)
  {
    super(message);
  }

  /** Constructor. Build a new LaserException and set the message and the root
   * exception.
   * @param message the message
   * @param cause the root exception
   */  
  public LaserException(String message, Throwable cause)
  {
    super(message);
    this.cause = cause;
  }

  /** Return the root exception.
   * @return the root exception, null if none
   */  
  public Throwable getRootCause() 
  {
    if (cause == null) 
    {
      return new Exception("root cause undefined");
    }
    
    return cause;
  }
  
}