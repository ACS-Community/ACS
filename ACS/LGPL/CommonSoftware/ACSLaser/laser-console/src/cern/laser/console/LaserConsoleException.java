/*
 * $Id: LaserConsoleException.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.console;
import cern.laser.client.LaserException;

/** Laser console exception. */
public class LaserConsoleException extends LaserException 
{
  /** Default constructor. */  
  public LaserConsoleException()
  {
    super();
  }

  /** Constructor. Build a new exception and set the message.
   * @param message the message
   */  
  public LaserConsoleException(String message)
  {
    super(message);
  }

  /** Constructor. Build a new exception and set the message and the root
   * exception.
   * @param message the message
   * @param rootCause the root exception
   */  
  public LaserConsoleException(String message, Throwable rootCause)
  {
    super(message, rootCause);
  }
}