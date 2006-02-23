/*
 * $Id: LaserConfigurationNotFoundException.java,v 1.1 2005/06/07 03:17:25 kzagar Exp $
 *
 * $Date: 2005/06/07 03:17:25 $ 
 * $Revision: 1.1 $ 
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.console;

/** Laser console exception. Thrown when a configuration definition does not exist.*/
public class LaserConfigurationNotFoundException extends LaserConsoleException 
{
  /** Default constructor. */  
  public LaserConfigurationNotFoundException()
  {
    super();
  }

  /** Constructor. Build a new exception and set the message.
   * @param message the message
   */  
  public LaserConfigurationNotFoundException(String message)
  {
    super(message);
  }

  /** Constructor. Build a new exception and set the message and the root
   * exception.
   * @param message the message
   * @param rootCause the root exception
   */  
  public LaserConfigurationNotFoundException(String message, Throwable rootCause)
  {
    super(message, rootCause);
  }
}