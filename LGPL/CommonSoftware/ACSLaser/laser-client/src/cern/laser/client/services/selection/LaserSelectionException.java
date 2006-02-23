/*
 * $Id: LaserSelectionException.java,v 1.1 2005/06/06 18:19:40 kzagar Exp $
 *
 * $Date: 2005/06/06 18:19:40 $ 
 * $Revision: 1.1 $ 
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.services.selection;
import cern.laser.client.LaserException;

/** The alarm selection exception. It is asynchronously triggered on communication
 * loss/recovery.
 */
public class LaserSelectionException extends LaserException 
{
  private String code = null;
  
  /** Connection dropped code. */  
  public static final String CONNECTION_DROPPED = "CONNECTION_DROPPED";
  /** Connection reestabilished code. */  
  public static final String CONNECTION_REESTABILISHED = "CONNECTION_REESTABILISHED";
  
  /** Constructor.
   * @param code The exception code
   */  
  public LaserSelectionException(String code)
  {
    this.code = code;
  }

  /** Accessor method.
   * @return the exception code
   */  
  public String getCode() 
  {
    return code;
  }
}