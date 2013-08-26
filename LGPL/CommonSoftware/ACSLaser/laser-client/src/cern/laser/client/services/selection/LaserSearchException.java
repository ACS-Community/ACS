/*
 * $Id: LaserSearchException.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.services.selection;


/**
 * The alarm selection exception. It is asynchronously triggered on communication loss/recovery.
 */
public class LaserSearchException extends LaserSelectionException {
  /**
   * Constructor.
   * 
   * @param code The exception code
   */
  public LaserSearchException(String code) {
    super(code);
  }
}