/*
 * $Id: LaserSearchException.java,v 1.1 2005/06/06 18:19:40 kzagar Exp $
 *
 * $Date: 2005/06/06 18:19:40 $ 
 * $Revision: 1.1 $ 
 * $Author: kzagar $
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