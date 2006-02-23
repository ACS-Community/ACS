/*
 * $Id: LaserHeartbeatException.java,v 1.1 2005/06/06 18:19:40 kzagar Exp $
 *
 * $Date: 2005/06/06 18:19:40 $ 
 * $Revision: 1.1 $ 
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.services.selection;


/**
 * The alarm heartbeat exception. It is asynchronously triggered on heartbeat loss/recovery.
 */
public class LaserHeartbeatException extends LaserSelectionException {
  private String code = null;

  /** Connection dropped code. */
  public static final String HEARTBEAT_LOST = "HEARTBEAT_LOST";
  /** Connection reestabilished code. */
  public static final String HEARTBEAT_RECONNECTED = "HEARTBEAT_RECONNECTED";

  /**
   * Constructor.
   * 
   * @param code The exception code
   */
  public LaserHeartbeatException(String code) {
    super(code);
  }

}