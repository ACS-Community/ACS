/*
 * $Id: LaserHeartbeatException.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
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