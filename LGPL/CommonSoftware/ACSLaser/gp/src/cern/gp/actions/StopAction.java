/*
 * $Id: StopAction.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.actions;

import cern.gp.actions.support.*;
import cern.gp.capabilities.Capability;
import cern.gp.capabilities.StopCapability;
import cern.gp.nodes.GPNode;

/**
 * Action for stopping something. Associated with the StopCapability.
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public class StopAction extends BeanAction {
  
  public StopAction() {
    super(StopCapability.class);
  }
  
  public String getName() {
    return "Stop";
  }
  
  protected String iconResource() {
    return "resources/stop.gif";
  }
  
  protected void performCapability(GPNode node, Capability capability) {
    StopCapability stopCapability = (StopCapability) capability;
    stopCapability.stop(node);
  }  
  
}
