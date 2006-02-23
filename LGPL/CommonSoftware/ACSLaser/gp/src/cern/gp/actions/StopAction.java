/*
 * $Id: StopAction.java,v 1.1 2005/06/07 03:26:13 kzagar Exp $
 *
 * $Date: 2005/06/07 03:26:13 $
 * $Revision: 1.1 $
 * $Author: kzagar $
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
 * @version $Revision: 1.1 $  $Date: 2005/06/07 03:26:13 $
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
