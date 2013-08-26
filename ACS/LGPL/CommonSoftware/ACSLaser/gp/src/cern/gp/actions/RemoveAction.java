/*
 * $Id: RemoveAction.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
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
import cern.gp.capabilities.RemoveCapability;
import cern.gp.nodes.GPNode;

/**
 * SearchAction
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public class RemoveAction extends BeanAction {
  /** 
   * Creates a new instance 
   * do not call this directly, use SystemActions.get() instead
   */
  
  public RemoveAction() {
    super(RemoveCapability.class);
  }
  
  public String getName() {
    return "&Remove";
  }
  
  protected String iconResource() {
    return "resources/stop.gif";
  }
  
  protected void performCapability(GPNode node, Capability capability) {
    RemoveCapability removeCapability = (RemoveCapability) capability;
    removeCapability.remove(node);
  }  
  
}
