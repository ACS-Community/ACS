/*
 * $Id: DeleteAction.java,v 1.1 2005/06/07 03:26:13 kzagar Exp $
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
import cern.gp.capabilities.DeleteCapability;
import cern.gp.nodes.GPNode;

/**
 * The action for "deleting" something. An Object that wants to support
 * this action (i.e. it wants to be "deleted") has to implement the DeleteCapability
 * and has to declare in its BeanInfo that it supports the DeleteAction.
 *
 * @see cern.gp.capabilities.DeleteCapability
 * @see cern.gp.beans.BeanInfoSupport
 * @author  Vito Baggiolini
 * @version $Revision: 1.1 $ $Date: 2005/06/07 03:26:13 $
 */
public class DeleteAction extends BeanAction {
  
  /** 
   * Creates a new instance 
   * do not call this directly, use SystemActions.get() instead
   */
  public DeleteAction() {
    super(DeleteCapability.class);
  }
  
  public String getName() {
    return "&Delete";
  }
  
  /** Performs the capability for the given node.
   * @param node the node for which the capability is performed
   * @param capability the capability to perform
   *
   */
  protected void performCapability(GPNode node, Capability capability) {
    DeleteCapability deleteCapability = (DeleteCapability) capability;
    deleteCapability.delete(node);
  }
  protected boolean surviveFocusChange() { return true; }
  
}
