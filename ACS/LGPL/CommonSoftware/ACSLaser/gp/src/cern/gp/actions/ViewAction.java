/*
 * ViewEqSettingsAction.java
 *
 * Created on September 25, 2002, 9:35 PM
 */

package cern.gp.actions;

import cern.gp.actions.support.*;
import cern.gp.capabilities.Capability;
import cern.gp.capabilities.ViewCapability;
import cern.gp.nodes.GPNode;

/**
 * The action for "viewing" something. An Object that wants to support
 * this action (i.e. it wants to be "viewed") has to implement the ViewCapability
 * and has to declare in its BeanInfo that it supports the ViewAction.
 *
 * @see cern.gp.capabilities.ViewCapability
 * @see cern.gp.beans.BeanInfoSupport
 * @author  Vito Baggiolini
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public class ViewAction extends BeanAction {
  
  /** 
   * Creates a new instance 
   * do not call this directly, use SystemActions.get() instead
   */
  public ViewAction() {
    super(ViewCapability.class);
  }
  
  public String getName() {
    return "&View";
  }
  
  /** Performs the capability for the given node.
   * @param node the node for which the capability is performed
   * @param capability the capability to perform
   *
   */
  protected void performCapability(GPNode node, Capability capability) {
    ViewCapability viewCapability = (ViewCapability) capability;
    viewCapability.view(node);
  }
  
}
