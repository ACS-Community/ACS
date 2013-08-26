/*
 * $Id: EditAction.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
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
import cern.gp.capabilities.EditCapability;
import cern.gp.nodes.GPNode;

/**
 * The action for "editing" something. An Object that wants to support
 * this action (i.e. it wants to be "edited") has to implement the EditCapability
 * and has to declare in its BeanInfo that it supports the EditAction.
 *
 * @see cern.gp.capabilities.EditCapability
 * @see cern.gp.beans.BeanInfoSupport
 * @author  Vito Baggiolini
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public class EditAction extends BeanAction {
  
  /** 
   * Creates a new instance 
   * do not call this directly, use SystemActions.get() instead
   */
  public EditAction() {
    super(EditCapability.class);
  }
  
  public String getName() {
    return "&Edit";
  }
  
  /** Performs the capability for the given node.
   * @param node the node for which the capability is performed
   * @param capability the capability to perform
   *
   */
  protected void performCapability(GPNode node, Capability capability) {
    EditCapability editCapability = (EditCapability) capability;
    editCapability.edit(node);
  }
  
}
