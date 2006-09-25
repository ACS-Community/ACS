/*
 * $Id: CopyAction.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
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
import cern.gp.capabilities.CopyCapability;
import cern.gp.nodes.GPNode;

/**
 * The action for "copying" something. An Object that wants to support
 * this action (i.e. it wants to be "copied") has to implement the CopyCapability
 * and has to declare in its BeanInfo that it supports the CopyAction.
 *
 * @see cern.gp.capabilities.CopyCapability
 * @see cern.gp.beans.BeanInfoSupport
 * @author  Vito Baggiolini
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public class CopyAction extends BeanAction {
  
  /** 
   * Creates a new instance 
   * do not call this directly, use SystemActions.get() instead
   */
  public CopyAction() {
    super(CopyCapability.class);
  }
  
  public String getName() {
    return "&Copy";
  }
  
  /** Performs the capability for the given node.
   * @param node the node for which the capability is performed
   * @param capability the capability to perform
   *
   */
  protected void performCapability(GPNode node, Capability capability) {
    CopyCapability copyCapability = (CopyCapability) capability;
    copyCapability.copy(node);
  }
  
  protected boolean surviveFocusChange() { return true; }
}
