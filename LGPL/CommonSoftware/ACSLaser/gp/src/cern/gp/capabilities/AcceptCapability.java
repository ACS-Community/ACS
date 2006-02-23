/*
 * $Id: AcceptCapability.java,v 1.1 2005/06/07 03:26:13 kzagar Exp $
 *
 * $Date: 2005/06/07 03:26:13 $
 * $Revision: 1.1 $
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.capabilities;

import cern.gp.nodes.GPNode;

/**
 * Capability an object implements so that it can be accepted.
 * This capability is invoked by the corresponding Action
 *
 * @see cern.gp.actions.AcceptAction
 * @author Vito Baggiolini
 * @version $Revision: 1.1 $ $Date: 2005/06/07 03:26:13 $
 */
public interface AcceptCapability extends Capability {
 
  /** 
   * Call to the accept action on the object that implements this capability.
   * @param node the node representing the bean on which the capability has been activated
   */
  public void accept(GPNode node);
  
}
