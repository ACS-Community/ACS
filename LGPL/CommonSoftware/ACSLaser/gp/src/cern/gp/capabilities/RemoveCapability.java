/*
 * $Id: RemoveCapability.java,v 1.1 2005/06/07 03:26:13 kzagar Exp $
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
 * Capability an object implements to give the possibility to be removed.
 *
 * @version $Revision: 1.1 $  $Date: 2005/06/07 03:26:13 $
 * @author Lionel Mestre
 */
public interface RemoveCapability extends Capability {
  
  /** 
   * Indicates to this object that it has to be removed. The object 
   * has to interpret what does remove means in its own context.
   * @param node the node representing the bean on which the capability has been activated
   */
  public void remove(GPNode node);
  
}
