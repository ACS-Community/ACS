/*
 * AcceptorBean.java
 *
 * Created on October 4, 2002, 4:30 PM
 */

package cern.gp.actions.test.helpers;

import cern.gp.capabilities.AcceptCapability;
import cern.gp.nodes.GPNode;

/**
 * A bean that implements the AcceptCapability
 * 
 * @version $Revision: 1.1 $ $Date: 2005/06/07 03:26:13 $
 * @author  Vito Baggiolin
 */
public class AcceptorBean implements AcceptCapability {
  
  /** Creates a new instance of AcceptorBean */
  public AcceptorBean() {
  }
  
  /**
   * Call to the accept action on the object that implements this capability.
   *
   */
  public void accept(GPNode node) {
    System.err.println("accept called");
  }
  
}
