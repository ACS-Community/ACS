/*
 * $Id: NodeUpdaterProvider.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.beans.impl;

/**
 * <i><font size="-1" color="#FF0000">**Experimental : for internal use only** </font></i>
 * A class implementing this interface, typically a Bean, is interested 
 * in updating the GUI based on some internal events. The <code>NodeUpdater</code>
 * object returned is able to receive a listener and provide all updates to the GUI.
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
interface NodeUpdaterProvider {
  
  /**
   * Returns a NodeUpdater
   * @return the NodeUpdater that propagates the changes 
   * taking place in the bean
   */
  public NodeUpdater getNodeUpdater();

}
