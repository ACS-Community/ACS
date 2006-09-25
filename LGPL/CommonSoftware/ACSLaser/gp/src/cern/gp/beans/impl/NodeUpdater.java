/*
 * $Id: NodeUpdater.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.beans.impl;

import cern.gp.beans.GPBean;

/**
 * <i><font size="-1" color="#FF0000">**For internal use only** </font></i>
 * A class implementing this interface, typically a Bean, produces
 * events when changes occur notifying the <code>NodeUpdaterListener</code>.
 * Usually a JavaBean or a support class will implement this interface to 
 * update a node dynamically.
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public interface NodeUpdater extends GPBean {
  
  /**
   * Adds the given NodeUpdaterListener that will be notified of any change
   * in one of the property.
   * @param <code>listener</code> the listener to register
   */
  public void addNodeUpdaterListener(NodeUpdaterListener listener);


  /**
   * Removes the given listener
   * @param <code>listener</code> the listener to remove
   */
  public void removeNodeUpdaterListener(NodeUpdaterListener listener);
  
}
