/*
 * $Id: NodeUpdaterListener.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.beans.impl;

/**
 * <i><font size="-1" color="#FF0000">** for internal use only** </font></i>
 * A class implementing this interface is interested in updates 
 * taking place in a NodeUpdater. Typically, a NodeUpdaterListener 
 * will be a node listening to update taking place in a bean.
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public interface NodeUpdaterListener {
  
  /**
   * Signals a name change for this listener.
   * @param newName the new name the listener should use
   */
  public void nameChanged(String newName);

  /**
   * Signals a display name change for this listener.
   * @param newDisplayName the new display name the listener should use
   */
  public void displayNameChanged(String newDisplayName);

  /**
   * Signals a short description change for this listener.
   * @param newShortDescription the new short description the listener should use
   */
  public void shortDescriptionChanged(String newShortDescription);

  /**
   * Signals a icon change for this listener.
   * @param newIcon the new icon the listener should use
   */
  public void nodeIconChanged(java.awt.Image newIcon);

  /**
   * Signals a node default action change for this listener.
   * @param newDefaultAction the new default action the listener should use
   */
  public void nodeDefaultActionChanged(String newDefaultAction);

}
