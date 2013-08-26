/*
 * $Id: CallableSystemAction.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.actions.support;

import org.openide.util.HelpCtx;

/**
 * Generic action that should be used as a parent class for all non contextual actions.
 * <p>
 * Subclasses should overwrite the following methods in order to provide
 * custom name and icon (if not overwritten a generic name and icon will
 * be used) :
 * <ul>
 * <li><code>getName</code></li>
 * <li><code>iconResource</code></li>
 * </ul>
 * </p>
 * <p>
 * Subclasses must overwrite the following abstract methods :
 * <ul>
 * <li><code>performAction()</code></li>
 * </ul>
 * </p>
 * <p>
 * The action can be attached to any menu or button.
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public abstract class CallableSystemAction extends org.openide.util.actions.CallableSystemAction {
  
  //
  // -- CONSTRUCTORS -----------------------------------------------
  //

  //
  // -- PUBLIC METHODS -----------------------------------------------
  //

  /**
   * Human presentable name of the action. This should be
   * presented as an item in a menu.
   * @return the name of the action
   */
  public String getName() {
    return "Generic GP Callable System Action";
  }

  /**
   * Help context where to find more about the action.
   * @return the help context for this action
   */
  public HelpCtx getHelpCtx() {
    return HelpCtx.DEFAULT_HELP;
  }


  //
  // -- PROTECTED METHODS -----------------------------------------------
  //

  /**
   * Returns the pathname of the icon to use to display this action
   * @return the pathname of the icon
   */
  protected String iconResource() {
    // [PENDING] there is a problem here. THis doesn't work if this code is used in the IDE, and
    // [PENDING] causes a NullPointerException to be launched
    // [PENDING] it should return an absolute path anyway, c.f. AbstractNode.iconResource()
    //return "resources/actions.gif";
    return "org/openide/resources/actions/clean.gif";
  }

  /**
   * Performs the action for the given nodes.
   * @param activatedNodes the non null (possibly empty) array of nodes selected at the moment the action has been
   * triggered
   */
  public abstract void performAction();


}
