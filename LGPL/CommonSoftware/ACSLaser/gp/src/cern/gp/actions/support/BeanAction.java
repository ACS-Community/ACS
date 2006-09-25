/*
 * $Id: BeanAction.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.actions.support;

import java.lang.reflect.InvocationTargetException;

import cern.gp.capabilities.Capability;
import cern.gp.nodes.GPNode;
import cern.gp.util.GPManager;

import javax.swing.JButton;
import org.openide.nodes.Node;
import org.openide.util.HelpCtx;
import org.openide.util.actions.CookieAction;

/**
 * Generic action that should be used as a parent class for all actions
 * linked to beans in the GP Platform.
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
 * <li><code>performCapability</code></li>
 * </ul>
 * </p>
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public abstract class BeanAction extends CookieAction {
  
  
  
  /** the associated capability class as an array in order to return
   * it as an array of Cookie as NetBeans expect it to be returned */
  private Class[] capabilityClasses;
  
  
  
  
  /** The mode which describe how the action is enabled when the selection 
   * contains more than one node */
  private int mode;
  
  //
  // -- CONSTRUCTORS -----------------------------------------------
  //
  
  /**
   * Creates a new BeanAction linked a the given <code>capabilityClass</code>  
   * and with default mode <code>MODE_ANY</code>
   * @param capabilityClass the capability class associated with this action
   */
  protected BeanAction(Class capabilityClass) {
    this(capabilityClass, MODE_ANY);
  }
  
  /**
   * Creates a new BeanAction linked a the given <code>capabilityClass</code>  
   * and with the given mode
   * @param capabilityClass the capability class associated with this action
   * @param mode the mode of this action (see modes in <code>CookieAction</code>)
   */
  protected BeanAction(Class capabilityClass, int mode) {
    if (! Capability.class.isAssignableFrom(capabilityClass) || !capabilityClass.isInterface()) 
      throw new IllegalArgumentException("parameter " + capabilityClass
       + " must be an Interface and inherit from Capability");
    capabilityClasses = new Class[] {capabilityClass};
    this.mode = mode;
  }
  


  //
  // -- PUBLIC METHODS -----------------------------------------------
  //
  
  /** 
   * Human presentable name of the action. This should be
   * presented as an item in a menu.
   * @return the name of the action
   */
  public String getName() {
    return "Generic GP Action";
  }
  
  /**
   * Help context where to find more about the action.
   * @return the help context for this action
   */
  public HelpCtx getHelpCtx() {
    return HelpCtx.DEFAULT_HELP;
  }
    
  /**
   * returns a Button that can invoke this action, and that is enabled or disabled
   * properly. This is analogous to getMenuItemPresenter() or getToolBarPresenter()
   * @return a button connected to this action
   */
  public JButton createJButton() {
    return ActionUtils.createJButton(this);
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
   * Returns the set of capabilities supported by this action. Currently
   * this action only support one single capability (named cookie in NetBeans).
   * @return set of needed cookies 
   */
  protected Class[] cookieClasses() {
    return capabilityClasses;
  }
  

  protected boolean surviveFocusChange() {
    return true;
  }
  

  protected int mode() {
    return mode;
  }
  
  /**
   * Standard perform action extended by actually activated nodes.
   * @see org.openide.util.actions.CookieAction#performAction
   * @param activatedNodes gives array of actually activated nodes.
   */
  protected void performAction(final Node[] activatedNodes) {
    for (int i = 0; i < activatedNodes.length; i++) {
      Capability capability = (Capability) activatedNodes[i].getCookie(capabilityClasses[0]);
      try {
        if (capability != null) {
          if (activatedNodes[i] instanceof GPNode) {
            performCapability((GPNode) activatedNodes[i], capability);
          } else {
            performCapability(null, capability);
          }
        }
      } catch (InvocationTargetException ex) {
        GPManager.notify(GPManager.EXCEPTION, ex);
      }
    }
  }
  
  
  /**
   * Performs the capability for the given node.
   * @param node the node for which the capability is performed
   * @param capability the capability to perform
   */
  protected abstract void performCapability(GPNode node, Capability capability) throws InvocationTargetException;
  
}
