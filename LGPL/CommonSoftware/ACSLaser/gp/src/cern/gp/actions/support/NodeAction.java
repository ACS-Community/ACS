/*
 * $Id: NodeAction.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.actions.support;

import javax.swing.JButton;

import org.openide.nodes.Node;
import org.openide.util.HelpCtx;

import cern.gp.nodes.GPNode;

/**
 * Generic action that should be used as a parent class for all actions
 * linked to nodes in the GP Platform.
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
 * <li><code>performAction(GPNode[])</code></li>
 * </ul>
 * </p>
 * <p>
 * The action will only be enable if at least one GPNode is in the selection. It is possible to change the behavior of
 * the enable by overriding the <code>enable</code> method.
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public abstract class NodeAction extends org.openide.util.actions.NodeAction {
  
  private static final GPNode[] EMPTY_GPNODE_ARRAY = new GPNode[0];

  //
  // -- CONSTRUCTORS -----------------------------------------------
  //

  /**
   * Creates a new BeanAction linked a the given <code>capabilityClass</code>
   * and with default mode <code>MODE_ANY</code>
   * @param capabilityClass the capability class associated with this action
   */
  protected NodeAction() {
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
    return "Generic GP Node Action";
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

  protected boolean surviveFocusChange() {
    return true;
  }


  /**
   * The action will only be enabled if at least one GPNode is selected.
   * @see org.openide.util.actions.NodeAction#enable
   * @param activatedNodes gives array of actually activated nodes.
   */
  protected boolean enable(Node[] activatedNodes) {
    if ((activatedNodes == null) || (activatedNodes.length == 0)) return false;
    for (int i = 0; i < activatedNodes.length; i++) {
      if (activatedNodes[i] instanceof GPNode) {
        return true;
      }
    }
    return false;
  }

  /**
   * Standard perform action extended by actually activated nodes. Only actual instance of GPNode are taking into
   * account by this implementation. The other are ignored.
   * @see org.openide.util.actions.NodeAction#performAction
   * @param activatedNodes gives array of actually activated nodes.
   */
  protected void performAction(final Node[] activatedNodes) {
    int counter = 0;
    GPNode[] gpNodes = new GPNode[activatedNodes.length];
    for (int i = 0; i < activatedNodes.length; i++) {
      if (activatedNodes[i] instanceof GPNode) {
        gpNodes[counter] = (GPNode) activatedNodes[i];
        counter++;
      }
    }
    if (counter == activatedNodes.length) {
      performAction(gpNodes);
    } else if (counter == 0) {
      performAction(EMPTY_GPNODE_ARRAY);
    } else {
      GPNode[] temp = new GPNode[counter];
      System.arraycopy(gpNodes, 0, temp, 0, counter);
      performAction(temp);
    }
  }


  /**
   * Performs the action for the given nodes.
   * @param activatedNodes the non null (possibly empty) array of nodes selected at the moment the action has been
   * triggered
   */
  protected abstract void performAction(GPNode[] activatedNodes);


}
