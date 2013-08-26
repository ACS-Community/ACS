/*
 * $Id: OpenLocalExplorerAction.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.actions;

import org.openide.nodes.Node;
import org.openide.util.HelpCtx;
import org.openide.util.actions.NodeAction;

/**
 * Open an Explorer window with a particular root node. 
 * Adapter of <code>org.openide.actions.OpenLocalExplorerAction</code>.
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public class OpenLocalExplorerAction extends NodeAction {
  
  protected void performAction(Node[] activatedNodes) {
    //bugfix #7579 test if nodes array is empty and node is enabled
    if ((activatedNodes != null) &&
    (activatedNodes.length == 1) &&
    (activatedNodes[0].isLeaf() == false) ) {
      org.openide.nodes.NodeOperation.getDefault().explore(activatedNodes[0]);
    }
  }
  
  protected boolean enable(Node[] activatedNodes) {
    if ((activatedNodes == null) || (activatedNodes.length != 1) ||
    (activatedNodes[0].isLeaf()))
      return false;
    return true;
  }
  
    /* Human presentable name of the action. This should be
     * presented as an item in a menu.
     * @return the name of the action
     */
  public String getName() {
    return "Open Local E&xplorer";
  }
  
    /* Help context where to find more about the action.
     * @return the help context for this action
     */
  public HelpCtx getHelpCtx() {
    return new HelpCtx(org.openide.actions.OpenLocalExplorerAction.class);
  }
  
    /* Icon resource.
     * @return name of resource for icon
     */
  protected String iconResource() {
    return "org/openide/resources/actions/openLocalExplorer.gif"; // NOI18N
  }
}
