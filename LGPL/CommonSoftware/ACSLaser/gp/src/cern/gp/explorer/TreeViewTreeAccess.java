/*
 * $Id $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.explorer;

import javax.swing.JTree;

import org.openide.explorer.view.TreeView;

/**
 * An interface a TreeView can implements to give pubilc access to its underlying JTree
 * @version $Id: TreeViewTreeAccess.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 * @author Lionel Mestre
 */
public interface TreeViewTreeAccess {
  
  /**
   * Returns the underlying JTree of this TreeView
   * @return
   */
  public JTree getTree();

  /**
   * Returns the the tree view implementing this interface
   * @return
   */
  public TreeView getTreeView();
}

