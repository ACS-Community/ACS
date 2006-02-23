/*
 * $Id $
 *
 * $Date: 2005/06/07 03:26:13 $
 * $Revision: 1.1 $
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.explorer;

import javax.swing.JTree;

import org.openide.explorer.view.TreeView;

/**
 * An interface a TreeView can implements to give pubilc access to its underlying JTree
 * @version $Id: TreeViewTreeAccess.java,v 1.1 2005/06/07 03:26:13 kzagar Exp $
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

