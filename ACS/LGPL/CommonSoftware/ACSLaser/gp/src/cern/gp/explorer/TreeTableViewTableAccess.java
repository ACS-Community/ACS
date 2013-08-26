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

import javax.swing.JTable;

import org.openide.explorer.view.TreeTableView;

/**
 * An interface a TableView can implements to give pubilc access to its underlying JTable
 * @version $Id: TreeTableViewTableAccess.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 * @author Lionel Mestre
 */
public interface TreeTableViewTableAccess extends TreeViewTreeAccess {
  
  /**
   * Returns the underlying JTable of this TableView
   * @return
   */
  public JTable getTable();
  
  /**
   * Returns the the tree table view implementing this interface
   * @return
   */
  public TreeTableView getTreeTableView();
}

