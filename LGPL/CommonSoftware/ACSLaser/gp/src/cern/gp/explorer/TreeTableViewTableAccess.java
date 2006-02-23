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

import javax.swing.JTable;

import org.openide.explorer.view.TreeTableView;

/**
 * An interface a TableView can implements to give pubilc access to its underlying JTable
 * @version $Id: TreeTableViewTableAccess.java,v 1.1 2005/06/07 03:26:13 kzagar Exp $
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

