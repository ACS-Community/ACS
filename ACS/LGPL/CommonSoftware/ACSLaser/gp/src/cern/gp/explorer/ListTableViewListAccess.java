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

import org.openide.explorer.view.ListTableView;

/**
 * An interface a ListTableView can implements to give pubilc access to its underlying JTable
 * @version $Id: ListTableViewListAccess.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 * @author Lionel Mestre
 */
public interface ListTableViewListAccess extends ListViewListAccess {
  
  /**
   * Returns the underlying JTable of this TableView
   * @return
   */
  public JTable getTable();
  
  /**
   * Returns the the list table view implementing this interface
   * @return
   */
  public ListTableView getListTableView();
}

