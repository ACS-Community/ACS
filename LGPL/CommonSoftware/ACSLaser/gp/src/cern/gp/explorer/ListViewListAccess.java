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

import javax.swing.JList;

import org.openide.explorer.view.ListView;

/**
 * An interface a ListView can implements to give pubilc access to its underlying JList
 * @version $Id: ListViewListAccess.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 * @author Lionel Mestre
 */
public interface ListViewListAccess {
  
  /**
   * Returns the underlying JList of this ListView
   * @return
   */
  public JList getList();

  /**
   * Returns the the list view implementing this interface
   * @return
   */
  public ListView getListView();

}

