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

import javax.swing.JList;

import org.openide.explorer.view.ListView;

/**
 * An interface a ListView can implements to give pubilc access to its underlying JList
 * @version $Id: ListViewListAccess.java,v 1.1 2005/06/07 03:26:13 kzagar Exp $
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

