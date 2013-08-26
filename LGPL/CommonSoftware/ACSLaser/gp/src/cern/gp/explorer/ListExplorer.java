/*
 * EaNavigatorPanel.java
 *
 * Created on September 19, 2002, 5:28 PM
 */
package cern.gp.explorer;

import java.awt.BorderLayout;
import java.beans.IntrospectionException;

import javax.swing.JList;

import org.openide.explorer.view.ListView;

import cern.gp.nodes.GPNode;
import cern.gp.nodes.impl.NodeUtils;

/**
 * An Explorer GUI Component that displays Children of a root node as a List.
 * It is recommended that the developer uses the NodeFactory and ChildrenListManager or ChildrenMapManager
 * to create the Node hierarchy to be explored.
 *
 * @author  Vito Baggiolini
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 */
public class ListExplorer extends GPListExplorerPanel {
  
  private final ListViewListAccess listViewListAccess;
  
  //
  // -- CONSTRUCTORS -----------------------------------------------
  //
  
  /**
   * Creates an explorer with the default model and the default view
   */
  public ListExplorer() {
    this(new GPListView());
  }
  
  /**
   * create a ListExplorer and set the nodes to display
   * @param nodes the nodes to display
   */
  public ListExplorer(GPNode[] nodes) {
    this();
    this.setListNodes(nodes);
  }
    
  /**
   * Creates an explorer with the specified view
   * @param view the ListView to be used in this explorer
   */
  protected ListExplorer(ListViewListAccess view) {
    super();
    this.listViewListAccess = view;
    setLayout(new BorderLayout());
    add(view.getListView(), BorderLayout.CENTER);
  }
  
  //
  // -- PUBLIC METHODS ---------------------------------------------
  //
  
  /**
   * getter method
   * @return the ListView instance used in this explorer
   */
  public ListViewListAccess getListAccess() {
    return listViewListAccess;
  }

  /**
   * Set the list of nodes to be displayed. This method first checks whether all nodes have a common 
   * parent and calls the method {@link #setRootNode(GPNode)} (with you should have called instead of this one).
   * If not, it creates a root node and creates a ChildrenManager for the nodes. In other words, you cannot use
   * this method if you want to use your own ChildrenManager implementation to control the displayed list of nodes.
   * Use this method preferably if you have an array of nodes without a common parent, and if you do not need 
   * to use your own ChildrenManager. 
   * <P>Note that you can still get hold of the siblings of one of your GPNode object with the 
   * {@link GPNode#getNodeCollection} method.
   *
   * @param nodes the array nodes to be displayed in the ListExplorer
   */
  public final void setListNodes(GPNode[] nodes) {
    // this method is final because it is called from the constructor.
    // You can override and un-final it, but you have to make sure that your overridden method
    // is ready when it is called from the constructor of this class!
    // Beware: Calling an instance method in a derived class from the constructor of the
    // super class is dangerous, e.g. because the method might use unitialized variables in
    // the (not yet initialized) derived class.
    if (nodes.length <=0) { return; }
    if (NodeUtils.haveOneCommonParent(nodes)) {
      setRootNode(nodes[0].getParent());
    } else {
      try {
        Object rootBean = new NamedBean("");  
        setRootBean(rootBean);
        this.setRootNode(createRootNode(rootBean, nodes)); 
      } catch (IntrospectionException ex) {
        ex.printStackTrace();
      }
    }
  }


  //
  // -- INNER CLASSES ---------------------------------------------
  //
  
  //
  // -- INNER CLASS ----------------------------------------------
  //
  
  /**
   * This class is a regular list view that gives access to the underlying list from Swing.
   * 
   * @version $Id: ListExplorer.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
   * @author Lionel Mestre
   */
  private static class GPListView extends ListView implements ListViewListAccess {
    
    public GPListView() {
      super();
    }

    public JList getList() {
      return super.list;
    }

    /* (non-Javadoc)
     * @see cern.gp.explorer.ListViewListAccess#getListView()
     */
    public ListView getListView() {
      return this;
    }
  }
  
}