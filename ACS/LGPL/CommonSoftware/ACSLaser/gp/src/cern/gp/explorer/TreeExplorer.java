/*
 * EaNavigatorPanel.java
 *
 * Created on September 19, 2002, 5:28 PM
 */
package cern.gp.explorer;

import java.awt.BorderLayout;

import javax.swing.JTree;

import org.openide.explorer.ExplorerPanel;
import org.openide.explorer.view.BeanTreeView;
import org.openide.explorer.view.TreeView;
import cern.gp.nodes.GPNode;

/**
 * An Explorer GUI Component that displays a hierarchy of domain beans in a Tree.
 * It is recommended that the developer uses the NodeFactory and ChildrenListManager or ChildrenMapManager
 * to create the Node hierarchy to be explored.
 *
 * @author  Vito Baggiolini
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 */
public class TreeExplorer extends ExplorerPanel {
  
  private GPNode rootNode; // for getRootNode()

  private final TreeViewTreeAccess treeViewTreeAccess;

  //
  // -- CONSTRUCTORS -----------------------------------------------
  //

  /**
   * Creates an explorer with the default model and the default view
   */
  public TreeExplorer() {
    this(new GPBeanTreeView());
  }

  /**
   * Creates an explorer with the specified view
   * @param view the TreeView to be used in this explorer
   */
  protected TreeExplorer(TreeViewTreeAccess view) {
    super();
    // make the explorer non-persistent so that it does not leave bad .wstcref files in system dir
    putClientProperty("PersistenceType", "Never");
    this.treeViewTreeAccess = view;
    setLayout(new BorderLayout());
    add(view.getTreeView(), BorderLayout.CENTER);
  }

  //
  // -- PUBLIC METHODS ---------------------------------------------
  //

  /**
   * getter method
   * @return the TreeView instance used in this explorer
   */
  public TreeViewTreeAccess getTreeAccess() {
    return treeViewTreeAccess;
  }

  /**
   * set the root node of the hierarachy to be explored
   * @param node the root node
   */
  public void setRootNode(GPNode node) {
    this.rootNode = node;
    getExplorerManager().setRootContext(node.getPeerNode());
  }
  
  /**
   * accessor method
   * @return the node at the root of the explored hierarchy
   */
  public GPNode getRootNode() {
    return rootNode;
  }

  /**
   * set the height of the rows as in {@link JTree#setRowHeight(int)} or {@link javax.swing.JTable#setRowHeight(int)}
   * @param rowHeight
   * @since 2.0.7
   */
  public void setRowHeigth(int rowHeight) {
  	this.getTreeAccess().getTree().setRowHeight(rowHeight);
  }
  
	/**
	 * @see #setRowHeight(int)
	 * @return the present row height
   * @since 2.0.7
	 */
	public int getRowHeight() {
		return getTreeAccess().getTree().getRowHeight();
	}
  
  //
  // -- INNER CLASS ----------------------------------------------
  //

  private static class GPBeanTreeView extends BeanTreeView implements TreeViewTreeAccess {
    
    public GPBeanTreeView() {
      super();
    }

    public JTree getTree() {
      return super.tree;
    }

    /* (non-Javadoc)
     * @see cern.gp.explorer.TreeViewTreeAccess#getTreeView()
     */
    public TreeView getTreeView() {
      return this;
    }
  }

  
}
