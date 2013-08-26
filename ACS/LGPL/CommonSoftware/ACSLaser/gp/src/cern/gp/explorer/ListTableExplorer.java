package cern.gp.explorer;

import java.awt.BorderLayout;
import java.beans.IntrospectionException;

import javax.swing.JTable;
import javax.swing.JTree;

import org.openide.explorer.view.NodeTableModel;
import org.openide.explorer.view.TreeTableView;
import org.openide.explorer.view.TreeView;
import org.openide.nodes.Node;

import cern.gp.nodes.GPNode;
import cern.gp.nodes.impl.NodeUtils;


/**
 * A GUI component that combines a List with a Table, with the List on the left side and the Table on the right.
 * Each node of the list corresponds to a row in the table. It can be used to explore JavaBeans in the
 * Tree and show ther properties in the Table.
 * This class shall simplify the task of creating and parametrizing this type of explorer.
 * As the other Explorers provided by the GP project, it is strongly based on JavaBeans:
 * The nodes in the List contain JavaBeans-compliant objects ("domain beans"), and the rows in the Table display
 * the properties of these domain beans.<p>
 * The implementation enables the developer to create a Explorer that s/he can easily place into
 * a NetBeans Mode. It has methods to easily configure the colums of the Table.
 * It is recommended that the developer uses the NodeFactory to create the Node hierarchy to be explored.
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Vito Baggiolini
 */
public class ListTableExplorer extends GPListExplorerPanel implements TableHolder, TablePropertyHolder {
  
  private static final String ATTR_COMPARABLE_COLUMN = "ComparableColumnTTV"; // NOI18N
  private static final String ATTR_SORTING_COLUMN = "SortingColumnTTV"; // NOI18N
  private static final String ATTR_DESCENDING_ORDER = "DescendingOrderTTV"; // NOI18N
  private final TableHolder tableHolder;
  
  private GPNode beanNode;
  private String[] tableColumnNames; 
  private final TreeTableViewTableAccess treeTableViewTableAccess;
  
  /**
   * create a ListTableExplorer with the default view and model.
   */
  public ListTableExplorer() {
    this(new GPTreeTableView());
  }
  
  /**
   * create a ListTableExplorer and set the nodes to display
   * @param nodes the nodes to display
   */
  public ListTableExplorer(GPNode[] nodes) {
    this();
    this.setListNodes(nodes);
  }
  
  protected ListTableExplorer(TreeTableViewTableAccess view) {
    super();
    this.treeTableViewTableAccess = view;
    setLayout(new BorderLayout());
    add(view.getTreeTableView(), BorderLayout.CENTER);
    tableHolder = new TableHolderImpl(this);
  }
  
  /**
   * create a TreeTableExplorer and specify the class of the bean from which the Table rows
   * shall be inferred. This constructor works if you want to display the properties of only
   * one Bean in the Table. Otherwise use another constructor.
   *
   * @param viewableBeanClass the bean class whose properties shall be displayed in the Table
   * @deprecated this method is only used for a real SortableTreeTableExplorer
   */
  public ListTableExplorer(Class viewableBeanClass) {
    this(new Class[] { viewableBeanClass });
  }
  
  /**
   * create a TreeTableExplorer and specify which beans shall be visible in the Table part of the
   * TreeTable. Only Bean classes that equal or inherit from one of the classes passed to this constructor
   * will have their properties displayed in the Table. The others table cells will be empty.
   * 
   * @param viewableBeanClass the bean classes whose properties shall be displayed in the Table
   * @deprecated this method is only used for a real SortableTreeTableExplorer
   */
  public ListTableExplorer(Class[] viewableBeanClasses) {
    this(new GPTreeTableView(new TableHolderImpl.SelectiveTableModel(viewableBeanClasses)));
  }
  
  /**
   * Getter Method
   * @return the TreeTableView of this TreeTableExplorer
   */
  public TreeTableViewTableAccess getTreeTableAccess() {
    return treeTableViewTableAccess;
  }

  /**
   * set the nodes to be displayed in the table.
   * @param nodes an array of nodes each associated with a Bean.
   */
  public final void setListNodes(final GPNode[] nodes) {
    // this method is final because it is called from the constructor.
    // You can override and un-final it, but you have to make sure that your overridden method
    // is ready when it is called from the constructor of this class!
    // Beware: Calling an instance method in a derived class from the constructor of the
    // super class is dangerous, e.g. because the method might use unitialized variables in
    // the (not yet initialized) derived class.
    if (nodes.length <=0) { return; }
    beanNode = nodes[0];
    
    // if any of the nodes has children of their own which would be displayed in the tree,
    // we create an array of new GPNodes
    // TODO this should be based on FilterNode,  
    // TODO and then it should be merged with the ListExplorer.setListNodes
    boolean copyNodes = !areAllNodesLeaves(nodes);
    
    try {
      GPNode[] nodesToSet = copyNodes ? NodeUtils.copyToNewGPNodeArray(nodes) : nodes;
      
      Object rootBean = new NamedBean("");
      setRootBean(rootBean);
      this.setRootNode(createRootNode(rootBean, nodesToSet)); 
    } catch(IntrospectionException ex) {
      ex.printStackTrace();
      return;
    }
    
    try {
      setTableColumns(beanNode);
    } catch (IntrospectionException ex) {
      ex.printStackTrace();
      return;
    }
  }
  
  /**
   * set the parent node of the list of nodes to be explored. The parent node is not
   * displayed, only its children nodes.
   * Use this method 
   * <ul>
   * <li>if you already have a root node whose children you want to display</li>
   * <li>if you want to control the nodes with your own ChildrenManager as shown below</li>
   * </ul>
   * Otherwise, you should use {@link #setListNodes(GPNode[])}
   * 
   * @param parentNode the parent of the nodes to be displayed in the List
   */
  public void setRootNode(GPNode node){
    super.setRootNode(node);
    // refactored into superclass: getExplorerManager().setRootContext(node.getPeerNode());
    getTreeTableAccess().getTreeTableView().setRootVisible(false);
    this.setTableColumns(node, null); //???
  }
  
  /**
   * set the preferred size of the TableExplorer
   */
  public void setPreferredSize(java.awt.Dimension dim) {
    getTreeTableAccess().getTreeTableView().setPreferredSize(dim);
  }
  
  /**
   * set the preferred width of the whole TableExplorer
   */
  public void setTreePreferredWidth(int width) {
    getTreeTableAccess().getTreeTableView().setTreePreferredWidth(width);
  }
  
  /**
   * set the preferred width of the specified column
   */
  public void setTableColumnPreferredWidth(int index, int width) {
    getTreeTableAccess().getTreeTableView().setTableColumnPreferredWidth(index, width);
  }

  /**
   * set the columns to be displayed. Note that before this method is called
   * you must have called {@link #setListNodes(GPNode[])}
   */
  public void setTableColumns(String[] propNames) throws IntrospectionException {
    if (beanNode == null) { throw new IntrospectionException("explored beans not yet known, use setTableNodes() first!"); }
    tableHolder.setTableColumns(beanNode, propNames); 
    tableColumnNames = propNames;
  }
  
  /**
   * Accessor method, the counterpart to {@link #setTableColumns(String[])}
   *
   * @return the names of the currently accessed table colums
   */
  public String[] getTableColumnNames() {
    // TODO this should be implemented in a method TableHolderImpl.getTableColumns
    return tableColumnNames;
  }
  //
  //----  implements TableHolder ----------------------------------------------
  //
  // 
  public void setTableColumns(GPNode node, String[] propNames) {
    tableHolder.setTableColumns(node, propNames);
  }
  
  public void setTableColumns(Object bean) throws IntrospectionException {
    tableHolder.setTableColumns(bean);
  }
  
  public void setTableColumns(Object bean, String[] propNames) throws IntrospectionException {
    tableHolder.setTableColumns(bean, propNames);
  }
  
  /* (non-Javadoc)
   * @see cern.gp.explorer.TableHolder#setTableColumns(java.lang.Class[], java.lang.String[])
   */
  public void setTableColumns(Class[] propTypes, String[] propNames) {
    tableHolder.setTableColumns(propTypes, propNames);
  }
	/**
	 * @since 2.0.6
	 */
	public void setTableColumns(Class[] propTypes, String[] propNames, boolean[] sortable) {
		tableHolder.setTableColumns(propTypes, propNames, sortable);
	}

	/**
	 * @since 2.0.6
	 */
	public void setTableColumns(GPNode node, String[] propNames, boolean[] sortable) {
		tableHolder.setTableColumns(node, propNames, sortable);
	}

	/**
	 * @since 2.0.6
	 */
	public void setTableColumns(Object bean, String[] propNames, boolean[] sortable) throws IntrospectionException {
		tableHolder.setTableColumns(bean, propNames, sortable);
	}

  /** @deprecated use setTableColumns(Object) or setTableColumns(GPBean, String[]) instead */
  public void setTableColumns(Class beanClass) throws IntrospectionException {
    tableHolder.setTableColumns(beanClass);
  }

  /** @deprecated use setTableColumns(Object, String[]) or setTableColumns(GPBean, String[]) instead */
  public void setTableColumns(Class beanClass, String[] propNames) throws IntrospectionException {
    tableHolder.setTableColumns(beanClass, propNames);
  }
  
  //
  //----  implements TablePropertyHolder --------------------------------------
  //
  // 
  public void setProperties(Node.Property[] props, boolean[] sortable) {
  	if (sortable != null) {
			for (int ix = 0; ix < props.length; ix++) {
				if (sortable[ix]) {
					props[ix].setValue(ATTR_COMPARABLE_COLUMN, Boolean.TRUE);
				}
			}
		} else {
			for (int ix = 0; ix < props.length; ix++) {
				props[ix].setValue(ATTR_COMPARABLE_COLUMN, Boolean.TRUE);
			}
		}
    if (isVisible()) {
      setVisible(false);
      treeTableViewTableAccess.getTreeTableView().setProperties(props);
      setVisible(true);
    } else {
      treeTableViewTableAccess.getTreeTableView().setProperties(props);
    }
  }
  
  /**
   * helper method, finds out if all nodes are Leaves or if any of the
   * nodes has children
   * @param nodes an array of nodes
   * @return true if all are leaves, false if at least one node has children
   */
  private boolean areAllNodesLeaves(GPNode[] nodes) {
    for (int ix=0; ix<nodes.length; ix++) {
      if (nodes[ix].getNodeCollection() != null) return false;
    }
    return true;
  }

  //
  // -- INNER CLASS ----------------------------------------------
  //
  
  /**
   * This class is a regular table view that gives access to the underlying table and tree from Swing.
   * 
   * @version $Id: ListTableExplorer.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
   * @author Lionel Mestre
   */
  private static class GPTreeTableView extends TreeTableView implements TreeTableViewTableAccess {
    
    /**
     * @param arg0
     */
    public GPTreeTableView(NodeTableModel model) {
      super(model);
    }

    public GPTreeTableView() {
      super();
    }

    public JTree getTree() {
      return super.tree;
    }

    public JTable getTable() {
      return super.treeTable;
    }

    /* (non-Javadoc)
     * @see cern.gp.explorer.TreeTableViewTableAccess#getTreeTableView()
     */
    public TreeTableView getTreeTableView() {
      return this;
    }

    /* (non-Javadoc)
     * @see cern.gp.explorer.TreeViewTreeAccess#getTreeView()
     */
    public TreeView getTreeView() {
      return this;
    }
  }

}