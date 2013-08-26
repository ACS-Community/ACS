package cern.gp.explorer;

import java.beans.IntrospectionException;

import javax.swing.JTable;
import javax.swing.JTree;

import org.openide.explorer.view.NodeTableModel;
import org.openide.explorer.view.TreeTableView;
import org.openide.explorer.view.TreeView;
import org.openide.nodes.Node;

import cern.gp.nodes.GPNode;


/**
 * A GUI component that combines a Tree with a Table, whith a Tree on the left side and a table on the right.
 * Each node of the tree corresponds to a row in the table. It can be used to explore JavaBeans in the
 * Tree and show ther properties in the Table.
 * This class shall simplify the task of creating and parametrizing this type of explorer.
 * As the other Explorers provided by the GP project, it is strongly based on JavaBeans:
 * The nodes in the Tree contain JavaBeans-compliant objects, and the rows in the Table display
 * the properties of these objects.
 * The implementation enables the developer to create a Explorer that s/he can easily place into
 * a NetBeans Mode. It has methods to easily configure the colums of the Table.
 * It is recommended that the developer uses the NodeFactory and ChildrenListManager or ChildrenMapManager
 * to create the Node hierarchy to be explored.
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Vito Baggiolini
 */
public class TreeTableExplorer extends TreeExplorer implements TablePropertyHolder {
  
  private final TableHolder tableHolder;
  
  /**
   * create an TreeTableExplorer with the default view and model.
   */
  public TreeTableExplorer() {
    super(new GPTreeTableView());
    tableHolder = new TableHolderImpl(this);
  }
  
  /**
   * create a TreeTableExplorer and specify the class of the bean from which the Table rows
   * shall be inferred. This constructor works if you want to display the properties of only
   * one Bean in the Table. Otherwise use another constructor.
   *
   * @param viewableBeanClass the bean class whose properties shall be displayed in the Table
   */
  public TreeTableExplorer(Class viewableBeanClass) {
    this(new Class[] { viewableBeanClass });
  }
  
  /**
   * create a TreeTableExplorer and specify which beans shall be visible in the Table part of the
   * TreeTable. Only Bean classes that equal or inherit from one of the classes passed to this constructor
   * will have their properties displayed in the Table. The others table cells will be empty.
   *
   * @param viewableBeanClass the bean classes whose properties shall be displayed in the Table
   */
  public TreeTableExplorer(Class[] viewableBeanClasses) {
    super(new GPTreeTableView(new TableHolderImpl.SelectiveTableModel(viewableBeanClasses)));
    tableHolder = new TableHolderImpl(this);
  }
  
  /**
   * Getter Method
   * @return the TreeTableView of this TreeTableExplorer
   */
  public TreeTableViewTableAccess getTreeTableAccess() {
    return (TreeTableViewTableAccess) getTreeAccess();
  }
  
  /**
   * set the preferred size of the whole explorer + table
   * @see javax.swing.JComponent#setPreferredSize(Dimension)
   */
  public void setPreferredSize(java.awt.Dimension dim) {
    getTreeTableAccess().getTreeTableView().setPreferredSize(dim);
  }
  
  /**
   * set the width of the Tree part of the TreeTableExplorer
   * @param width the preferred width
   */
  public void setTreePreferredWidth(int width) {
    getTreeTableAccess().getTreeTableView().setTreePreferredWidth(width);
  }
  
  /**
   * set the width of a column
   * @param colIndex the index of the column (starting from left with index 0)
   * @param width the width of this column
   */
  public void setTableColumnPreferredWidth(int colIndex, int width) {
    getTreeTableAccess().getTreeTableView().setTableColumnPreferredWidth(colIndex, width);
  }
  
  //
  //----  implements TableHolder ----------------------------------------------
  //
  
  /** @deprecated */
  public void setTableColumns(Class beanClass) throws IntrospectionException {
    tableHolder.setTableColumns(beanClass);
  }
  
  /** @deprecated */
  public void setTableColumns(Class beanClass, String[] propNames) throws IntrospectionException {
    tableHolder.setTableColumns(beanClass, propNames);
  }
  
  public void setTableColumns(Class[] propTypes, String[] propNames) {
    tableHolder.setTableColumns(propTypes, propNames);
  }
  
  public void setTableColumns(Object bean, String[] propNames) throws IntrospectionException {
    tableHolder.setTableColumns(bean, propNames);
  }
  public void setTableColumns(Object bean) throws IntrospectionException {
    this.setTableColumns(bean, null);
  }
  public void setTableColumns(GPNode node, String[] propNames) {
    tableHolder.setTableColumns(node, propNames);
  }
  //
  //----  implements TablePropertyHolder --------------------------------------
  //
  
  public void setProperties(Node.Property[] props, boolean[] sortable) {
  	System.out.println(this + " warning: parameter sortable is ignored"); // TODO check
    getTreeTableAccess().getTreeTableView().setProperties(props);
  }
  
  //
  // -- INNER CLASS ----------------------------------------------
  //
  
  /**
   * This class is a regular table view that gives access to the underlying table and tree from Swing.
   * 
   * @version $Id: TreeTableExplorer.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
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
	/* (non-Javadoc)
	 * @see cern.gp.explorer.TreeExplorer#setRowHeigth(int)
	 */
	public void setRowHeigth(int rowHeight) {		
		super.setRowHeigth(rowHeight);
		this.getTreeTableAccess().getTable().setRowHeight(rowHeight);
	}

	/**
	 * @see #setRowHeight(int)
	 * @return the present row height
	 * @since 2.0.7
	 */
	public int getRowHeight() {
		int treeRowHeight = super.getRowHeight();
		int tableRowHeight = getTreeTableAccess().getTable().getRowHeight();
		if (tableRowHeight != treeRowHeight) {
			System.err.println("*** " + this + ".getRowHeight(): tableRowHeight != treeRowHeight: " + tableRowHeight + " <> " + treeRowHeight);
		}
		return tableRowHeight;
	}
	
}