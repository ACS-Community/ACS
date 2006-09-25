/*
 * Created on Mar 14, 2003
 *
 * To change this generated comment go to
 * Window>Preferences>Java>Code Generation>Code Template
 */
package cern.gp.explorer;

import java.beans.IntrospectionException;
import java.util.Iterator;

import org.openide.explorer.ExplorerPanel;

import cern.gp.nodes.GPNode;
import cern.gp.nodes.NodeFactory;
import cern.gp.nodes.children.DefaultChildrenListManager;
import cern.gp.nodes.children.NodeCollection;
import cern.gp.nodes.children.NodeList;
import cern.gp.nodes.children.NodeMap;

/**
 * @author Vito Baggiolini
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public abstract class GPListExplorerPanel extends ExplorerPanel {
  
  /**
   * do not instantiate directly
   */
  protected GPListExplorerPanel() {
    super();
    // make the explorer non-persistent so that it does not leave bad .wstcref files in system dir
    putClientProperty("PersistenceType", "Never");    
  }
  
  private GPNode rootNode;
  //the bean of the rootNode (often, a dummy bean is used because this node is not displayed in the List)
  private Object rootBean; 
  
  /**
   * set the parent node of the list of nodes to be explored. The parent node is not
   * displayed, only its children nodes.
   * Use this method
   * <ul>
   * <li>if you already have a root node whose children you want to display</li>
   * <li>if you want to control the nodes with your own ChildrenManager as shown below</li>
   * </ul>
   *
   * @param parentNode the parent of the nodes to be displayed in the List
   */
  public void setRootNode(GPNode parentNode) {
    rootNode = parentNode;
    getExplorerManager().setRootContext(parentNode.getPeerNode());
  }
  
  /**
   * accessor method
   * @return the node at the root of the explored hierarchy
   */
  public GPNode getRootNode() {
    return rootNode;
  }
	/**
   * Implementation to set the name of the explorer in the Title
   * @since 2.0.6
	 * @see java.awt.Component#setName(java.lang.String)
	 */  
  public void setName(String name) {
		if (rootBean != null && rootBean instanceof NamedBean) {
			((NamedBean) rootBean).setName(name);
		}
		super.setName(name);
	}

  /**
   * accessor method, returns the nodes set with the {@link #setListNodes(GPNode[])} method.
   * Attention: this implementation returns the order the nodes had when they were set
   * with the setListNodes() method, it not reflect the order of the nodes as they are displayed
   * in the Explorer
   * @return the nodes displayed in this explorer
   */
  public GPNode[] getListNodes() {
    // TODO implement some method like NodeCollection.toArray() instead of casting here.
    // TODO also make sure we have the same ordering of nodes, which is not guaranteed in the current implemenation
    NodeCollection coll = rootNode.getNodeCollection();
    GPNode[] ret= new GPNode[coll.getNodesCount()];
    
    if (coll instanceof NodeList) {
      Iterator it = ((NodeList)coll).iterator();
      for (int ix=0; ix< ret.length && it.hasNext(); ix++) {
        ret[ix] = (GPNode)it.next();
      }
    } else if (coll instanceof NodeMap) {
      NodeMap map = (NodeMap)coll;
      Iterator it = map.keySet().iterator();
      for (int ix=0; ix< ret.length && it.hasNext(); ix++) {
        ret[ix] = map.getNode(it.next());
      }
    } else {
      throw new RuntimeException("internal error: unknown NodeCollection class: " + coll.getClass());
    }
    return ret;
  }
  
  public abstract void setListNodes(final GPNode[] nodes);

  /**
   * Utility method, takes an array of Nodes and creates a RootNode with a default ChildrenListManager for it 
   * @param rootBean the Bean to use int he RootNode
   * @param nodes the children nodes of the rootNode
   * @return a rootNode representing the <code>rootBean</code> and with the children <code>nodes</code>
   * @throws IntrospectionException
   * @since 2.0.6
   */
  public static GPNode createRootNode(Object rootBean, GPNode[] nodes) throws IntrospectionException {
  	return NodeFactory.createNode(rootBean, new DefaultChildrenListManager(nodes));
  }
  /**
   * accessor method, can be un-finaled
   * @return
   */
  protected final Object getRootBean() {
  	return rootBean;
  }
  /**
   * accessor method, can be un-finaled
   * @param newRootBean
   */
  protected final void setRootBean(Object newRootBean) {
  	rootBean = newRootBean;
  }
}
