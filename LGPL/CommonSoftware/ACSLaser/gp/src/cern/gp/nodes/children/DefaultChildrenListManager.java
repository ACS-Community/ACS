/*
 * Created on Mar 10, 2003
 */
package cern.gp.nodes.children;

import java.util.Comparator;

import cern.gp.nodes.GPNode;

/**
 * A default implementation of a ChildrenListManager used for a fixed number of children nodes 
 * that are known at the moment when the ChildrenListeManager is instantiated.
 * 
 * @see DefaultChildrenMapManager
 * @author Vito Baggiolini
 * @version $Revision: 1.1 $, $Date: 2005/06/07 03:26:13 $, $Author: kzagar $
 */
public class DefaultChildrenListManager implements ChildrenListManager {
	private final GPNode[] children;

	public DefaultChildrenListManager(GPNode[] childrenNodes) {
		this.children = childrenNodes;
	}
  /* (non-Javadoc)
   * @see cern.gp.nodes.children.ChildrenListManager#initChildrenList(cern.gp.nodes.children.NodeList)
   */
  public void initChildrenList(NodeList nodeList) {
  	nodeList.addNodes(children);
  }

  /* (non-Javadoc)
   * @see cern.gp.nodes.children.ChildrenManager#getComparator()
   */
  public Comparator getComparator() {
		return null;
  }
	
}