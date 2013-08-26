package cern.gp.nodes.impl;

import cern.gp.nodes.GPNode;
import cern.gp.nodes.NodeFactory;
import java.beans.IntrospectionException;

/**
 * 
 * @author Vito Baggiolini
 *
 * @version $Revision: 1.2 $, $Date: 2006/09/25 08:52:36 $
 */
public class NodeUtils {

  /**
   * do not instantiate
   */
  private NodeUtils() {
  }

  /**
   * creates an array of new GPNode objects without from the origNodes.
   * Use this in case you need to "cut" the children below your nodes for some reason, e.g. 
   * because you want to display a list of children in a Tree
   *
   * @deprecated -- this should use a FilterNode
   * @param origNodes the nodes with children that need to be cut off
   * @return an array of new GPNode objects that refer to the same beans as the GPNodes passed as argument
   */
  public static GPNode[] copyToNewGPNodeArray(GPNode[] origNodes) {
    GPNode[] noChildenGPNodes = new GPNode[origNodes.length];
    try {
      for (int ix = 0; ix < origNodes.length; ix++) {
        noChildenGPNodes[ix] = NodeFactory.createNode(origNodes[ix].getBean());
      }
    } catch (IntrospectionException ex) {
      ex.printStackTrace();
      return null;
    }
    return noChildenGPNodes;
  }

  /**
   * checks whether the nodes in the array all have a common parent
   * @return true if there is a parent and it's the same for all nodes, false otherwise
   */
  public static boolean haveOneCommonParent(final GPNode[] nodes) {
    if (nodes.length <= 0) {
      return false;
    }

    GPNode parent = nodes[0].getParent();
    if (parent == null) {
      return false;
    }

    for (int ix = 1; ix < nodes.length; ix++) {
      if (parent != nodes[ix].getParent()) {
        return false;
      }
    }

    return true;
  }

  /**
   * a method that creates a new array of GPNodes with copies of the original GPNodes
   * The domain beans associated with the GPNodes are not copied.
   * @param origNodes the array of nodes to be cloned
   * @return the array of copied nodes
   */
  public static GPNode[] cloneNodes(GPNode[] origNodes) {
    final GPNode[] clones = new GPNode[origNodes.length];

    try {
      for (int ix = 0; ix < clones.length; ix++) {
        clones[ix] = NodeFactory.createNode(origNodes[ix].getBean());
      }
    } catch (IntrospectionException e) {
      e.printStackTrace();
    }
    return clones;
  }
}