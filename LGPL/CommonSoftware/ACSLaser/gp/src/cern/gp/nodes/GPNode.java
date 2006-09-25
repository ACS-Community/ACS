/*
 * $Id: GPNode.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.nodes;

import org.openide.nodes.Node;

import cern.gp.nodes.children.NodeCollection;

/**
 * A class implementing this interface is a node that extends the concept of beans. A node is a bean that can have
 * children. The children (a <code>NodeCollection</code>) are managed by a <code>ChildrenManager</code>.
 * <p>
 * <code>GPNode</code> is the same concept as <code>Node</code> in NetBeans, but wraps the API to make easier to
 * understand and use.
 * </p>
 *
 * @see org.openide.nodes.Node
 * @see NodeFactory
 * @see cern.gp.nodes.children.NodeCollection
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public interface GPNode {
  
  /**
   * Returns the name of this node
   * @return the name of this node
   */
  public String getName();

  /**
   * Returns the bean this node is based upon
   * @return the bean this node is based upon
   */
  public Object getBean();
  
  /**
   * Test whether the node is a leaf, or may contain children.
   * @return true if the children list is actually Children.LEAF
   */
  public boolean isLeaf();

  /**
   * Returns the children of this node. Null can be returned is this node is a leaf and does not have children.
   * @return the children of this node or null
   */
  public NodeCollection getNodeCollection();

  /**
   * Returns the parent node of this node. A node has a parent node if it is member of the children of that node. If
   * this node has no parent (it is a root node), null is returned.
   * @param the parent node of this node or null
   */
  public GPNode getParent();
  
  /**
   * Returns the NetBeans nodes representing this node
   * @param the NetBeans nodes representing this node
   */
  public Node getPeerNode();
  
}
