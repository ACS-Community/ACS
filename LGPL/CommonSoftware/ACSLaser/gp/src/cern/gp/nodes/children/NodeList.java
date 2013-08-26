/*
 * $Id: NodeList.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.nodes.children;

import java.util.Iterator;

import cern.gp.nodes.GPNode;

/**
 * A class implementing this interface hold a collection of nodes
 * as a List.
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public interface NodeList extends NodeCollection {

  /**
   * Adds the new node to this list
   * @param node the node to add to this list
   */
  public void addNode(GPNode node);

  /**
   * Adds all nodes in the given array to this list
   * @param nodes an array of nodes to add to this list
   */
  public void addNodes(GPNode[] nodes);

  /**
   * Removes the node from this list
   * @param node the node to remove from the list
   * @return true if a node has been removed false else.
   */
  public boolean removeNode(GPNode node);

  /**
   * Removes all nodes from this list
   * @param nodes the nodes to remove from the list
   * @return true if all nodes have been removed false else.
   */
  public boolean removeNodes(GPNode[] nodes);

  /**
   * Returns an iterator over the nodes in this list in proper sequence.
   * @return an iterator over the nodes in this list in proper sequence.
   */
  public Iterator iterator();
    
}
