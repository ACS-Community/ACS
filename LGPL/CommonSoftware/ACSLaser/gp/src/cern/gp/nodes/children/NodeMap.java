/*
 * $Id: NodeMap.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.nodes.children;

import java.util.Map;
import java.util.Set;

import cern.gp.nodes.GPNode;

/**
 * A class implementing this interface hold a collection of nodes
 * as a Map.
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public interface NodeMap extends NodeCollection {
  
  /**
   * Adds the new node to the map using the given key
   * @param key the key of the node to add
   * @param node the node to add to the map
   */
  public void addNode(Object key, GPNode node);


  /**
   * Adds the new node to the map using the name returned by the method <code>node.getName()</code> as the key.
   * @param node the node to add to the map
   */
  public void addNode(GPNode node);


  /**
   * Adds all nodes in the given array to the map using the name returned by the method <code>node.getName()</code> 
   * as the key for each node.
   * @param nodes the nodes to add to the map
   */
  public void addNodes(GPNode[] nodes);

  /**
   * Adds all nodes in the given map to this node map. The key for each node in the given map is used as the key in this 
   * node map.
   * @param nodesMap the nodes to add to this map
   */
  public void addNodes(Map nodesMap);

  /**
   * Removes the node associated to the given key
   * @param key the key of the node to remove
   * @return the node that has been removed or null if the key
   * did not match any node.
   */
  public GPNode removeNode(Object key);

  /**
   * Removes all nodes associated to the given keys
   * @param keys the keys of the nodes to remove
   */
  public void removeNodes(Object[] keys);

  /**
   * Returns the node associated to the given key or null
   * @param key the key of the node to return
   * @return the node associated with the key or null
   */
  public GPNode getNode(Object key);

  /**
   * Returns a set view of the keys contained in this map.
   * @return a set view of the keys contained in this map.
   */
  public Set keySet();
}
