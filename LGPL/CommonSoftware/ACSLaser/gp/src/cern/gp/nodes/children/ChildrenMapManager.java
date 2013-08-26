/*
 * $Id: ChildrenMapManager.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.nodes.children;

import cern.gp.nodes.children.NodeMap;

/**
 * A class implementing this interface is a manager of the children of a given node.
 * It can receive a NodeMap that provides a view on the map of the nodes that makes 
 * the children of the given node. The manager is in charge of updating it as needed 
 * adding or removing nodes using the NodeMap interface.

 * A class implementing this interface is able to receive a NodeMap and is in charge 
 * of updating it as needed adding or removing nodes.
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public interface ChildrenMapManager extends ChildrenManager {
  
  /**
   * Receives the <code>NodeMap</code> that is managed by this manager
   * and that can be used to add or remove children. 
   * <p>
   * This method is called lazily once when the children are going 
   * to be displayed. It is similar to the <code>addNotify()</code> 
   * of a GUI component.
   * </p>
   * <p>
   * The implementation of this method should do two things. First it
   * should initialize the map of children with the current children.
   * For that it can use the method <code>addNode</code> of the nodeMap.
   * Second, and only if the children are dynamic and are changing based 
   * on some external events, it should keep the reference to the given
   * nodeMap for future use.
   * </p>
   * <p>
   * If the children are not dynamic, which means that they don't change
   * after they have been initialized here, there is no need to keep a 
   * reference to the nodeMap.
   * </p>
   * @param nodeMap the map representing the children 
   * managed by this manager
   */
  public void initChildrenMap(NodeMap nodeMap);
  
}
