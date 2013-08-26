/*
 * $Id: ChildrenListManager.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.nodes.children;

import cern.gp.nodes.children.NodeList;

/**
 * A class implementing this interface is a manager of a list of children of a given node.
 * It can receive a NodeList that provides a view on the list of nodes that makes 
 * the children of the given node. The manager is in charge of updating it as needed 
 * adding or removing nodes using the NodeList interface.
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public interface ChildrenListManager extends ChildrenManager {
  
  /**
   * Receives the <code>NodeList</code> that is managed by this manager
   * and that can be used to add or remove children.
   * <p>
   * This method is called lazily once when the children are going 
   * to be displayed. It is similar to the <code>addNotify()</code> 
   * of a GUI component.
   * </p>
   * <p>
   * The implementation of this method should do two things. First it
   * should initialize the list of children with the current children.
   * For that it can use the method <code>addNode</code> of the nodeList.
   * Second, and only if the children are dynamic and are changing based 
   * on some external events, it should keep the reference to the given
   * nodeList for future use.
   * </p>
   * <p>
   * If the children are not dynamic, which means that they don't change
   * after they have been initialized here, there is no need to keep a 
   * reference to the nodeList.
   * </p>
   * @param nodelist the list representing the children 
   * managed by this manager
   */
  public void initChildrenList(NodeList nodeList);
  
}
