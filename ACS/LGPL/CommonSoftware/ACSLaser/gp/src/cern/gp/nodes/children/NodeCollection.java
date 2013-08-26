/*
 * $Id: NodeCollection.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.nodes.children;

import cern.gp.nodes.GPNode;

/**
 * A class implementing this interface hold a collection of nodes.
 * <p>
 * The nodes can be organized hierarchically so that a given collection can have a parent node. In this case, the
 * collection is the children of that parent node. Any node of the collection can be either a leaf or a parent node of
 * another collection.
 * </p><p>
 * A <code>NodeCollection</code> is managed by a <code>ChildrenManager</code> that control the addition and removal of
 * nodes.
 * </p><p>
 * A <code>NodeCollection</code> can be ordered or not. If the collection is ordered and if the sorting criteria is
 * dynamically changed, it is necessary to refresh the ordering of the collection using the method
 * <code>refreshOrdering</code>.
 * </p>
 * 
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public interface NodeCollection {

  /**
   * Signal this collection that it should refresh the ordering of its member using the sorting mecanism already in
   * place. This method should only be called in the case the collection is ordered and the ordering criteria has been
   * dynamically updated. The method should not have any effect in case the collection is not already sorted.
   */
  public void refreshOrdering();

  /**
   * Gets the parent node of this collection of children
   * @return the parent node of those children
   */
  public GPNode getParentNode();

  /**
   * Returns the <code>ChildrenManager</code> this collection is managed with
   * @return the <code>ChildrenManager</code> this collection is managed with
   */
  public ChildrenManager getChildrenManager();

  /**
   * Gets the number of nodes in the collection.
   * @return the count
   */
  public int getNodesCount();

  /**
   * Finds a child node by name.
   * @param <code>name</code> the name of the child node to 
   * find or null if any arbitrary child may be returned
   * @return return the node or null if it could not be found
   */
  public GPNode findChildByName(String name);

  /**
   * Removes all nodes from this collection
   */
  public void clear();
}
