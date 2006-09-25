/*
 * $Id: ChildrenManager.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.nodes.children;

import java.util.Comparator;

/**
 * A class implementing this interface is able to manage
 * a collection of nodes that can be sorted or not depending of the
 * given <code>Comparator</code>.
 *
 * @see BeanComparatorAdapter
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public interface ChildrenManager {
  /**
   * Returns a comparator that can be used to sort the collection of nodes 
   * managed by this class. <code>null</code> can be returned to specify 
   * that the collection should not be sorted.
   * If a comparator is returned it should be able to compare two nodes belonging
   * to the children managed by this manager. An adapter can be used to deal with 
   * the bean associated to the node instead of the nodes itself.
   * @return a comparator for the node or  <code>null</code>
   * @see BeanComparatorAdapter
   */
  public Comparator getComparator();
  
}
