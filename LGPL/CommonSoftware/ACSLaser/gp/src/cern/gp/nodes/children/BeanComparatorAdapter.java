/*
 * $Id: BeanComparatorAdapter.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.nodes.children;

import java.util.Comparator;

import cern.gp.nodes.GPNode;

/**
 * This class adapts a <code>Comparator</code> able to compare bean objects, 
 * to a <code>Comparator</code> able to compare nodes based on those bean objects.
 * <p>
 * The nodes will be sorted as the underlying bean objects are sorted by the 
 * bean comparator passed in the constructor of this object.
 * </p>
 * <p>
 * A typical use of the <code>BeanComparatorAdapter</code> will be inside a <code>ChildrenManager</code>. 
 * The method <code>getComparator()</code> must return a <code>Comparator</code> that compares
 * nodes. But as a user you may prefer to compare at the bean level as each node
 * represents a given bean. You can therefore write a <code>Comparator</code> that compares your 
 * beans and use the <code>BeanComparatorAdapter</code> to return a node <code>Comparator</code>
 * based on your bean <code>Comparator</code> :
 * <pre>
 * public Comparator getComparator() {
 *   return new BeanComparatorAdapter(new MyBeanComparator());
 * }
 * </pre>
 * In this example, <code>MyBeanComparator</code> is a class that you write to compare
 * the beans that are used to create the node. <code>MyBeanComparator</code> only knows
 * how to compare two bean objects, instead of comparing two nodes based on those bean objects.
 * </p>
 *
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public class BeanComparatorAdapter implements Comparator {
  
  private final Comparator beanComparator;

  //
  // -- CONSTRUCTORS -----------------------------------------------
  //

  /**
   * Creates a new GPBeanNode based on the given bean with no children
   * The new node is therefore a leaf.
   */
  public BeanComparatorAdapter(Comparator beanComparator) {
    this.beanComparator = beanComparator;
  }


  //
  // -- PUBLIC METHODS -----------------------------------------------
  //
  
  //
  // -- implements Comparator interface ------------------------------
  //

  public final int compare(Object o1, Object o2) {
    GPNode b1 = (GPNode) o1;
    GPNode b2 = (GPNode) o2;
    return beanComparator.compare(b1.getBean(), b2.getBean());
  }

  public final boolean equals(Object obj) {
    return super.equals(obj);
  }
  

  //
  // -- PROTECTED METHODS -----------------------------------------------
  //

  //
  // -- PRIVATE METHODS -----------------------------------------------
  //

  //
  // -- INNER CLASSES -----------------------------------------------
  //
}
