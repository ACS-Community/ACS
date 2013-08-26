/*
 * $Id: NodeFactory.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.nodes;

import java.beans.IntrospectionException;

import cern.gp.nodes.children.ChildrenManager;
import cern.gp.nodes.impl.GPBeanNode;

/**
 * This factory can be used to build a node representing a given bean. The node will extract the information it needs
 * directly from the bean. As nodes are used to build up a hierarchy of objects (a node having children that can also
 * have children) and as beans don't carry the concept of a hierarchy, it is possible to specify the children of the
 * node representing a given bean by passing a <code>ChildrenManager</code>.
 * <p>
 * Example of creating a node with children
 * </p>
 * <pre>
 * GPNode node = NodeFactory.createNode(equipmentBean, new EquipmentChildrenListManager(euipmentBean));
 * </pre>
 * <p>
 * Example of creating a node without children
 * </p>
 * <pre>
 * GPNode leaf = NodeFactory.createNode(equipmentBean);
 * </pre>
 * 
 * @see GPNode
 * @see cern.gp.nodes.children.ChildrenManager
 * 
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public class NodeFactory {
  
  /** cannot be instantiated */
  private NodeFactory() {}
  
  
  /**
   * Instantiates a new Node for the given bean and NodeManager
   * @param bean the bean the new node represents
   * @param childrenManager the children manager controlling the children of the new node
   * @return a Node representing the bean whose children are controlled by the given manager
   */
  public static GPNode createNode(Object bean, ChildrenManager childrenManager) throws IntrospectionException {
    return new GPBeanNode(bean, childrenManager);
  }
  

  /**
   * Instantiates a new leaf Node for the given bean
   * @param bean the bean the new node represents
   * @return a leaf Node representing the bean
   */
  public static GPNode createNode(Object bean) throws IntrospectionException {
    return new GPBeanNode(bean);
  }


  /**
   * Instantiates new leaf Nodes for the given beans
   * @param beans the beans to represent by nodes
   * @return an array of leaf Nodes representing the beans
   */
  public static GPNode[] createNode(Object[] beans) throws IntrospectionException {
    GPNode[] nodes = new GPNode[beans.length];
    for (int i = 0; i < beans.length; i++) {
      nodes[i] = new GPBeanNode(beans[i]);
    }
    return nodes;
  }

  //
  // -- INNER CLASSES -----------------------------------------------
  //
  

}
