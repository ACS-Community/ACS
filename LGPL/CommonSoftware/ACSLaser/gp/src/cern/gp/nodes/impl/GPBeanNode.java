/*
 * $Id: GPBeanNode.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.nodes.impl;

import java.beans.IntrospectionException;

import org.openide.nodes.Children;
import org.openide.nodes.Index;
import org.openide.nodes.Node;
import org.openide.nodes.Sheet;
import org.openide.nodes.Sheet.Set;
import org.openide.util.HelpCtx;

import cern.gp.nodes.GPNode;
import cern.gp.nodes.children.ChildrenFactory;
import cern.gp.nodes.children.ChildrenManager;
import cern.gp.nodes.children.NodeCollection;

/**
 * <i><font size="-1" color="#FF0000">**For internal use only** </font></i>
 * A node based on BeanNode with some children. This node cannot be
 * copied, cut, destroyed or renamed.
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public class GPBeanNode extends BeanNode implements GPNode {

  /** a view of the children of this node */
  private NodeCollection nodeCollection;

  //
  // -- CONSTRUCTORS -----------------------------------------------
  //

  /**
   * Creates a new GPBeanNode based on the given bean with no children
   * The new node is therefore a leaf.
   */
  public GPBeanNode(Object bean) throws IntrospectionException {
    super(bean, Children.LEAF);
  }

  /**
   * Creates a new GPBeanNode based on the given bean and on the given children
   * @see cern.gp.nodes.impl.BeanNode#BeanNode(Object, Children)
   */
  public GPBeanNode(Object bean, ChildrenManager childrenManager) throws IntrospectionException {
    super(bean, ChildrenFactory.createChildren(childrenManager)); 
    this.nodeCollection = (NodeCollection) getChildren();
    if (nodeCollection instanceof Index) {
        super.getCookieSet().add((Cookie) nodeCollection);
    }
  }

  //
  // -- PUBLIC METHODS -----------------------------------------------
  //

  /** 
   * Get context help associated with this node.
   * @return the context help object (could be <code>null</code> or {@link HelpCtx#DEFAULT_HELP})
   */
  public HelpCtx getHelpCtx() {
    return super.getHelpCtx();
  }

  /** 
   * Can this node be renamed?
   * @return <code>false</code>
   */
  public boolean canRename() {
    return false;
  }

  /** 
   * Can this node be destroyed?
   * @return <CODE>false</CODE>
   */
  public boolean canDestroy() {
    return false;
  }

  /** 
   * Can this node be copied?
   * @return <code>false</code>
   */
  public boolean canCopy() {
    return false;
  }

  /** 
   * Can this node be cut?
   * @return <code>false</code>
   */
  public boolean canCut() {
    return false;
  }
  
  
  /** 
   * Get a serializable handle for this node.
   * Return null in this implementation which means the 
   * node is not persisted
   * @return <code>null</code>
   */
  public Node.Handle getHandle() {
    return null;
  }
  
  
  //
  // -- implements GPNode -------------------------------------------------
  //

  public NodeCollection getNodeCollection() {
    return nodeCollection;
  }
  
  public Node getPeerNode() {
    return this;
  }

  public GPNode getParent() {
    return (GPNode) super.getParentNode();
  }
  
  
  //
  // -- PROTECTED METHODS -----------------------------------------------
  //

   /** 
    * Initialize a default
    * property sheet; commonly overridden. If {@link #getSheet}
    * is called and there is not yet a sheet,
    * this method is called to allow a subclass
    * to specify its properties.
    * <P>
    * <em>Warning:</em> Do not call <code>getSheet</code> in this method.
    * <P>
    * The default implementation returns an empty sheet.
    *
    * @return the sheet with initialized values (never <code>null</code>)
    */
  protected Sheet createSheet() {
    Sheet sheet = super.createSheet();
    // Make sure there is a "Properties" set:
    Set props = sheet.get(Sheet.PROPERTIES); // get by name, not display name
    if (props == null) {
      props = Sheet.createPropertiesSet();
      sheet.put(props);
    }
    //props.put(new MyProp(someParams));
    return sheet;
  }

  //
  // -- PRIVATE METHODS -----------------------------------------------
  //

  //
  // -- INNER CLASSES -----------------------------------------------
  //
}
