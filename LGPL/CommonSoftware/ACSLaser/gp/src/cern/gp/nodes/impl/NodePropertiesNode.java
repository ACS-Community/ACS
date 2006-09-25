/*
 * $Id: NodePropertiesNode.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.nodes.impl;

import java.awt.Component;
import java.util.ArrayList;

import org.openide.nodes.AbstractNode;
import org.openide.nodes.Children;
import org.openide.nodes.Node;
import org.openide.util.actions.SystemAction;

/** 
 * <i><font size="-1" color="#FF0000">**For internal use only** </font></i>
 * A node that represents a Node and has all the properties of this node
 * as child nodes.
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public class NodePropertiesNode extends AbstractNode {

  private Node proxiedNode;
  
  //
  // -- CONSTRUCTORS -----------------------------------------------
  //
  
  public NodePropertiesNode(Node node) {
    super(computeChildren(node));
    proxiedNode = node;
  }
  
  
  
  //
  // -- PUBLIC METHODS -----------------------------------------------
  //
  
  public boolean canCopy() { return proxiedNode.canCopy(); }
  public boolean canCut() {  return proxiedNode.canCut(); }
  public boolean canRename() { return proxiedNode.canRename(); }
  public boolean canDestroy() { return proxiedNode.canDestroy(); }
  public Component getCustomizer() { return proxiedNode.getCustomizer(); }
  public boolean hasCustomizer() { return proxiedNode.hasCustomizer(); }
  public PropertySet[] getPropertySets() { return proxiedNode.getPropertySets(); }  
  public String getDisplayName() { return proxiedNode.getDisplayName(); }
  public String getName() { return proxiedNode.getName(); }
  public String getShortDescription() { return proxiedNode.getShortDescription(); }
  
  public java.awt.Image getIcon(int type) {
    return proxiedNode.getIcon(type); 
  }
  
  public java.awt.Image getOpenedIcon(int type) {
    return proxiedNode.getOpenedIcon(type); 
  }

  //
  // -- PROTECTED METHODS -----------------------------------------------
  //
  
  protected SystemAction[] createActions() {
    return new SystemAction[] {};
  }
  
  
  //
  // -- PRIVATE METHODS -----------------------------------------------
  //
  
  private static Children computeChildren(Node node) {
    PropertySet[] sets = node.getPropertySets();
    if (sets == null || sets.length == 0) return Children.LEAF;
    return new PropertyChildren(sets);
  }
  
  //
  // -- INNER CLASSES -----------------------------------------------
  //
  
  private static class PropertyChildren extends Children.Array {
    
    private PropertySet[] sets;

    public PropertyChildren(PropertySet[] sets) {
      super(new ArrayList());
      this.sets = sets;
    }
    
    protected void addNotify() {
      super.addNotify();
      for (int i=0; i<sets.length; i++) {
        addPropertyNodes(sets[i].getProperties());
      }
    }
    
    protected void removeNotify() {
      nodes.clear();
      super.removeNotify();
    }
    
    private void addPropertyNodes(Property[] properties) {
      if (properties == null || properties.length == 0) return;
      Node[] nodesToAdd = new Node[properties.length];
      for (int i=0; i<properties.length; i++) {
        nodesToAdd[i] = new PropertyNode(properties[i]);
      }
      add(nodesToAdd);
    }
    
  } // end inner class PropertyChildren

}
