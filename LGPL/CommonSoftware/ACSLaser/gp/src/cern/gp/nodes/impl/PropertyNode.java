/*
 * $Id: PropertyNode.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.nodes.impl;

import java.awt.Component;
import java.awt.Image;
import java.awt.Toolkit;
import java.beans.BeanInfo;
import java.beans.Introspector;
import java.beans.PropertyEditor;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.util.ArrayList;

import org.openide.nodes.AbstractNode;
import org.openide.nodes.BeanNode;
import org.openide.nodes.Children;
import org.openide.nodes.Node;
import org.openide.nodes.BeanNode.Descriptor;
import org.openide.util.actions.SystemAction;

import cern.gp.beans.BeanTagger;



/** 
 * <i><font size="-1" color="#FF0000">**For internal use only** </font></i>
 * A node that represents a property of a node. This node can be used to 
 * expand a node that represent a bean with some properties to show those
 * properties. Each child represent one single property and has one 
 * property called "value" that represent the value of the property itself.
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public class PropertyNode extends AbstractNode {
  
  private static final String ICON_NAME = "cern/gp/nodes/resources/properties.gif";
  private static final Image ICON_IMAGE = loadImage(ICON_NAME);
  public static final String VALUE = "value";
  
  private Property property;
  private PropertySet[] propertySets;
  
  //
  // -- CONSTRUCTORS -----------------------------------------------
  //
  
  public PropertyNode(Property property) {
    super(computeChildren(property));
    this.property = property;
    this.propertySets = new PropertySet[] { new ValuePropertySet(property) };
    setIconBase(ICON_NAME);
  }
  
  
  
  //
  // -- PUBLIC METHODS -----------------------------------------------
  //
  
  public boolean canCopy() { return false; }
  public boolean canCut() {  return false; }
  public boolean canRename() { return false; }
  public boolean canDestroy() { return false; }
  public String getDisplayName() { return property.getDisplayName(); }
  public String getName() { return property.getName(); }
  public String getShortDescription() { return property.getShortDescription(); }
  
  public Image getIcon(int type) {
    if (ICON_IMAGE == null) return super.getIcon(type);
    return ICON_IMAGE;
  }

  public Image getOpenedIcon(int type) {
    if (ICON_IMAGE == null) return super.getOpenedIcon(type);
    return ICON_IMAGE;
  }

  public Component getCustomizer() {
    PropertyEditor propertyEditor = property.getPropertyEditor();
    if (propertyEditor == null) return null;
    return propertyEditor.getCustomEditor();
  }
  
  public boolean hasCustomizer() {
    PropertyEditor propertyEditor = property.getPropertyEditor();
    if (propertyEditor == null) return false;
    return propertyEditor.supportsCustomEditor();
  }
  
  
  public PropertySet[] getPropertySets() {
    return propertySets;
  }
  
  
  public Object getValue(String attributeName) {
    if (VALUE.equals(attributeName)) {
      return property;
    } else {
      return super.getValue(attributeName);
    }
  }
  
  //
  // -- PROTECTED METHODS -----------------------------------------------
  //
  
  /**
   * Returns the actions that shall be displayed in the pop-up menu for this node.
   * This method is called by Netbeans Explorer to build the pop-up menu for this node.
   */
  protected SystemAction[] createActions() {
    return new SystemAction[] {};
  }
  
  
  //
  // -- PRIVATE METHODS -----------------------------------------------
  //
  
  private static Children computeChildren(Property property) {
    try {
      Object value = property.getValue();
      if (value == null) return Children.LEAF;
      Class clazz = value.getClass();
      Class arrayClass = clazz.getComponentType();
      if (arrayClass != null) {
        // it is an array
        return new PropertyArrayChildren(property);
      }
      // not an array
      // test if the property declares itself as expandable
      if (BeanTagger.isExpandable(property)) {
        return new PropertyChildren(property);
      }
      // the property is not marked as hierarchical => look for editor
      if (property.getPropertyEditor() != null) {
        return Children.LEAF;
      }
      // here depending what we want we can decide that property without a PropertyEditor
      // are expandable or not
      return Children.LEAF;
      //return new PropertyChildren(property);
    } catch (Exception e) {
      return Children.LEAF;
    }
  }
  
  
  private static Image loadImage(String pathname) {
    URL url = PropertyNode.class.getClassLoader().getResource(pathname);
    if (url != null)
      return Toolkit.getDefaultToolkit().createImage(url);
    return null;
  }
  
  //
  // -- INNER CLASSES -----------------------------------------------
  //
  
  /**
   * Children for a PropertyNode that represents a value that is 
   * a bean with properties. The children are the nodes representing
   * those properties.
   */
  private static class PropertyChildren extends Children.Array {
    
    private Property property;

    public PropertyChildren(Property property) {
      super(new ArrayList());
      this.property = property;
    }
    
    protected void addNotify() {
      super.addNotify();
      try {
        BeanInfo beanInfo = Introspector.getBeanInfo(property.getValueType());
        Descriptor descriptor = BeanNode.computeProperties(property.getValue(), beanInfo);
        addPropertyNodes(descriptor.property);
        addPropertyNodes(descriptor.expert);
      } catch (Exception e) {
        e.printStackTrace();
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
  
  
  /**
   * Children for a PropertyNode that represents an array of values
   */
  private static class PropertyArrayChildren extends Children.Array {
    
    private Property property;
    private Object[] values;
    
    public PropertyArrayChildren(Property property) throws java.lang.IllegalAccessException, InvocationTargetException {
      super(new ArrayList(((Object[]) property.getValue()).length));
      this.property = property;
      this.values = (Object[]) property.getValue();
    }
    
    protected void addNotify() {
      super.addNotify();
      Node[] nodesToAdd = new Node[values.length];
      for (int i=0; i<values.length; i++) {
        nodesToAdd[i] = new PropertyNode(new ValuePropertyProxy(property, values[i], i));
      }
      add(nodesToAdd);
    }
    
    protected void removeNotify() {
      nodes.clear();
      super.removeNotify();
    }
    
  } // end inner class PropertyArrayChildren
  

  
  /**
   * A simple PropertySet that contains only a single property
   * called value and whose value is the property represented 
   * by the node.
   */
  private static class ValuePropertySet extends PropertySet {
    
    private Property[] properties;
    
    public ValuePropertySet(Property property) {
      super("ValueSet", "ValueSet", "ValueSet");
      properties = new Property[] { property };
    }
    
    public Property[] getProperties() {
      return properties;
    }
  }
  
  
  /**
   * Proxy on a Node.Property that represents an array. The proxy acts as 
   * if it was a property and represents one value of the array of values
   * the proxied property is equal to.
   */
  private static class ValuePropertyProxy extends Property {
    
    private Property proxiedProperty;
    private Object value;
    private String indexString;
    
    public ValuePropertyProxy(Property proxiedProperty, Object value, int index) {
      super(proxiedProperty.getValueType());
      this.proxiedProperty = proxiedProperty;
      this.value = value;
      this.indexString = "["+index+"]";
    }
    
    public Object getValue() throws IllegalAccessException, InvocationTargetException {
      return value;
    }
    
    public void setValue(Object value) throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
      throw new IllegalAccessException();
    }
    
    public boolean supportsDefaultValue() {
      return proxiedProperty.supportsDefaultValue();
    }
    
    public void restoreDefaultValue() throws IllegalAccessException, InvocationTargetException {
      proxiedProperty.restoreDefaultValue();
    }
    
    public PropertyEditor getPropertyEditor() {
      PropertyEditor editor = proxiedProperty.getPropertyEditor();
      return editor;
    }
       
    public boolean canRead() { return true; }
    public boolean canWrite() { return false; }
    
    public String getDisplayName() {
      String s = proxiedProperty.getDisplayName();
      if (s == null)
        return "null "+indexString;
      else return s+indexString;
    }
    
    public String getName() {
      String s = proxiedProperty.getName();
      if (s == null)
        return "null "+indexString;
      else return s+indexString;
    }
    
    public String getShortDescription() { 
      return proxiedProperty.getShortDescription(); 
    }
     
  }
}
