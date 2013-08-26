/*
 * $Id: BeanSupport.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.beans;

import java.awt.Image;
import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;

/**
 * This support class should be subclassed by bean classes. It facilitates the writing of JavaBean classes that are
 * friendly with a node representing them by providing the extra information needed. The information provided by the
 * bean can be static or dynamic using PropertyChangeEvents.
 * <p>
 * This support class provides the necessary methods to register a listener and provides methods to send
 * PropertyChangeEvents. The bean can use the protected methods to fire events each time a property changes.
 * </p><p>
 * In order to be represented by a node, the bean can provide extra information that is not directly related to the
 * business of the bean but that is nevertheless interesting to control the node. The bean can provide that information
 * through its <code>BeanInfo</code> or through specific getters (defined in the interface <code>GPNode</code>). The
 * information provided can be static or dynamic. It is static if the value is just given once when the node is created,
 * it is dynamic if the bean allow the node to register itself as <code>PropertyChangeListener</code> and fire events
 * when those properties changes.
 * </p><p>
 * The list of properties recognized by the node and associated getters is given by the <code>GPBean</code> interface.
 * This support class implements all the getter methods in <code>GPNode</code> by returning a null value. You should
 * override each method you want to provide information for. For each property that can be dynamically updated there is
 * a protected method in this class you can use to fire an event to update the value. Here is the list :
 * </p>
 * <table border="1">
 * <tr>
 * <td><b>property name</b></td>
 * <td><b>method name</b></td>
 * <td><b>method to fire related event</b></td>
 * </tr>
 * <tr>
 * <td>name</td>
 * <td><code>getName()</code></td>
 * <td><code>fireNamePropertyChange()</code></td>
 * </tr>
 * <tr>
 * <td>displayName</td>
 * <td><code>getDisplayName()</code></td>
 * <td><code>fireDisplayNamePropertyChange()</code></td>
 * </tr>
 * <tr>
 * <td>shortDescription</td>
 * <td><code>getShortDescription()</code></td>
 * <td><code>fireShortDescriptionPropertyChange()</code></td>
 * </tr>
 * <tr>
 * <td>nodeDefaultAction</td>
 * <td><code>getNodeDefaultAction()</code></td>
 * <td><code>fireNodeDefaultActionPropertyChange()</code></td>
 * </tr>
 * <tr>
 * <td>nodeIcon</td>
 * <td><code>getNodeIcon</code></td>
 * <td><code>fireNodeIconPropertyChange()</code></td>
 * </tr>
 * </table>
 * <p>
 * If the bean fire a given event, it should provide the getter matching the <code>PropertyChangeEvent</code> that 
 * it fires. For instance if the bean call the protected method <code>fireNamePropertyChange</code> when a 
 * name change occurs, it should have the matching getter <code>getName()</code> implemented and returning a non null
 * value.
 * </p><p>
 * When a given getter is not available in this bean, the corresponding information will be looked-up in the
 * <code>BeanInfo</code>. It will only be static, because the BeanInfo provides static information common to all beans.
 * There is only one BeanInfo for a given class of beans and therefore the information founds inside the BeanInfo is
 * shared by all beans of that class. If the <code>BeanInfo</code> does not contain the information, a standard default
 * value will be applied when possible.
 * </p>
 * 
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public class BeanSupport implements GPBean {
  
  private PropertyChangeSupport propertyChangeSupport;
  
  //
  // -- CONSTRUCTORS -----------------------------------------------
  //
  
  protected BeanSupport() {
  }
  
  
  
  //
  // -- PUBLIC METHODS -----------------------------------------------
  //
  
  /**
   * Add a PropertyChangeListener to the listener list. The listener is registered for all properties.
   * @param listener The PropertyChangeListener to be added
   */
  public void addPropertyChangeListener(PropertyChangeListener listener) {
    checkPropertyChangeSupport();
    propertyChangeSupport.addPropertyChangeListener(listener);
  }
  
  /**
   * Remove a PropertyChangeListener from the listener list. This removes a PropertyChangeListener 
   * that was registered for all properties.
   * @param listener The PropertyChangeListener to be removed
   */
  public void addPropertyChangeListener(String propertyName, PropertyChangeListener listener) {
    checkPropertyChangeSupport();
    propertyChangeSupport.addPropertyChangeListener(propertyName, listener);
  }
  
  /**
   * Add a PropertyChangeListener for a specific property. The listener will be invoked only 
   * when a call on firePropertyChange names that specific property.
   * @param propertyName The name of the property to listen on.
   * @param listener The PropertyChangeListener to be added
   */
  public void removePropertyChangeListener(PropertyChangeListener listener) {
    if (propertyChangeSupport == null) return;
    propertyChangeSupport.removePropertyChangeListener(listener);
  }

  /**
   * Remove a PropertyChangeListener for a specific property.
   * @param propertyName The name of the property that was listened on.
   * @param listener The PropertyChangeListener to be removed
   */
  public void removePropertyChangeListener(String propertyName, PropertyChangeListener listener) {
    if (propertyChangeSupport == null) return;
    propertyChangeSupport.removePropertyChangeListener(propertyName, listener);
  }
  
  
  //
  // -- implements GPBean -----------------------------------------------
  //
  
  /* (non-Javadoc)
   * @see cern.gp.beans.GPBean#getDisplayName()
   */
  public String getDisplayName() {
    return null;
  }

  /* (non-Javadoc)
   * @see cern.gp.beans.GPBean#getName()
   */
  public String getName() {
    return null;
  }

  /* (non-Javadoc)
   * @see cern.gp.beans.GPBean#getNodeActions()
   */
  public String[] getNodeActions() {
    return null;
  }

  /* (non-Javadoc)
   * @see cern.gp.beans.GPBean#getNodeDefaultAction()
   */
  public String getNodeDefaultAction() {
    return null;
  }

  /* (non-Javadoc)
   * @see cern.gp.beans.GPBean#getNodeIcon()
   */
  public Image getNodeIcon() {
    return null;
  }

  /* (non-Javadoc)
   * @see cern.gp.beans.GPBean#getNodePropertiesCacheable()
   */
  public Boolean getNodePropertiesCacheable() {
    return null;
  }

  /* (non-Javadoc)
   * @see cern.gp.beans.GPBean#getShortDescription()
   */
  public String getShortDescription() {
    return null;
  }
    
  /* (non-Javadoc)
   * @see cern.gp.beans.GPBean#getPropertyInfo()
   */
  public PropertyInfo[] getPropertyInfo() {
    return null;
  }
  
  //
  // -- PROTECTED METHODS -----------------------------------------------
  //
  
  /**
   * Reports a bound property update to any registered listeners. 
   * No event is fired if old and new are equal and non-null.
   *
   * You pass null for all 3 parameters, to tell the listeners to read <em>all</em>
   * properties again, using the accessor methods. This is useful if you want to 
   * force all properties of this bean to be refreshed in the GUI (e.g. refresh the
   * whole property sheet or a row in a ListTable at once).
   *
   * @param propertyName The programmatic name of the property that was changed.
   * @param oldValue The old value of the property.
   * @param newValue The new value of the property.
   */
  protected void firePropertyChange(String propertyName, Object oldValue, Object newValue) {
    if (propertyChangeSupport == null) return;
    propertyChangeSupport.firePropertyChange(propertyName, oldValue, newValue);
  }
  
  /**
   * Reports a Name property update to any registered listeners. 
   * @param newName The new value of the name.
   */
  protected void fireNamePropertyChange(String newName) {
    firePropertyChange(NAME_PROPERTY_NAME, null, newName); 
  }
  
  /**
   * Reports a DisplayName property update to any registered listeners. 
   * @param newDisplayName The new value of the DisplayName.
   */
  protected void fireDisplayNamePropertyChange(String newDisplayName) {
    firePropertyChange(DISPLAYNAME_PROPERTY_NAME, null, newDisplayName);
  }
  
  /**
   * Reports a ShortDescription property update to any registered listeners. 
   * @param newShortDescription The new value of the ShortDescription.
   */
  protected void fireShortDescriptionPropertyChange(String newShortDescription) {
    firePropertyChange(SHORTDESCRIPTION_PROPERTY_NAME, null, newShortDescription);
  }
  
  /**
   * Reports a icon property update to any registered listeners. 
   * @param newIcon The new value of the icon.
   * @see #getNodeIconFromPathname(String)
   */
  protected void fireNodeIconPropertyChange(java.awt.Image newIcon) {
    firePropertyChange(NODE_ICON_PROPERTY_NAME, null, newIcon);
  }
  
  /**
   * Reports a default action property update to any registered listeners. 
   * @param newDefaultAction The new value of the default action.
   */
  protected void fireNodeDefaultActionPropertyChange(String newDefaultAction) {
    firePropertyChange(NODE_DEFAULT_ACTION_PROPERTY_NAME, null, newDefaultAction);
  }
  
  /**
   * Returns the icon image from the pathname of the icon.
   * <p>
   * The pathname should be relative to the package of this bean and be contained in the classpath.
   * For instance if this bean is in <code>cern.gp.beans</code> and the icon is stored in
   * <code>cern/gp/beans/images/MyIcon.gif</code> the pathname to give would be 
   * <code>images/MyIcon.gif</code>.
   * </p>
   * @param newIconPathname The new value of the Icon pathname.
   * @return the image defined by the icon pathname or null if it cannot be found or loaded.
   */
  protected java.awt.Image getNodeIconFromPathname(String newIconPathname) {
    return BeanUtils.loadImage(newIconPathname, this.getClass());
  }
  
  
  /**
   * Merges the two <code>String</code> arrays into one. This method is useful when a class inherits from another that
   * already has a method <code>getNodeActions</code> that returned an array of <code>String</code>. If the subclass
   * wants to return a <code>String</code> array while taking into account the one from the parent class it can use this
   * method. For instance :
   * <pre>
   * String[] parentArray= super.getNodeActions(); 
   * String[] myArray = new String [] { .... }; 
   * return mergePropertyInfo(parentPI, myPI);
   * </pre>
   *
   * @param actions1 the first array of String to merge
   * @param actions2 the second array of String to merge
   * @return String[] the array resulting of the merging of the two given array
   */
  protected static String[] mergeNodeActions(String[] actions1, String[] actions2) {
    if (actions1 == null || actions1.length == 0) return actions2;
    if (actions2 == null || actions2.length == 0) return actions1;
    String[] result = new String[actions1.length + actions2.length];
    System.arraycopy(actions1, 0, result, 0, actions1.length);
    System.arraycopy(actions2, 0, result, actions1.length, actions2.length);
    return result;
  }


  /**
   * Merges the two <code>PropertyInfo</code> arrays into one. This method is useful when a class inherits from another
   * that already has a method <code>getPropertyInfo</code> that returned an array of <code>PropertyInfo</code>. If the
   * subclass wants to return <code>PropertyInfo</code> while taking into account the one from the parent class it can
   * use this method. For instance :
   * <pre>
   *   PropertyInfo[] parentPI = super.getPropertyInfo();
   *   PropertyInfo[] myPI = new PropertyInfo[] { .... };
   *   return mergePropertyInfo(parentPI, myPI);
   * </pre>
   * 
   * @param info1 the first array of PropertyInfo to merge
   * @param info2 the second array of PropertyInfo to merge
   * @return PropertyInfo[] the array resulting of the merging of the two given array
   */
  protected static PropertyInfo[] mergePropertyInfo(PropertyInfo[] info1, PropertyInfo[] info2) {
    if (info1 == null || info1.length == 0) return info2;
    if (info2 == null || info2.length == 0) return info1;
    PropertyInfo[] result = new PropertyInfo[info1.length + info2.length];
    System.arraycopy(info1, 0, result, 0, info1.length);
    System.arraycopy(info2, 0, result, info1.length, info2.length);
    return result;
  }
  
  
  /**
   * Lazily creates the PropertyChangeSupport
   */
  protected synchronized void checkPropertyChangeSupport() {
    if (propertyChangeSupport == null) {
      propertyChangeSupport = new PropertyChangeSupport(this);
    }
  }
  
  //
  // -- PRIVATE METHODS -----------------------------------------------
  //
  

}
