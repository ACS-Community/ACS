/*
 * $Id: GPBean.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.beans;

/**
 * This interface defines the additional information a bean may provide in order to interact with the node that
 * represents it. 
 * <p>
 * A node that is built based on a given bean will use introspection to find those methods. The bean can implements
 * any of them and possibly none of them. The node will use whatever information is available to initialize itself. The
 * information non available through those methods will be taken from the BeanInfo or defaulted to a given value.
 * </p><p>
 * A bean that aimed to be used in GP Layer can choose one of the following strategies :
 * <ul>
 * <li>inherits from <code>BeanSupport</code> and override any of those methods to return an appropriated value. The
 * methods non overriden are implemented in <code>BeanSupport</code> and return null</li>
 * <li>implements directly the methods defined here without inheriting from <code>BeanSupport</code>. Since the
 * methods are found by introspection it is not strictly necessary to implement this interface. Nevertheless the
 * interface brings a compile time check that can prove to be useful. It the bean implements the interface it needs to
 * implement all methods are can return null for those where it doesn't want to specify a value.
 * </li>
 * <li>as above, implements directly 0 or more methods defined here without inheriting from <code>BeanSupport</code> and
 * provides an explicit <code>BeanInfo</code> for the specifying additional information.
 * </li>
 * </ul>
 * </p>
 * 
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public interface GPBean {

  /** the class property name */
  public static final String CLASS_PROPERTY_NAME = "class";

  /** the name property name */
  public static final String NAME_PROPERTY_NAME = "name";

  /** the display name property name */
  public static final String DISPLAYNAME_PROPERTY_NAME = "displayName";

  /** the short description property name */
  public static final String SHORTDESCRIPTION_PROPERTY_NAME = "shortDescription";
  
  /** the node icon property name */
  public static final String NODE_ICON_PROPERTY_NAME = "nodeIcon";

  /** the node default action property name */
  public static final String NODE_DEFAULT_ACTION_PROPERTY_NAME = "nodeDefaultAction";

  /** the node actions property name */
  public static final String NODE_ACTIONS_PROPERTY_NAME = "nodeActions";

  /** the node property cacheable property name */
  public static final String NODE_PROPERTIES_CACHEABLE_PROPERTY_NAME = "nodePropertiesCacheable";

  /** the property info property name */
  public static final String PROPERTY_INFO_PROPERTY_NAME = "propertyInfo";

  /** the display name of the name property */
  public static final String NAME_PROPERTY_DISPLAY_NAME = "Name";

  /** the display name of the display name property */
  public static final String DISPLAYNAME_PROPERTY_DISPLAY_NAME = "Display name";

  /** the display name of the short description property */
  public static final String SHORTDESCRIPTION_PROPERTY_DISPLAY_NAME = "Short description";
  
  /**
   * Returns the internal name of the associated bean. If null is returned a default name will be used.
   * <ul>
   * <li>Property name : <code>name</code></li>
   * <li>Visibility : <code>normal (visible in a property sheet)</code></li>
   * <li>Default value (when null is returned from this method) : <code>a unique name generated using BeanUtils.
   * generateUniqueBeanName()
   * </code></li>
   * </ul>
   * @return the internal name of the associated bean or null.
   */
  public String getName();

  /**
   * Returns the internal display name of the associated bean. If null is returned a default name will be used.
   * <ul>
   * <li>Property name : <code>displayName</code></li>
   * <li>Visibility : <code>normal (visible in a property sheet)</code></li>
   * <li>Default value (when null is returned from this method) : <code>if getName() is available it is used instead of
   * getDisplayName(), else beanInfo.getBeanDescriptor().getDisplayName() is used</code></li>
   * </ul>
   * @return the internal display name of the associated bean or null
   */
  public String getDisplayName();

  /**
   * Returns a short description of the associated bean (used in tooltip for instance). If null is returned a default
   * description will be used.
   * <ul>
   * <li>Property name : <code>shortDescription</code></li>
   * <li>Visibility : <code>normal (visible in a property sheet)</code></li>
   * <li>Default value (when null is returned from this method) : <code>beanInfo.getBeanDescriptor().
   * getShortDescription()</code></li>
   * </ul>
   * @return a short description of the associated bean or null
   */
  public String getShortDescription();

  /**
   * Returns the icon in 16x16 size used to display the node representing the associated bean. If null is returned a
   * default icon will be used.
   * <ul>
   * <li>Property name : <code>nodeIcon</code></li>
   * <li>Visibility : <code>hidden (not visible in a property sheet)</code></li>
   * <li>Default value (when null is returned from this method) : <code>beanInfo.getIcon()</code></li>
   * </ul>
   * @return the icon in 16x16 size used to display the node representing the associated bean or null
   */
  public java.awt.Image getNodeIcon();

  /**
   * Returns the qualified name of the default action for the node representing the associated bean. 
   * The default action is the one triggered when the user double-clic on the node.  If null is returned a default
   * default action will be used.
   * <ul>
   * <li>Property name : <code>nodeDefaultAction</code></li>
   * <li>Visibility : <code>hidden</code></li>
   * <li>Default value (when null is returned from this method) : <code>use the possible tagging of the BeanDescriptor
   * of the BeanInfo : BeanTagger.getDefaultAction(beanInfo.getBeanDescriptor())</code></li>
   * </ul>
   * @return the qualified name of the default action for the node or null representing the associated bean.
   */
  public String getNodeDefaultAction();

  /**
   * Returns an array of qualified name of actions for the node representing the associated bean.
   * Null can be used in the array to represent a separator.
   * <ul>
   * <li>Property name : <code>nodeActions</code></li>
   * <li>Visibility : <code>hidden (not visible in a property sheet)</code></li>
   * <li>Default value (when null is returned from this method) : <code>use the possible tagging of the BeanDescriptor
   * of the BeanInfo : BeanTagger.getActions(beanInfo.getBeanDescriptor())</code></li>
   * </ul>
   * @return an array of qualified name of actions for a node representing the bean.
   */
  public String[] getNodeActions();


  /**
   * Returns true if the value of the properties can be cached in the node, false if they cannot or null if no behavior
   * is specified. The default behavior is not to cache the value of the properties.
   * <p>
   * If the value of a property is not cached, the getter method will be invoked
   * whenever the node need to get the value of the property.
   * </p><p>
   * If the value of the property is cached, the getter method for the property will only be invoked
   * once to read the value. Then, whenever the node need the value of the property it will used the cached 
   * one until a <code>PropertyChangeEvent</code> for the property (or for all properties) is fired.
   * </p><p>
   * Caching values can improve performance greatly but it adds one more responsibility on the developer
   * who has to fire <code>PropertyChangeEvent</code> whenever the property values change.
   * </p><p>
   * This property gives the caching strategy for all properties. It is possible to override the bean level caching
   * strategy at the property level by using the property information <code>getPropertyInformation()</code>.
   * </p>
   * <ul>
   * <li>Property name : <code>nodePropertiesCacheable</code></li>
   * <li>Visibility : <code>hidden (not visible in a property sheet)</code></li>
   * <li>Default value (when null is returned from this method) : <code>use the possible tagging of the BeanDescriptor
   * of the BeanInfo : BeanTagger.isCacheable(beanInfo.getBeanDescriptor())</code></li>
   * </ul>
   * @return true is the value of the properties can be cached in the node, false else or null for default behavior
   */
  public Boolean getNodePropertiesCacheable();
  

  /**
   * Returns an array of <code>PropertyInfo</code> specifying information on some properties. 
   * <p>
   * Each <code>PropertyInfo</code> of this array defines information for one property of this bean. Defining a
   * <code>PropertyInfo</code> for a given property is usefull when there is no explicit <code>BeanInfo</code> for the
   * bean and when the property have non default characteristics such as, for instance, a specific
   * <code>PropertyEditor</code>.
   * </p>
   * <ul>
   * <li>Property name : <code>propertyInfo</code></li>
   * <li>Visibility : <code>hidden (not visible in a property sheet)</code></li>
   * <li>Default value (when null is returned from this method) : <code>use PropertyDescriptors from
   * BeanInfo</code></li>
   * </ul>
   * @return true is the value of the properties can be cached in the node, false else or null for default behavior
   */
  public PropertyInfo[] getPropertyInfo();

}
