/*
 * $Id: IntrospectionBasedNodeUpdater.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.3 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.beans.impl;

import java.beans.BeanInfo;
import java.beans.EventSetDescriptor;
import java.beans.IntrospectionException;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import org.openide.util.Utilities;

import cern.gp.beans.BeanTagger;
import cern.gp.beans.BeanUtils;
import cern.gp.beans.GPBean;
import cern.gp.beans.PropertyInfo;
import cern.gp.util.GPManager;
import java.beans.PropertyDescriptor;

/**
 * <i><font size="-1" color="#FF0000">**For internal use only** </font></i>
 * Provides support for implementing a <code>NodeUpdater</code> based on introspection.
 * <p>
 * For beans that do not implement any interface we use the JavaBeans specification for doing the introspection of the
 * properties we are interested in. Those properties should have at least an associated getter. The list of properties
 * we look for is defined by the <code>GPBean</code> interface.
 * </p>
 * <p> This NodeUpdater also supports the <code>BeanInfo</code> tagging done through the <code>BeanTagger</code> class.
 * Tagging a <code>BeanInfo</code> allows to add information that is not directly supported by the
 * <code>BeanInfo</code>. Three tags are recognized by <code>BeanNode</code> :
 * <ul>
 * <li><code>BeanTagger.getActions()</code> to get the actions for this node. By default 
 * no action is added.</li>
 * <li><code>BeanTagger.getDefaultAction()</code> to get the default action for this node. 
 * By default the Property action is used.</li>
 * <li><code>BeanTagger.isCacheable()</code> to get whether the value of the properties of 
 * the bean can be cached or not.</li>
 * </ul>
 * </p>
 * 
 * @version $Revision: 1.3 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 *
 */
public abstract class IntrospectionBasedNodeUpdater implements NodeUpdater {

  //
  // -- STATIC VARIABLES -----------------------------------------------
  //

  private static final InternalPropertyDescriptor NAME_DESC =
    new InternalPropertyDescriptor(GPBean.NAME_PROPERTY_NAME, "getName", String.class);
  private static final InternalPropertyDescriptor DISPLAYNAME_DESC =
    new InternalPropertyDescriptor(GPBean.DISPLAYNAME_PROPERTY_NAME, "getDisplayName", String.class);
  private static final InternalPropertyDescriptor SHORTDESCRIPTION_DESC =
    new InternalPropertyDescriptor(GPBean.SHORTDESCRIPTION_PROPERTY_NAME, "getShortDescription", String.class);
  private static final InternalPropertyDescriptor NODE_ICON_DESC =
    new InternalPropertyDescriptor(GPBean.NODE_ICON_PROPERTY_NAME, "getNodeIcon", java.awt.Image.class);
  private static final InternalPropertyDescriptor NODE_DEFAULT_ACTION_DESC =
    new InternalPropertyDescriptor(GPBean.NODE_DEFAULT_ACTION_PROPERTY_NAME, "getNodeDefaultAction", String.class);
  private static final InternalPropertyDescriptor NODE_ACTIONS_DESC =
    new InternalPropertyDescriptor(GPBean.NODE_ACTIONS_PROPERTY_NAME, "getNodeActions", String[].class);
  private static final InternalPropertyDescriptor NODE_PROPERTIES_CACHEABLE_DESC =
    new InternalPropertyDescriptor(
      GPBean.NODE_PROPERTIES_CACHEABLE_PROPERTY_NAME,
      "getNodePropertiesCacheable",
      Boolean.class);
  private static final InternalPropertyDescriptor PROPERTY_INFO_DESC =
    new InternalPropertyDescriptor(GPBean.PROPERTY_INFO_PROPERTY_NAME, "getPropertyInfo", PropertyInfo[].class);

  private static final Class[] EMPTY_CLASS_ARRAY = new Class[0];

  //
  // -- MEMBER VARIABLES -----------------------------------------------
  //

  private Method _nameGetter;
  private Method _displayNameGetter;
  private Method _shortDescriptionGetter;
  private Method _nodeIconGetter;
  private Method _nodeDefaultActionGetter;
  private Method _nodeActionsGetter;
  private Method _nodePropertiesCacheableGetter;
  private Method _propertyInfoGetter;
  
  private boolean _shouldHideName;
  private boolean _shouldHideDisplayName;
  private boolean _shouldHideShortDescription;

  /** remove PropertyChangeListener method */
  private Method _removePropertyChangeListenerMethod;

  /** listener for properties */
  private PropertyChangeListener _propertyChangeListener;

  /** bean */
  private Object _bean;

  /** bean info */
  private BeanInfo _beanInfo;

  private boolean _hasRegisteredListener;

  //
  // -- CONSTRUCTORS -----------------------------------------------
  //

  /**
   * Creates a new <code>GuiUpdaterIntrospector</code>
   */
  protected IntrospectionBasedNodeUpdater(Object bean) throws IntrospectionException {
    Class targetClass = BeanUtils.findTargetClass(bean);
    initialize(bean, Utilities.getBeanInfo(targetClass), targetClass);
  }

  /**
   * Creates a new <code>GuiUpdaterIntrospector</code>
   */
  protected IntrospectionBasedNodeUpdater(Object bean, BeanInfo beanInfo) throws IntrospectionException {
    this(bean, beanInfo, beanInfo.getClass());
  }

  /**
   * Creates a new <code>GuiUpdaterIntrospector</code>
   */
  protected IntrospectionBasedNodeUpdater(Object bean, BeanInfo beanInfo, Class targetClass)
    throws IntrospectionException {
    initialize(bean, beanInfo, targetClass);
  }

  private final void initialize(Object bean, BeanInfo beanInfo, Class targetClass) {
    this._bean = bean;
    this._beanInfo = beanInfo;
    _nameGetter = findGetterMethod(targetClass, NAME_DESC);
    _shouldHideName = _nameGetter == null;
    _displayNameGetter = findGetterMethod(targetClass, DISPLAYNAME_DESC);
    if (_displayNameGetter == null) {
      // if displayNameGetter does not exist we want to use the nameGetter
      // if it exist to prevent the display of a static string comming from BeanInfo
      _displayNameGetter = _nameGetter;
      _shouldHideDisplayName = true;
    } else {
      // if it exists we check that is does not return a null value. If it does return a null value we use
      // the getName instead
      if (getStringFromGetterMethod(_displayNameGetter) == null) {
        _displayNameGetter = _nameGetter;
        _shouldHideDisplayName = true;
      }
    }
    _shortDescriptionGetter = findGetterMethod(targetClass, SHORTDESCRIPTION_DESC);
    _shouldHideShortDescription = _shortDescriptionGetter == null || getStringFromGetterMethod(_shortDescriptionGetter) == null;
    _nodeIconGetter = findGetterMethod(targetClass, NODE_ICON_DESC);
    _nodeDefaultActionGetter = findGetterMethod(targetClass, NODE_DEFAULT_ACTION_DESC);
    _nodeActionsGetter = findGetterMethod(targetClass, NODE_ACTIONS_DESC);
    _nodePropertiesCacheableGetter = findGetterMethod(targetClass, NODE_PROPERTIES_CACHEABLE_DESC);
    _propertyInfoGetter = findGetterMethod(targetClass, PROPERTY_INFO_DESC);
    findPropertyChangeListenerMethods();
  }

  //
  // -- PUBLIC STATIC METHODS -----------------------------------------------
  //
  /**
   * Returns whether or not the property having the given <code>PropertyDescriptor</code> is a property 
   * supported by this introspector that should automatically be hidden.
   * In the case the property is supported and should not be hidden, the <code>PropertyDescriptor</code> will
   * be updated with the display name of the property.
   * @param propertyName the property name to check
   * @return true if the property is automatically hidden false else
   */
  public boolean isPropertyHidden(PropertyDescriptor propertyDescriptor) {
    String propertyName = propertyDescriptor.getName();
    if (GPBean.NAME_PROPERTY_NAME.equals(propertyName)) {
      propertyDescriptor.setDisplayName(GPBean.NAME_PROPERTY_DISPLAY_NAME);
      return _shouldHideName;
    }
    if (GPBean.DISPLAYNAME_PROPERTY_NAME.equals(propertyName)) {
      propertyDescriptor.setDisplayName(GPBean.DISPLAYNAME_PROPERTY_DISPLAY_NAME);
      return _shouldHideDisplayName;
    }
    if (GPBean.SHORTDESCRIPTION_PROPERTY_NAME.equals(propertyName)) {
      propertyDescriptor.setDisplayName(GPBean.SHORTDESCRIPTION_PROPERTY_DISPLAY_NAME);
      return _shouldHideShortDescription;
    }
    if (GPBean.CLASS_PROPERTY_NAME.equals(propertyName))
      return true;
    if (GPBean.NODE_ICON_PROPERTY_NAME.equals(propertyName))
      return true;
    if (GPBean.NODE_DEFAULT_ACTION_PROPERTY_NAME.equals(propertyName))
      return true;
    if (GPBean.NODE_ACTIONS_PROPERTY_NAME.equals(propertyName))
      return true;
    if (GPBean.NODE_PROPERTIES_CACHEABLE_PROPERTY_NAME.equals(propertyName))
      return true;
    if (GPBean.PROPERTY_INFO_PROPERTY_NAME.equals(propertyName))
      return true;
    return false;
  }

  //
  // -- PUBLIC METHODS -----------------------------------------------
  //

  //
  // -- implements NodeUpdater -----------------------------------------------
  //

  public String getName() {
    if (_nameGetter != null) {
      String value = getStringFromGetterMethod(_nameGetter);
      if (value != null) return value;
    }
    return BeanUtils.generateUniqueBeanName(_bean);
  }

  public String getDisplayName() {
    if (_displayNameGetter != null) {
      String value = getStringFromGetterMethod(_displayNameGetter);
      if (value != null) return value;
    }
    return _beanInfo.getBeanDescriptor().getDisplayName();
  }

  public String getShortDescription() {
    if (_shortDescriptionGetter != null) {
      String value = getStringFromGetterMethod(_shortDescriptionGetter);
      if (value != null) return value;
    }
    return _beanInfo.getBeanDescriptor().getShortDescription();
  }

  public java.awt.Image getNodeIcon() {
    if (_nodeIconGetter != null) {
      java.awt.Image value = (java.awt.Image) getObjectFromGetterMethod(_nodeIconGetter);
      if (value != null) return value;
    }
    return _beanInfo.getIcon(BeanInfo.ICON_COLOR_16x16);
  }

  public String getNodeDefaultAction() {
    if (_nodeDefaultActionGetter != null) {
      String value = getStringFromGetterMethod(_nodeDefaultActionGetter);
      if (value != null) return value;
    }
    return BeanTagger.getDefaultAction(_beanInfo);
  }

  public String[] getNodeActions() {
    if (_nodeActionsGetter != null) {
      String[] value = (String[]) getObjectFromGetterMethod(_nodeActionsGetter);
      if (value != null) return value;
    }
    return BeanTagger.getActions(_beanInfo);
  }

  public Boolean getNodePropertiesCacheable() {
    if (_nodePropertiesCacheableGetter != null) {
      Boolean value = (Boolean) getObjectFromGetterMethod(_nodePropertiesCacheableGetter);
      if (value != null) return value;
    }
    return BeanTagger.isCacheable(_beanInfo.getBeanDescriptor());
  }

  /* (non-Javadoc)
   * @see cern.gp.beans.GPBean#getPropertyInfo()
   */
  public PropertyInfo[] getPropertyInfo() {
    if (_nodePropertiesCacheableGetter != null) {
      PropertyInfo[] value = (PropertyInfo[]) getObjectFromGetterMethod(_propertyInfoGetter);
      if (value != null) return value;
    }
    return null;
  }

  //
  // -- PROTECTED METHODS -----------------------------------------------
  //

  protected abstract void firePropertyChange(String propertyName, Object oldValue, Object newValue);

  protected abstract void fireNameChange(String newName);

  protected abstract void fireDisplayNameChange(String newDisplayName);

  protected abstract void fireShortDescriptionChange(String newShortDescription);

  protected abstract void fireNodeDefaultActionChange(String newDefaultAction);

  protected abstract void fireNodeIconChange(java.awt.Image newIcon);

  protected final void removePropertyChangeListener() {
    if (_removePropertyChangeListenerMethod != null) {
      try {
        _removePropertyChangeListenerMethod.invoke(_bean, new Object[] { _propertyChangeListener });
      } catch (Exception e) {
        // [PENDING] deal with the error to notify
      }
    }
  }

  protected final boolean hasRegisteredListener() {
    return _hasRegisteredListener;
  }

  //
  // -- PRIVATE METHODS -----------------------------------------------
  //

  private void findPropertyChangeListenerMethods() {
    // add propertyChangeListener
    EventSetDescriptor[] eventSetDescriptors = _beanInfo.getEventSetDescriptors();
    for (int i = 0; i < eventSetDescriptors.length; i++) {
      Method method = eventSetDescriptors[i].getAddListenerMethod();
      if (method != null
        && method.getName().equals("addPropertyChangeListener")
        && Modifier.isPublic(method.getModifiers())) {
        // Possible for a public class to extend a package-private class,
        // where the private class defines addPropertyChangeListener, in which
        // case the introspector lists an inaccessible method in the event
        // set descriptor. In such a case, do not try to add a listener.
        try {
          _propertyChangeListener = new PropertyChangeListenerImpl();
          method.invoke(_bean, new Object[] { _propertyChangeListener });
          _hasRegisteredListener = true;
        } catch (Exception e) {
          // Warning, not info: likely to call e.g. getters or other things used
          // during startup of the bean, so it is not good to swallow errors here
          // (e.g. SharedClassObject.initialize throws RuntimeException -> it is
          // caught here and probably someone wants to know).
          GPManager.notify(GPManager.ERROR, e);
          // [PENDING] deal with the error to notify
        }
        _removePropertyChangeListenerMethod = eventSetDescriptors[i].getRemoveListenerMethod();
        break;
      }
    }
  }

  private final String getStringFromGetterMethod(Method getterMethod) {
    try {
      return (String) getterMethod.invoke(_bean);
    } catch (Exception ex) {
      // [PENDING] notify of the problem
      return null;
    }
  }

  private final Object getObjectFromGetterMethod(Method getterMethod) {
    try {
      return getterMethod.invoke(_bean);
    } catch (Exception ex) {
      // [PENDING] notify of the problem
      return null;
    }
  }

  /**
   * Finds getter methods for the given getter name. The return type
   * of the getter must be of type String
   */
  private static Method findGetterMethod(Class beanClass, InternalPropertyDescriptor propertyDesc) {
    try {
      Method getter = beanClass.getMethod(propertyDesc.getterName, EMPTY_CLASS_ARRAY);
      if (getter.getReturnType() != propertyDesc.type)
        return null;
      return getter;
    } catch (Exception e) {
      return null;
    }
  }

  //
  // -- INNER CLASSES -----------------------------------------------
  //

  /**
   * Property change listener to update the properties of the node and
   * also the name of the node (sometimes)
   */
  private final class PropertyChangeListenerImpl extends Object implements PropertyChangeListener {
    public void propertyChange(PropertyChangeEvent e) {
      Object newValue = e.getNewValue();
      String propertyName = e.getPropertyName();
      firePropertyChange(propertyName, e.getOldValue(), newValue);
      if (propertyName == null) {
        fireNameChange(getName());
        fireDisplayNameChange(getDisplayName());
        fireShortDescriptionChange(getShortDescription());
        fireNodeDefaultActionChange(getNodeDefaultAction());
        fireNodeIconChange(getNodeIcon());

      } else {
        if (NAME_PROPERTY_NAME.equals(propertyName)) {
          if (newValue == null || (!(newValue instanceof String))) {
            fireNameChange(getName());
          } else {
            fireNameChange((String) newValue);
          }

        } else if (DISPLAYNAME_PROPERTY_NAME.equals(propertyName)) {
          if (newValue == null || (!(newValue instanceof String))) {
            fireDisplayNameChange(getDisplayName());
          } else {
            fireDisplayNameChange((String) newValue);
          }

        } else if (SHORTDESCRIPTION_PROPERTY_NAME.equals(propertyName)) {
          if (newValue == null || (!(newValue instanceof String))) {
            fireShortDescriptionChange(getShortDescription());
          } else {
            fireShortDescriptionChange((String) newValue);
          }

        } else if (NODE_DEFAULT_ACTION_PROPERTY_NAME.equals(propertyName)) {
          if (newValue == null || (!(newValue instanceof String))) {
            fireNodeDefaultActionChange(getNodeDefaultAction());
          } else {
            fireNodeDefaultActionChange((String) newValue);
          }

        } else if (NODE_ICON_PROPERTY_NAME.equals(propertyName)) {
          if (newValue == null || (!(newValue instanceof String[]))) {
            fireNodeIconChange(getNodeIcon());
          } else {
            fireNodeIconChange((java.awt.Image) newValue);
          }
        }
      }
    }
  }

  private final static class InternalPropertyDescriptor {
    String name;
    String getterName;
    Class type;
    InternalPropertyDescriptor(String name, String getterName, Class type) {
      this.name = name;
      this.getterName = getterName;
      this.type = type;
    }
  }

}
