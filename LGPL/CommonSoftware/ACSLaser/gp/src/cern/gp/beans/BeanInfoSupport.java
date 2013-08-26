/*
 * $Id: BeanInfoSupport.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.beans;

import java.beans.BeanDescriptor;
import java.beans.BeanInfo;
import java.beans.EventSetDescriptor;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.MethodDescriptor;
import java.beans.PropertyChangeListener;
import java.beans.PropertyDescriptor;
import java.beans.SimpleBeanInfo;

/**
 * Utility class that provides default implementation and caching for all aspect of
 * the BeanInfo.
 * <p>
 * Should be subclassed in order to overwrite the protected methods.
 * </p>
 *
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public class BeanInfoSupport extends SimpleBeanInfo {
  
  /** Empty array of property set that you can return in <code>getPropertyDescriptorsLazy</code>
   * in order not to have any property and to prevent the Introspector to introspect the 
   * properties from the bean */
  protected static final PropertyDescriptor[] EMPTY_PROPERTY_SET = new PropertyDescriptor[0];
  
  /** Empty array of method descriptors that you can return in <code>getMethodDescriptorsLazy</code>
   * in order not to have any method descriptor and to prevent the Introspector to introspect the 
   * methods from the bean */
  protected static final MethodDescriptor[] EMPTY_METHOD_SET = new MethodDescriptor[0];
  
  /** Empty array of event set descriptor that you can return in <code>getEventSetDescriptorsLazy</code>
   * in order not to have any event set and to prevent the Introspector to introspect the 
   * event set from the bean */
  protected static final EventSetDescriptor[] EMPTY_EVENT_SET = new EventSetDescriptor[0];
  
  
  private static final BeanInfo[] EMPTY_BEAN_INFO_SET = new BeanInfo[0];
  
  /** cached value if the icon */
  private java.awt.Image iconColor16;
  /** cached value if the icon */
  private java.awt.Image iconColor32;
  /** cached value if the icon */
  private java.awt.Image iconMono16;
  /** cached value if the icon */
  private java.awt.Image iconMono32;
  /** cached value if the bean descriptor */
  private BeanDescriptor beanDescriptor;
  /** cached value if the property descriptors */
  private PropertyDescriptor[] propertyDescriptors;
  /** cached value if the event set descriptors */
  private EventSetDescriptor[] eventSetDescriptors;
  /** cached value if the method descriptors */
  private MethodDescriptor[] methodDescriptors;
  /** cached additional beaninfos */
  private BeanInfo[] additionalBeanInfos;
  
  
  //
  // -- CONSTRUCTORS -----------------------------------------------
  //
  
  public BeanInfoSupport() {
    setActionsFromAdditionalBeanInfo();
  }
  
  
  
  //
  // -- PROTECTED METHODS -----------------------------------------------
  //
  
  
  /**
   * Returns the bean descriptor for this bean info.
   * @return the bean descriptor for this bean info.
   */
  protected BeanDescriptor getBeanDescriptorLazy() {
    return null;
  }
  
  
  /**
   * Returns the property descriptors for this bean info. Default
   * implementation returns null which means that the Introspector
   * will perform the introspection to get the PropertyDescriptors
   * This method is guarantee to be called once
   * @return the property descriptors for this bean info.
   */
  protected PropertyDescriptor[] getPropertyDescriptorsLazy() throws IntrospectionException {
    return null;
  }
  
  /**
   * Returns the event set descriptors for this bean info. Default
   * implementation returns null which means that the Introspector
   * will perform the introspection to get the EventSetDescriptors
   * This method is guarantee to be called once
   * @return the event set descriptors for this bean info.
   */
  protected EventSetDescriptor[] getEventSetDescriptorsLazy() throws IntrospectionException {
    return null;
  }
  
  /**
   * Returns the method descriptors for this bean info. Default
   * implementation returns null which means that the Introspector
   * will perform the introspection to get the MethodDescriptors.
   * This method is guarantee to be called once
   * @return the method descriptors for this bean info.
   */
  protected MethodDescriptor[] getMethodDescriptorsLazy() throws IntrospectionException {
    return null;
  }
  
  /**
   * Returns the name of the icon M16. Default
   * implementation returns null.
   * @return the name of the icon M16 or null if no icon.
   */
  protected String getIconNameM16() { return null; }
  
  /**
   * Returns the name of the icon M32. Default
   * implementation returns null.
   * @return the name of the icon M32 or null if no icon.
   */
  protected String getIconNameM32() { return null; }
  
  /**
   * Returns the name of the icon C16. Default
   * implementation returns null.
   * @return the name of the icon C16 or null if no icon.
   */
  protected String getIconNameC16() { return null; }
  
  /**
   * Returns the name of the icon C32. Default
   * implementation returns null.
   * @return the name of the icon C32 or null if no icon.
   */
  protected String getIconNameC32() { return null; }
  
  /**
   * Returns the index of the default property. Default
   * implementation returns -1.
   * @return the index of the default property.
   */
  protected int getDefaultPropertyIndexLazy() { return -1; }
  
  /**
   * Returns the index of the event. Default
   * implementation returns -1.
   * @return the index of the default event.
   */
  protected int getDefaultEventIndexLazy() { return -1; }
  
  /**
   * Returns the superclass of the class this bean info is for. This
   * is used in order to look for additional BeanInfo. Default
   * implementation returns null which means that there is no additional
   * BeanInfo
   * @return the superclass of the class this bean info is for or null.
   */
  protected Class getSuperclass() { return null; }
  
  
  /**
   * Returns a new <code>EventSetDescriptor</code> for PropertyChangeEvent.
   * Call this method inside the method <code>getEventSetDescriptorsLazy</code>
   * in order to include PropertyChangeEvent in the array of  <code>EventSetDescriptor</code>
   * you return. You do that if the bean class has the methods <code>addPropertyChangeListener</code>
   * and <code>removePropertyChangeListener</code>.
   * @param <code>beanClass</code> the class of the bean this BeanInfo is for
   * @return A EventSetDescriptor for the PropertyChange events. 
   * @exception IntrospectionException if the EventSetDescriptor cannot be built. 
   */
  protected EventSetDescriptor createPropertyChangeEventSet(Class beanClass) throws IntrospectionException {
    return new EventSetDescriptor(beanClass, "PropertyChangeEvent", PropertyChangeListener.class, "propertyChange");
  }
  
  //
  // -- PUBLIC STATIC METHODS -----------------------------------------------
  //
  
  //
  // -- PUBLIC METHODS -----------------------------------------------
  //
  
  public final BeanInfo[] getAdditionalBeanInfo() {
    if (additionalBeanInfos == null) {
      Class superclass = getSuperclass();
      if (superclass == null) {
        additionalBeanInfos = EMPTY_BEAN_INFO_SET;
      } else {
        try {
          additionalBeanInfos = new BeanInfo[] { Introspector.getBeanInfo(superclass) };
        } catch (IntrospectionException ex) {
          additionalBeanInfos = EMPTY_BEAN_INFO_SET;
        }
      }
    }
    return additionalBeanInfos;
  }
  
  
  /**
   * Gets the bean's <code>BeanDescriptor</code>s.
   *
   * @return BeanDescriptor describing the editable
   * properties of this bean.  May return null if the
   * information should be obtained by automatic analysis.
   */
  public final BeanDescriptor getBeanDescriptor() {
    if (beanDescriptor == null) {
      beanDescriptor = getBeanDescriptorLazy();
    }
    return beanDescriptor;
  }
  
  /**
   * Gets the bean's <code>PropertyDescriptor</code>s.
   *
   * @return An array of PropertyDescriptors describing the editable
   * properties supported by this bean.  May return null if the
   * information should be obtained by automatic analysis.
   * <p>
   * If a property is indexed, then its entry in the result array will
   * belong to the IndexedPropertyDescriptor subclass of PropertyDescriptor.
   * A client of getPropertyDescriptors can use "instanceof" to check
   * if a given PropertyDescriptor is an IndexedPropertyDescriptor.
   */
  public final PropertyDescriptor[] getPropertyDescriptors() {
    if (propertyDescriptors == null) {
      try {
        propertyDescriptors = getPropertyDescriptorsLazy();
      } catch (IntrospectionException e) {
        propertyDescriptors = EMPTY_PROPERTY_SET;
      }
    }
    return propertyDescriptors;
  }
  
  /**
   * Gets the bean's <code>EventSetDescriptor</code>s.
   *
   * @return  An array of EventSetDescriptors describing the kinds of
   * events fired by this bean.  May return null if the information
   * should be obtained by automatic analysis.
   */
  public final EventSetDescriptor[] getEventSetDescriptors() {
    if (eventSetDescriptors == null) {
      try {
        eventSetDescriptors = getEventSetDescriptorsLazy();
      } catch (IntrospectionException e) {
        eventSetDescriptors = EMPTY_EVENT_SET;
      }
    }
    return eventSetDescriptors;
  }
  
  /**
   * Gets the bean's <code>MethodDescriptor</code>s.
   *
   * @return  An array of MethodDescriptors describing the methods
   * implemented by this bean.  May return null if the information
   * should be obtained by automatic analysis.
   */
  public final MethodDescriptor[] getMethodDescriptors() {
    if (methodDescriptors == null) {
      try {
        methodDescriptors = getMethodDescriptorsLazy();
      } catch (IntrospectionException e) {
        methodDescriptors = EMPTY_METHOD_SET;
      }
    }
    return methodDescriptors;
  }
  
  /**
   * A bean may have a "default" property that is the property that will
   * mostly commonly be initially chosen for update by human's who are
   * customizing the bean.
   * @return  Index of default property in the PropertyDescriptor array
   *    returned by getPropertyDescriptors.
   * <P>  Returns -1 if there is no default property.
   */
  public final int getDefaultPropertyIndex() {
    return getDefaultPropertyIndexLazy();
  }
  
  /**
   * A bean may have a "default" event that is the event that will
   * mostly commonly be used by human's when using the bean.
   * @return Index of default event in the EventSetDescriptor array
   *    returned by getEventSetDescriptors.
   * <P>  Returns -1 if there is no default event.
   */
  public final int getDefaultEventIndex() {
    return getDefaultEventIndexLazy();
  }
  
  /**
   * This method returns an image object that can be used to
   * represent the bean in toolboxes, toolbars, etc.   Icon images
   * will typically be GIFs, but may in future include other formats.
   * <p>
   * Beans aren't required to provide icons and may return null from
   * this method.
   * <p>
   * There are four possible flavors of icons (16x16 color,
   * 32x32 color, 16x16 mono, 32x32 mono).  If a bean choses to only
   * support a single icon we recommend supporting 16x16 color.
   * <p>
   * We recommend that icons have a "transparent" background
   * so they can be rendered onto an existing background.
   *
   * @param  iconKind  The kind of icon requested.  This should be
   *    one of the constant values ICON_COLOR_16x16, ICON_COLOR_32x32,
   *    ICON_MONO_16x16, or ICON_MONO_32x32.
   * @return  An image object representing the requested icon.  May
   *    return null if no suitable icon is available.
   */
  public final java.awt.Image getIcon(int iconKind) {
    switch (iconKind) {
      case ICON_COLOR_16x16 :
        if (iconColor16 == null) {
          if (getIconNameC16() == null) return null;
          iconColor16 = loadImage(getIconNameC16());
          return iconColor16;
        }
        return iconColor16;
        
      case ICON_COLOR_32x32 :
        if (iconColor32 == null) {
          if (getIconNameC32() == null) return null;
          iconColor32 = loadImage(getIconNameC32());
          return iconColor32;
        }
        return iconColor32;
        
      case ICON_MONO_16x16 :
        if (iconMono16 == null) {
          if (getIconNameM16() == null) return null;
          iconMono16 = loadImage(getIconNameM16());
          return iconMono16;
        }
        return iconMono16;
        
      case ICON_MONO_32x32 :
        if (iconMono32 == null) {
          if (getIconNameM32() == null) return null;
          iconMono32 = loadImage(getIconNameM32());
          return iconMono32;
        }
        return iconMono32;
        
      default :
        return null;
    }
  }
  
  
  
  //
  // -- PRIVATE METHODS -----------------------------------------------
  //
  
  /**
   * Sets the qname of available actions for this BeanInfo by getting
   * the ones of all additional BeanInfos.
   */
  private void setActionsFromAdditionalBeanInfo() {
    // A BeanInfo may reference additional BeanInfos that may
    // defines action. Problem is that once the BeanInfo is reconstructed by the
    // Intropector it will loose track of the additional BeanInfos. This is why we have
    // to copy locally here the actions defined for additional BeanInfos.
    BeanInfo[] infos = getAdditionalBeanInfo();
    if (infos == null || infos.length == 0) return;
    for (int i = 0; i < infos.length; i++) {
      String[] actions = BeanTagger.getActions(infos[i]);
      if (actions != null) BeanTagger.addActions(this, actions);
      String defaultAction = BeanTagger.getDefaultAction(infos[i]);
      if (defaultAction != null) BeanTagger.setDefaultAction(this, defaultAction);
    }
  }
    
}
