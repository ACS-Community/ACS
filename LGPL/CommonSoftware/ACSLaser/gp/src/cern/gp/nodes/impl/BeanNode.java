/*
 * $Id: BeanNode.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.nodes.impl;

import java.beans.BeanInfo;
import java.beans.Customizer;
import java.beans.IndexedPropertyDescriptor;
import java.beans.IntrospectionException;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.beans.PropertyDescriptor;

import javax.swing.Action;

import org.openide.nodes.Children;
import org.openide.nodes.CookieSet;
import org.openide.nodes.IndexedPropertySupport;
import org.openide.nodes.Node;
import org.openide.nodes.PropertySupport;
import org.openide.nodes.Sheet;
import org.openide.util.Utilities;
import org.openide.util.actions.SystemAction;

import cern.gp.beans.BeanTagger;
import cern.gp.beans.BeanUtils;
import cern.gp.beans.PropertyInfo;
import cern.gp.beans.impl.IntrospectionBasedNodeUpdater;
import cern.gp.beans.impl.NodeUpdaterListener;
import cern.gp.capabilities.Capability;
import cern.gp.capabilities.CapabilityProvider;
import cern.gp.nodes.cache.Cacheable;
import cern.gp.nodes.cache.CachingStrategy;
import cern.gp.nodes.cache.NoCachingStrategy;
import cern.gp.nodes.cache.StickyCachingStrategy;
import cern.gp.nodes.cache.TimeLimitedCachingStrategy;
import cern.gp.util.GPManager;

/**
 * <i><font size="-1" color="#FF0000">**For internal use only** </font></i>
 * Represents one JavaBean in the nodes hierarchy.
 * It provides all methods that are needed for communication between
 * the NetBeans platform and the bean.
 * <p>
 * You may use this node type for an already-existing JavaBean
 * in order for its JavaBean properties to be reflected
 * as corresponding node properties. Thus, it serves as a compatibility wrapper.
 * </p>
 * <p>
 * This BeanNode is based on a <code>NodeUpdater</code> that provides it the information
 * it needs. BeanNode builds a <code>NodeUpdater</code> based on the introspection of the bean.
 * Then it tries to register itself as <code>PropertyChangeListener</code> provided that the
 * bean implements the method <code>addPropertyChangeListener</code>. In such a case,
 * the BeanNode will receive the <code>PropertyChangeEvent</code> and update itself
 * when needed.
 * </p><p>
 * Whether or not the BeanNode can register itself as <code>PropertyChangeListener</code> or
 * receives any <code>PropertyChangeEvent</code>, it will still try to use the getters
 * available through introspection to initialize itself. When a given getter is not available
 * for a property needed by the node, the node will try to find a default value in the
 * <code>BeanInfo</code>.
 * </p><p>
 * To see the property this node is interested in, see the class
 * <code>cern.gp.beans.IntrospectionBasedNodeUpdater</code>.
 * </p>
 *
 * @see cern.gp.beans.impl.NodeUpdater
 * @see cern.gp.beans.impl.NodeUpdaterProvider
 * @see cern.gp.beans.impl.IntrospectionBasedNodeUpdater
 *
 * @author Lionel Mestre
 */
public class BeanNode extends org.openide.nodes.AbstractNode implements Cacheable {
  
  // register GP standard PropertyEditor 
  static {
    BeanUtils.registerEditorSearchPaths(new String[] {"cern.gp.beans.editors" });
  }
  
  //
  // -- STATIC VARIABLES -----------------------------------------------
  //

  private static final CachingStrategy DEFAULT_CACHING_STRATEGY = new NoCachingStrategy();

  /** Icon base for bean nodes */
  private static final String ICON_BASE = "org/openide/resources/beans";

  private static final Node.Property[] EMPTY_NODE_PROPERTY_ARRAY = new Node.Property[0];

  //
  // -- MEMBER VARIABLES -----------------------------------------------
  //

  /** cached value of the icon when changed */
  private java.awt.Image _iconColor16;

  /** bean */
  private Object _bean;

  /** bean info for the bean */
  private BeanInfo _beanInfo;

  private GuiUpdaterIntrospector _guiUpdater;

  private Boolean _nodePropertiesCacheable;
  
  private SystemAction _defaultAction;

  //
  // -- CONSTRUCTORS -----------------------------------------------
  //

  /**
   * Constructs a node for a JavaBean with a defined child list.
   * Intended for use by subclasses with different strategies for computing the children.
   * @param bean the bean this node will be based on
   * @param children the children of this node
   * @throws IntrospectionException if the bean cannot be analyzed
   */
  protected BeanNode(Object bean, Children children) throws IntrospectionException {
    super(children);
    this._bean = bean;
    initialization();
  }

  //
  // -- PUBLIC METHODS -----------------------------------------------
  //

  /* (non-Javadoc)
   * @see org.openide.nodes.Node#getPreferredAction()
   */
  public Action getPreferredAction() {
    if (_defaultAction == null) {
      return super.getPreferredAction();
    }
    return _defaultAction;
  }

  public Object getBean() {
    return _bean;
  }

  /**
   * Detaches all listeners from the bean and destroys it.
   * @throws IOException if there was a problem
   */
  public void destroy() throws java.io.IOException {
    _guiUpdater.removeNodeUpdaterListener(null);
    super.destroy();
  }

  /**
   * Get an icon for this node in the closed state.
   * Uses the Bean's icon if possible.
   *
   * @param type constant from {@link java.beans.BeanInfo}
   * @return icon to use
   */
  public java.awt.Image getIcon(int type) {
    if ((type == java.beans.BeanInfo.ICON_COLOR_16x16 || type == java.beans.BeanInfo.ICON_MONO_16x16)
      && _iconColor16 != null) {
      return _iconColor16;
    }
    java.awt.Image image = _beanInfo.getIcon(type);
    if (image != null)
      return image;
    return super.getIcon(type);
  }

  /**
   * Get an icon for this node in the open state.
   * @param type type constants
   * @return icon to use. The default implementation just uses {@link #getIcon(int)}.
   */
  public java.awt.Image getOpenedIcon(int type) {
    return getIcon(type);
  }

  /** Test if there is a customizer for this node. If <CODE>true</CODE>
   * the customizer can be obtained via <CODE>getCustomizer</CODE> method.
   * @return <code>true</code> if there is a customizer.
   */
  public boolean hasCustomizer() {
    // true if we have already computed beanInfo and it has customizer class
    return _beanInfo.getBeanDescriptor().getCustomizerClass() != null;
  }

  /**
   * Returns the customizer component.
   * @return the component or <code>null</code> if there is no customizer
   */
  public java.awt.Component getCustomizer() {
    Class clazz = _beanInfo.getBeanDescriptor().getCustomizerClass();
    if (clazz == null)
      return null;
    Object o;
    try {
      o = clazz.newInstance();
    } catch (InstantiationException e) {
      exception(e);
      return null;
    } catch (IllegalAccessException e) {
      exception(e);
      return null;
    }

    if (!(o instanceof Customizer))
      return null;
    Customizer cust = ((java.beans.Customizer) o);
    attachCustomizer(this, cust);
    // looking for the component
    java.awt.Component comp = null;
    if (o instanceof java.awt.Component) {
      comp = (java.awt.Component) o;
    } else {
      // create the dialog from descriptor
      comp = createDialog(o);
    }
    if (comp == null) {
      // no component provided
      return null;
    }
    cust.setObject(_bean);
    if (!_guiUpdater.hasRegisteredListenerInternal()) {
      cust.addPropertyChangeListener(new PropertyChangeListener() {
        public void propertyChange(PropertyChangeEvent e) {
          firePropertyChange(e.getPropertyName(), e.getOldValue(), e.getNewValue());
        }
      });
    }
    return comp;
  }

  //
  // -- implements Cacheable interface -------------------------------------------------
  //

  /**
   * reset the cache for this Node. As a result the node will reset the cache of all 
   * its properties.
   */
  public void resetCache() {
    Sheet.Set regularProperties = getSheet().get(Sheet.PROPERTIES);
    Sheet.Set expertProperties = getSheet().get(Sheet.EXPERT);
    if (regularProperties != null)
      resetCache(regularProperties);
    if (expertProperties != null)
      resetCache(expertProperties);
  }

  //
  // -- PROTECTED METHODS -----------------------------------------------
  //

  /**
   * Prepare node properties based on the bean, storing them into the current property sheet.
   * Called when the bean info is ready.
   * This implementation always creates a set for standard properties
   * and may create a set for expert ones if there are any.
   * @see org.openide.nodes.BeanNode#computeProperties
   * @param bean bean to compute properties for
   * @param info information about the bean
   * @param propertyInfo extra information of some properties (possibly null)
   */
  protected void createProperties(Object bean, BeanInfo info, PropertyInfo[] propertyInfo) {
    Descriptor d = computeProperties(bean, _beanInfo, true, _nodePropertiesCacheable, propertyInfo);
    Sheet sets = getSheet();
    Sheet.Set pset = Sheet.createPropertiesSet();
    pset.put(d.property);
    sets.put(pset);
    if (d.expert.length != 0) {
      Sheet.Set eset = Sheet.createExpertSet();
      eset.put(d.expert);
      sets.put(eset);
    }
  }

  /**
   * Returns the actions that shall be displayed in the pop-up menu for this node.
   * This method is called by Netbeans Explorer to build the pop-up menu for this node.
   * It uses the BeanInfo associated with the contained bean for finding the actions.
   */
  protected SystemAction[] createActions() {
    String[] actions = _guiUpdater.getNodeActions();
    if (actions == null)
      return null;
    SystemAction[] sysActions = new SystemAction[actions.length];
    for (int i = 0; i < actions.length; i++) {
      if (actions[i] != null) {
        try {
          Class actionClass = Class.forName(actions[i], true, _bean.getClass().getClassLoader());
          sysActions[i] = SystemAction.get(actionClass);
        } catch (ClassNotFoundException e) {
          warning(e);
        }
      }
    }
    return sysActions;
  }

  //
  // -- PRIVATE METHODS -----------------------------------------------
  //

  private void resetCache(String propertyName) {
    Sheet.Set regularProperties = getSheet().get(Sheet.PROPERTIES);
    Sheet.Set expertProperties = getSheet().get(Sheet.EXPERT);
    if (regularProperties != null)
      resetCache(regularProperties, propertyName);
    if (expertProperties != null)
      resetCache(expertProperties, propertyName);
  }

  private void resetCache(Sheet.Set sheetSet) {
    Property[] properties = sheetSet.getProperties();
    for (int i = 0; i < properties.length; i++) {
      if (properties[i] instanceof Cacheable) {
        ((Cacheable) properties[i]).resetCache();
      }
    }
  }

  private void resetCache(Sheet.Set sheetSet, String propertyName) {
    Property property = sheetSet.get(propertyName);
    if (property != null && property instanceof Cacheable) {
      ((Cacheable) property).resetCache();
    }
  }


  private void hideUnwantedPropertyDescriptors() {
    PropertyDescriptor[] propertyDescriptor = _beanInfo.getPropertyDescriptors();
    int k = propertyDescriptor.length;
    for (int i = 0; i < k; i++) {
      if (_guiUpdater.isPropertyHidden(propertyDescriptor[i])) {
        propertyDescriptor[i].setHidden(true); 
      }
    }
  }
  
  /**
   * Performs initalization of the node
   */
  private void initialization() throws IntrospectionException {
    setIconBase(ICON_BASE);
    Class clazz = BeanUtils.findTargetClass(_bean);
    _beanInfo = Utilities.getBeanInfo(clazz);
    _guiUpdater = new GuiUpdaterIntrospector(_bean, _beanInfo, clazz);

    // set the node properties from the bean / BeanInfo
    String name = _guiUpdater.getName();
    if (name != null && name.length() > 0)
      setName(name);
    String displayName = _guiUpdater.getDisplayName();
    if (displayName != null && displayName.length() > 0)
      setDisplayName(displayName);
    String shortDescription = _guiUpdater.getShortDescription();
    if (shortDescription != null && shortDescription.length() > 0)
      setShortDescription(shortDescription);
    _iconColor16 = _guiUpdater.getNodeIcon();
    _defaultAction = getDefaultActionInstance(_guiUpdater.getNodeDefaultAction());
    _nodePropertiesCacheable = _guiUpdater.getNodePropertiesCacheable();
    hideUnwantedPropertyDescriptors();
    createProperties(_bean, _beanInfo, _guiUpdater.getPropertyInfo());
    for (java.util.Enumeration e = _beanInfo.getBeanDescriptor().attributeNames(); e.hasMoreElements();) {
      String aname = (String) e.nextElement();
      setValue(aname, _beanInfo.getBeanDescriptor().getValue(aname));
    }
    Node.Cookie instanceCookie = createInstanceCookie(_bean);
    if (instanceCookie != null) {
      getCookieSet().add(instanceCookie);
    }
    // add capabilities from the bean to the cookieSet
    initializeCookieSet(_bean, getCookieSet());
  }

  private SystemAction getDefaultActionInstance(String defaultAction) {
    if (defaultAction == null)
      return SystemAction.get(cern.gp.actions.PropertiesAction.class);
    try {
      return SystemAction.get(Class.forName(defaultAction, true, _bean.getClass().getClassLoader()));
    } catch (ClassNotFoundException e) {
      warning(e);
      return SystemAction.get(cern.gp.actions.PropertiesAction.class);
    }
  }

  //
  // -- PRIVATE STATIC METHODS : properties -----------------------------------------------
  //

  private static Node.Property createIndexedNodeProperty(
    Object bean,
    IndexedPropertyDescriptor p,
    CachingStrategy strategy) {
    if ((p.getReadMethod() != null) && (!p.getReadMethod().getReturnType().isArray())) {
      // this is fix for #17728. This situation should never happen
      // But if the BeanInfo (IndexedPropertyDescriptor) is wrong
      // we will ignore this property
      return null;
    }
    IndexedPropertySupport support =
      new CacheableIndexedPropertySupport(
        bean,
        p.getPropertyType(),
        p.getIndexedPropertyType(),
        p.getReadMethod(),
        p.getWriteMethod(),
        p.getIndexedReadMethod(),
        p.getIndexedWriteMethod(),
        strategy);
    support.setName(p.getName());
    support.setDisplayName(p.getDisplayName());
    support.setShortDescription(p.getShortDescription());
    for (java.util.Enumeration e = p.attributeNames(); e.hasMoreElements();) {
      String aname = (String) e.nextElement();
      support.setValue(aname, p.getValue(aname));
    }
    return support;
  }

  private static Node.Property createNodeProperty(Object bean, PropertyDescriptor p, CachingStrategy strategy) {
    // Note that PS.R sets the method accessible even if it is e.g.
    // defined as public in a package-accessible superclass.
    PropertySupport.Reflection support =
      new CacheablePropertySupport(bean, p.getPropertyType(), p.getReadMethod(), p.getWriteMethod(), strategy);
    support.setName(p.getName());
    support.setDisplayName(p.getDisplayName());
    support.setShortDescription(p.getShortDescription());
    support.setPropertyEditorClass(p.getPropertyEditorClass());
    for (java.util.Enumeration e = p.attributeNames(); e.hasMoreElements();) {
      String aname = (String) e.nextElement();
      support.setValue(aname, p.getValue(aname));
    }
    return support;
  }

  private static CachingStrategy createCachingStrategy(Boolean beanCacheable, Boolean propertyCacheable) {
    boolean shouldCache = false;
    if (propertyCacheable == null) {
      // unspecified at the property level : we use bean level
      shouldCache = beanCacheable != null && beanCacheable.booleanValue();
    } else {
      // specified at the property level : override bean level
      shouldCache = propertyCacheable.booleanValue();
    }
    if (shouldCache) {
      return new StickyCachingStrategy();
    } else {
      return new TimeLimitedCachingStrategy(500);
    }
  }

  /** 
   * Computes a descriptor for properties from a bean info.
   * @param bean bean to create properties for
   * @param info about the bean
   * @param ignoreHiddenProperties true if hidden property should be ignored completely
   * @param propertyInfo extra information of some properties (possibly null)
   * @return three property lists
   */
  private static Descriptor computeProperties(
    Object bean,
    BeanInfo info,
    boolean ignoreHiddenProperties,
    Boolean nodePropertiesCacheable,
    PropertyInfo[] propertyInfo) {
    java.util.List property = null;
    java.util.List expert = null;
    java.util.List hidden = null;
    PropertyDescriptor[] propertyDescriptor = info.getPropertyDescriptors();
    int k = propertyDescriptor.length;
    for (int i = 0; i < k; i++) {
      if (propertyDescriptor[i].getPropertyType() == null)
        continue;
      String propName = propertyDescriptor[i].getName();
      // we first update the PropertyDescriptor with the PropertyInfo
      // that update may make non hidden a property that was hidden
      PropertyInfo propInfo = findPropertyInfoByName(propertyInfo, propName);
      if (propInfo != null)
        propInfo.updatePropertyDescriptor(propertyDescriptor[i]);
      // after the update we can test whether the property is hidden or not
      if (ignoreHiddenProperties && propertyDescriptor[i].isHidden()) {
        continue;
      }
      CachingStrategy strategy =
        createCachingStrategy(nodePropertiesCacheable, BeanTagger.isCacheable(propertyDescriptor[i]));
      Node.Property prop;
      if (propertyDescriptor[i] instanceof IndexedPropertyDescriptor) {
        prop = createIndexedNodeProperty(bean, (IndexedPropertyDescriptor) propertyDescriptor[i], strategy);
      } else {
        prop = createNodeProperty(bean, propertyDescriptor[i], strategy);
      }
      if (prop == null)
        continue;
      // Add to right category.
      
      if (propertyDescriptor[i].isHidden()) {
        if (hidden == null)
          hidden = new java.util.ArrayList();
        hidden.add(prop);
      } else if (propertyDescriptor[i].isExpert()) {
        if (expert == null)
          expert = new java.util.ArrayList();
        expert.add(prop);
      } else {
        if (property == null)
          property = new java.util.ArrayList();
        property.add(prop);
      }
    } // for    
    return new Descriptor(property, expert, hidden);
  }

  private static PropertyInfo findPropertyInfoByName(PropertyInfo[] propertyInfo, String propName) {
    if (propertyInfo == null)
      return null;
    for (int i = 0; i < propertyInfo.length; i++) {
      if (propName.equals(propertyInfo[i].getName()))
        return propertyInfo[i];
    }
    return null;
  }

  //
  // -- PRIVATE STATIC METHODS : Helpers -----------------------------------------------
  //

  private static void exception(Throwable e) {
    GPManager.notify(GPManager.INFORMATIONAL, e);
  }

  private static void warning(Throwable e) {
    GPManager.notify(GPManager.WARNING, e);
  }

  /**
   * Initializes the CookieSet with the corresponding Capability available
   * from the bean directly or through a CapabilityProvider
   * @param bean the bean from which to find the capabilities
   * @param cookieSet the cookie set to update
   */
  private static void initializeCookieSet(Object bean, CookieSet cookieSet) {
    if (bean instanceof CapabilityProvider) {
      // the bean is a provides a method to get capabilities
      CapabilityProvider provider = (CapabilityProvider) bean;
      Capability[] capabilities = provider.getCapabilities();
      if (capabilities == null || capabilities.length == 0)
        return;
      for (int i = 0; i < capabilities.length; i++) {
        cookieSet.add(capabilities[i]);
      }
    } else {
      // we introspect to get the capabilities
      Class[] interfaces = bean.getClass().getInterfaces();
      for (int i = 0; i < interfaces.length; i++) {
        if (Capability.class.isAssignableFrom(interfaces[i])) {
          cookieSet.add((Capability) bean);
        }
      }
    }
  }

  /** Creates InstanceCookie, if available.
   * @param bean the object to create cookie for
   * @return Node.Cookie or null
   */
  private static final Node.Cookie createInstanceCookie(final Object o) {
    return new org.openide.cookies.InstanceCookie() {
      public String instanceName() {
        return o.getClass().getName();
      }
      public Class instanceClass() {
        return o.getClass();
      }
      public Object instanceCreate() {
        return o;
      }
    };
  }

  /** Checks whether an object is instance of DialogDescriptor and if
   * so it used top manager to create its instance.
   * @param maybeDialogDescriptor an object
   * @return a dialog or null
   */
  private static final java.awt.Dialog createDialog(Object o) {
    if (o instanceof org.openide.DialogDescriptor) {
      return GPManager.createDialog((org.openide.DialogDescriptor) o);
    }
    return null;
  }

  /** Attaches a customizer to given node.
   * @param node the bean node
   * @param cust customizer to attach
   */
  private static void attachCustomizer(Node node, java.beans.Customizer cust) {
    if (cust instanceof org.openide.explorer.propertysheet.editors.NodeCustomizer) {
      ((org.openide.explorer.propertysheet.editors.NodeCustomizer) cust).attach(node);
    }
  }

  //
  // -- INNER CLASSES -----------------------------------------------
  //

  /** 
   * Descriptor of three types of properties. Regular, expert and hidden.
   */
  private static final class Descriptor {
    /** Regular properties. */
    public final Node.Property[] property;
    /** Expert properties. */
    public final Node.Property[] expert;
    /** Hidden properties. */
    public final Node.Property[] hidden;

    /** private constructor */
    Descriptor(java.util.List property, java.util.List expert, java.util.List hidden) {
      if (property == null || property.size() == 0) {
        this.property = EMPTY_NODE_PROPERTY_ARRAY;
      } else {
        this.property = new Node.Property[property.size()];
        property.toArray(this.property);
      }
      if (expert == null || expert.size() == 0) {
        this.expert = EMPTY_NODE_PROPERTY_ARRAY;
      } else {
        this.expert = new Node.Property[expert.size()];
        expert.toArray(this.expert);
      }
      if (hidden == null || hidden.size() == 0) {
        this.hidden = EMPTY_NODE_PROPERTY_ARRAY;
      } else {
        this.hidden = new Node.Property[hidden.size()];
        hidden.toArray(this.hidden);
      }
    }
  }

  /**
   * Inherits from <code>IntrospectionBasedNodeUpdater</code> in order to hook
   * the abstract <code>fire***Change</code> to the properties of this node.
   */
  private final class GuiUpdaterIntrospector extends IntrospectionBasedNodeUpdater {

    public GuiUpdaterIntrospector(Object bean, BeanInfo beanInfo, Class targetClass) throws IntrospectionException {
      super(bean, beanInfo, targetClass);
    }

    boolean hasRegisteredListenerInternal() {
      return hasRegisteredListener();
    }

    //
    // -- implements NodeUpdater -----------------------------------------------
    //

    public void addNodeUpdaterListener(NodeUpdaterListener listener) {
    }

    public void removeNodeUpdaterListener(NodeUpdaterListener listener) {
      removePropertyChangeListener();
    }

    //
    // -- PROTECTED METHODS -----------------------------------------------
    //

    protected void firePropertyChange(String propertyName, Object oldValue, Object newValue) {
      if (propertyName == null) {
        resetCache();
      } else {
        resetCache(propertyName);
      }
      BeanNode.this.firePropertyChange(propertyName, oldValue, newValue);
    }

    protected void fireNameChange(String newName) {
      setName(newName);
    }

    protected void fireDisplayNameChange(String newDisplayName) {
      setDisplayName(newDisplayName);
    }

    protected void fireShortDescriptionChange(String newShortDescription) {
      setShortDescription(newShortDescription);
    }

    protected void fireNodeIconChange(java.awt.Image newIcon) {
      _iconColor16 = newIcon;
      BeanNode.this.fireIconChange();
    }

    protected void fireNodeDefaultActionChange(String newDefaultAction) {
      _defaultAction = getDefaultActionInstance(newDefaultAction);
    }

  } // end inner class GuiUpdaterIntrospector

}
