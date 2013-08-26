/*
 * $Id: CacheablePropertySupport.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.nodes.impl;

import java.beans.PropertyEditor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.openide.nodes.PropertySupport;

import cern.gp.beans.editors.support.BeanDependentPropertyEditor;
import cern.gp.nodes.cache.Cacheable;
import cern.gp.nodes.cache.CachingStrategy;

/**
 * <i><font size="-1" color="#FF0000">**For internal use only** </font></i>
 * A version of PropertySupport that caches the property value to avoid that
 * the underlying Bean has to handle bursts of getValue() calls.
 * The caching strategy is given at construction time.
 * @author  Lionel Mestre
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $ 
 */
class CacheablePropertySupport extends PropertySupport.Reflection implements Cacheable {
  
  /** cached value */
  private Object cachedValue;
  private CachingStrategy cachingStrategy;
  
  //
  // -- CONSTRUCTORS -----------------------------------------------
  //
  
  //------------- overrides PropertySupport.Reflection -----------------------
  public CacheablePropertySupport(Object instance, Class valueType, Method getter, Method setter, CachingStrategy cachingStrategy) {
    super(instance, valueType, getter, setter);
    this.cachingStrategy = cachingStrategy;
  }
  
  
  //
  // -- PUBLIC METHODS -----------------------------------------------
  //
  
  /**
   * overrides getValue, and accesses the Bean only if the cache has expired
   */
  public Object getValue() throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
    if (! cachingStrategy.isCacheValid()) {
      // get a new value
      cachedValue = super.getValue();
      // enable the cache
      cachingStrategy.validateCache();
    }
    return cachedValue;
  }
  
  /**
   * overrides setValue, and caches the value
   */
  public void setValue(Object newValue) throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
    super.setValue(newValue);
    cachedValue = newValue;
    cachingStrategy.validateCache();
  }
  
  /**
   * Returns an instance of property editor for this property.
   * The implementation reconizes a special type of PropertyEditor
   * that implements <code>cern.gp.beans.BeanDependentPropertyEditor</code> and 
   * invoke their initialization method.
   * @return the property editor or <CODE>null</CODE> if there should not be any editor.
   * @see cern.gp.beans.BeanDependentPropertyEditor
   */
  public PropertyEditor getPropertyEditor() {
    PropertyEditor propertyEditor = super.getPropertyEditor();
    if (propertyEditor != null && (propertyEditor instanceof BeanDependentPropertyEditor)) {
      ((BeanDependentPropertyEditor) propertyEditor).initializePropertyEditor(instance, getName());
    }
    return propertyEditor;
  }
  
  //
  // -- implements Cacheable interface -------------------------------------------------
  //
  
  /**
   * reset the cache for this property
   */
  public void resetCache() {
    cachingStrategy.invalidateCache();
  }
    
  //
  // -- PROTECTED METHODS -----------------------------------------------
  //

  //
  // -- PRIVATE METHODS -----------------------------------------------
  //
  
}