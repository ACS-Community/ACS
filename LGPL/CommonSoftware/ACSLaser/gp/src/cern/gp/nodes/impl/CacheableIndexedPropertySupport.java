/*
 * $Id: CacheableIndexedPropertySupport.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
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

import org.openide.nodes.IndexedPropertySupport;

import cern.gp.beans.editors.support.BeanDependentPropertyEditor;
import cern.gp.nodes.cache.Cacheable;
import cern.gp.nodes.cache.CachingStrategy;

/**
 * <i><font size="-1" color="#FF0000">**For internal use only** </font></i>
 * A version of IndexedPropertySupport that caches the property value to avoid that
 * the underlying Bean has to handle bursts of getValue() calls.
 * The caching strategy is given at construction time.
 * @author  Lionel Mestre
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
class CacheableIndexedPropertySupport extends IndexedPropertySupport implements Cacheable {
  
  /** cached value */
  private Object cachedValue;
  private CachingStrategy cachingStrategy;
  
  //
  // -- CONSTRUCTORS -----------------------------------------------
  //
  
  /** Constructor.
   * @param instance the bean for which these properties exist
   * @param valueType type of the entire property
   * @param elementType type of one element of the property
   * @param getter get method for the entire property
   * @param setter set method for the entire property
   * @param indexedGetter get method for one element
   * @param indexedSetter set method for one element
   */
  public CacheableIndexedPropertySupport(Object instance, Class valueType, Class elementType,
  Method getter, Method setter, Method indexedGetter, Method indexedSetter,
  CachingStrategy cachingStrategy) {
    super(instance, valueType, elementType, getter, setter, indexedGetter, indexedSetter);
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
   * overrides getIndexedValue, and caches the value
   */
  public Object getIndexedValue(int index) throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
    if (! cachingStrategy.isCacheValid()) {
      // get a new value
      cachedValue = super.getValue();
      // enable the cache
      cachingStrategy.validateCache();
    }
    return java.lang.reflect.Array.get(cachedValue, index);
  }
  
  /**
   * overrides setIndexedValue, and caches the value
   */
  public void setIndexedValue(int index, Object val) throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
    super.setIndexedValue(index, val);
    java.lang.reflect.Array.set(cachedValue, index, val);
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