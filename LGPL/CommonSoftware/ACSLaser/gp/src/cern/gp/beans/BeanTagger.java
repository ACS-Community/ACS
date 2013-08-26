/*
 * $Id: BeanTagger.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
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
import java.beans.FeatureDescriptor;

/**
 * Utility class that can tag a Property or a BeanInfo to add information to it.
 * The available static methods describe what information it is possible to add.
 * 
 * @version $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public class BeanTagger {

  /** name of property that can be used to specify that a property or a bean is cacheable
   * which neams that the property value or all properties values of the bean can be cached */
  private static final String IS_CACHEABLE_PROPERTY_NAME = "beantagger.isCacheable";

  /** name of property that can be used to specify that a property is expandable
   * which neams that the property value is a bean having more properties */
  private static final String IS_EXPANDABLE_PROPERTY_NAME = "beantagger.isExpandable";

  /** name of property that is used to specify the qname of available actions */
  private static final String ACTIONS_PROPERTY_NAME = "beantagger.actions";
  
  /** name of property that is used to specify the qname of the default action */
  private static final String DEFAULT_ACTION_PROPERTY_NAME = "beantagger.defaultAction";

  private static final String EMPTY_STRING = "";

  /** prevent the creation of an new instance */
  private BeanTagger() {
  }

  /**
   * Adds the information that this <code>FeatureDescriptor</code> is cacheable
   * which neams that its value(s) can be cached.
   * If it is a <code>PropertyDescriptor</code> it means that the value of the property can be cached.
   * If it is a <code>BeanDescriptor</code> it means that all property values can be cached
   * @param featureDescriptor the <code>FeatureDescriptor</code> to tag as expandable.
   * @param cacheable whether the value(s) of the FeatureDescriptor can be cached or not
   */
  public static void setCacheable(FeatureDescriptor featureDescriptor, boolean cacheable) {
    setBooleanValue(featureDescriptor, IS_CACHEABLE_PROPERTY_NAME, cacheable);
  }

  /**
   * Checks if the given <code>FeatureDescriptor</code> is cacheable
   * @return true if and only if this <code>FeatureDescriptor</code> is cacheable.
   */
  public static Boolean isCacheable(FeatureDescriptor featureDescriptor) {
    return (Boolean) featureDescriptor.getValue(IS_CACHEABLE_PROPERTY_NAME);
  }

  
  /**
   * Adds the information that this <code>FeatureDescriptor</code> is expandable
   * which neams that it can be expanded into a list of more FeatureDescriptors
   * @param featureDescriptor the <code>FeatureDescriptor</code> to tag as expandable.
   * @param expandable whether the FeatureDescriptor is expandable or not
   */
  public static void setExpandable(FeatureDescriptor featureDescriptor, boolean expandable) {
    setBooleanValue(featureDescriptor, IS_EXPANDABLE_PROPERTY_NAME, expandable);
  }

  /**
   * Checks if this <code>FeatureDescriptor</code> is expandable
   * which neams that it can be expanded into a list of more FeatureDescriptors
   * @return true if and only if this <code>FeatureDescriptor</code> is expandable.
   */
  public static boolean isExpandable(FeatureDescriptor featureDescriptor) {
    return getBooleanValue(featureDescriptor, IS_EXPANDABLE_PROPERTY_NAME);
  }

  
  /**
   * Sets the default action qualified name for this <code>FeatureDescriptor</code>
   * @param featureDescriptor the <code>FeatureDescriptor</code> to tag with the default action
   * @param defaultAction the qualified name of the default action
   */
  public static void setDefaultAction(FeatureDescriptor featureDescriptor, String defaultAction) {
    featureDescriptor.setValue(DEFAULT_ACTION_PROPERTY_NAME, defaultAction);
  }

  /**
   * Sets the default action qualified name for this <code>BeanInfo</code>
   * @param beanInfo the <code>BeanInfo</code> to tag with the default action.
   * @param defaultAction the qualified name of the default action
   */
  public static void setDefaultAction(BeanInfo beanInfo, String defaultAction) {
    BeanDescriptor descriptor = beanInfo.getBeanDescriptor();
    if (descriptor == null) return;
    setDefaultAction(descriptor, defaultAction);
  }
  
  /**
   * Gets the default action qualified name for this <code>FeatureDescriptor</code>
   * @param featureDescriptor the <code>FeatureDescriptor</code> to get the default action from
   * @return the qualified name of the default action or null is not specified
   */
  public static String getDefaultAction(FeatureDescriptor featureDescriptor) {
    return (String) featureDescriptor.getValue(DEFAULT_ACTION_PROPERTY_NAME);
  }

  /**
   * Gets the default action qualified name for this <code>BeanInfo</code>
   * @param beanInfo the <code>BeanInfo</code> to get the default action from
   * @return the qualified name of the default action or null is not specified
   */
  public static String getDefaultAction(BeanInfo beanInfo) {
    BeanDescriptor descriptor = beanInfo.getBeanDescriptor();
    if (descriptor == null) return null;
    return getDefaultAction(descriptor);
  }
  
  /**
   * Adds classes of available actions to this descriptor. If the featureDescriptor already 
   * defines some actions the existing ones and the new ones are merged together.
   * @param actionClasses an array of the class of available action(s)
   */
  public static void addActions(FeatureDescriptor featureDescriptor, String[] actions) {
    if (actions == null)
      throw new IllegalArgumentException("actionClasses cannot be null");
    String[] currentActions = getActions(featureDescriptor);
    if (currentActions == null || currentActions.length == 0) {
      // if no existing ones we take only the new ones
      featureDescriptor.setValue(ACTIONS_PROPERTY_NAME, actions);
      return;
    }
    // checking the actions that are not already in the current ones
    int numAdded = 0;
    String[] actionsCopy = (String[]) actions.clone();
    for (int i = 0; i < actionsCopy.length; i++) {
      if (actionsCopy != null && doesActionExist(currentActions, actionsCopy[i])) {
        actionsCopy[i] = EMPTY_STRING;
      } else {
        // we keep the null actions as they are separator
        numAdded++;
      }
    }
    if (numAdded == 0)
      return;
    // we have "numAdded" new action classes in actionClassesCopy
    String[] result = new String[currentActions.length + numAdded];
    System.arraycopy(currentActions, 0, result, 0, currentActions.length);
    int n = currentActions.length;
    for (int i = 0; i < actionsCopy.length; i++) {
      if (actionsCopy[i] != EMPTY_STRING) {
        result[n] = actionsCopy[i];
        n++;
      }
    }
    featureDescriptor.setValue(ACTIONS_PROPERTY_NAME, result);
  }

  /**
   * Gets the class of available actions for this descriptor
   * @return an array of class of available actions for this descriptor
   */
  public static String[] getActions(FeatureDescriptor featureDescriptor) {
    return (String[]) featureDescriptor.getValue(ACTIONS_PROPERTY_NAME);
  }

  /**
   * Gets the class of available actions for this BeanInfo
   * @return an array of class of available actions for this BeanInfo
   */
  public static String[] getActions(BeanInfo beanInfo) {
    BeanDescriptor descriptor = beanInfo.getBeanDescriptor();
    if (descriptor == null) return null;
    return getActions(descriptor);
  }

  /**
   * Adds classes of available actions to this BeanInfo. If the BeanInfo already 
   * defines some actions the existing ones and the new ones are merged together.
   * @param actionClasses an array of the class of available action(s)
   */
  public static void addActions(BeanInfo beanInfo, String[] actions) {
    BeanDescriptor descriptor = beanInfo.getBeanDescriptor();
    if (descriptor == null)
      return;
    addActions(descriptor, actions);
  }


  //
  // -- PRIVATE METHODS -----------------------------------------------
  //

  private static final boolean getBooleanValue(FeatureDescriptor featureDescriptor, String property) {
    Object o = featureDescriptor.getValue(property);
    if (o == null) return false;
    if (o.equals(Boolean.TRUE))
      return true;
    return false;
  }
  
  private static final void setBooleanValue(FeatureDescriptor featureDescriptor, String property, boolean value) {
    featureDescriptor.setValue(property, value ? Boolean.TRUE : Boolean.FALSE);
  }

  
  private static final boolean doesActionExist(String[] actions, String action) {
    for (int i = 0; i < actions.length; i++) {
      if (actions[i] != null) {
        if (actions[i].equals(action)) {
          return true;
        }
      }
    }
    return false;
  }

}
