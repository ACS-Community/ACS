/*
 * $Id: PropertyInfoSupport.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $
 * $Revision: 1.2 $
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.gp.beans;

import java.beans.PropertyDescriptor;

/**
 * Support class allowing to create instances of <code>PropertyInfo</code>.
 * 
 * @see PropertyInfo
 * @see cern.gp.beans.editors.support.BeanDependentPropertyEditor
 * @version   $Revision: 1.2 $  $Date: 2006/09/25 08:52:36 $
 * @author Lionel Mestre
 */
public class PropertyInfoSupport implements PropertyInfo {
  
  private String name;
  private String displayName;
  private Class propertyEditorClass;
  private Boolean isCacheable;
  private boolean isExpert;
  private boolean isHidden;

  //
  // -- CONSTRUCTORS -----------------------------------------------
  //
  
  /**
   * Creates a new PropertyInfoSupport with a given PropertyEditorClass that is non cached, non expert and non hidden.
   * The display name of the property will the name of the property.
   * @param name the name of the property that <code>PropertyInfo</code> object is for
   * @param propertyEditorClass the class of the property editor for this property
   * @see cern.gp.beans.editors.support.BeanDependentPropertyEditor
   */
  public PropertyInfoSupport(String name, Class propertyEditorClass) {
    this(name, null, propertyEditorClass, false);
  }
  
  /**
   * Creates a new PropertyInfoSupport with no PropertyEditor that is non cached, non expert and non hidden.
   * The display name of the property will be the one specified.
   * @param name the name of the property that <code>PropertyInfo</code> object is for
   * @param propertyEditorClass the class of the property editor for this property
   * @see cern.gp.beans.editors.support.BeanDependentPropertyEditor
   */
  public PropertyInfoSupport(String name, String displayName) {
    this(name, displayName, null, false);
  }
  
  /**
   * Creates a new PropertyInfoSupport with a given PropertyEditorClass that is non cached, non expert and non hidden.
   * The display name of the property will be the one specified.
   * @param name the name of the property that <code>PropertyInfo</code> object is for
   * @param displayName the display name of the property
   * @param propertyEditorClass the class of the property editor for this property
   * @see cern.gp.beans.editors.support.BeanDependentPropertyEditor
   */
  public PropertyInfoSupport(String name, String displayName, Class propertyEditorClass) {
    this(name, displayName, propertyEditorClass, false);
  }
  
  /**
   * Creates a new PropertyInfoSupport with no PropertyEditorClass that is non cached, non expert and hidden or not
   * depending of the given value.
   * @param name the name of the property that <code>PropertyInfo</code> object is for
   * @param isHidden whether this property is hidden or not
   */
  public PropertyInfoSupport(String name, boolean isHidden) {
    this(name, null, null, isHidden);
  }
  
  private PropertyInfoSupport(String name, String displayName, Class propertyEditorClass, boolean isHidden) {
    this.name = name;
    this.displayName = displayName;
    this.propertyEditorClass = propertyEditorClass;
    this.isHidden = isHidden;
  }
  
  //
  // -- PUBLIC METHODS -----------------------------------------------
  //  
  
  /**
   * Sets whether the property is cached or no. Null can be used to specify the default behavior.
   * @param isCached whether the property is cached or no.
   */
  public void setIsCached(Boolean isCached) {
    this.isCacheable = isCached;
  }

  /**
   * Sets whether the property is only visible to experts or no 
   * @param isExpert whether the property is only visible to experts or no 
   */
  public void setIsExpert(boolean isExpert) {
    this.isExpert = isExpert;
  }

  /**
   * Sets whether the property is hidden or no 
   * @param isHidden whether the property is hidden or no 
   */
  public void setIsHidden(boolean isHidden) {
    this.isHidden = isHidden;
  }


  //
  // -- implements PropertyInfo -----------------------------------------------
  //
  
  /* (non-Javadoc)
   * @see cern.gp.beans.PropertyInfo#updatePropertyDescriptor()
   */
  public void updatePropertyDescriptor(PropertyDescriptor propertyDescriptor) {
    propertyDescriptor.setHidden(isHidden);
    propertyDescriptor.setExpert(isExpert);
    if (displayName != null)
      propertyDescriptor.setDisplayName(displayName);
    if (isCacheable != null) 
      BeanTagger.setCacheable(propertyDescriptor, isCacheable.booleanValue());
    if (propertyEditorClass != null) 
      propertyDescriptor.setPropertyEditorClass(propertyEditorClass);
  }

  /* (non-Javadoc)
   * @see cern.gp.beans.PropertyInfo#getName()
   */
  public String getName() {
    return name;
  }

  /* (non-Javadoc)
   * @see cern.gp.beans.PropertyInfo#getDisplayName()
   */
  public String getDisplayName() {
    return displayName;
  }

  /* (non-Javadoc)
   * @see cern.gp.beans.PropertyInfo#getPropertyEditorClass()
   */
  public Class getPropertyEditorClass() {
    return propertyEditorClass;
  }

  /* (non-Javadoc)
   * @see cern.gp.beans.PropertyInfo#isCacheable()
   */
  public Boolean isCacheable() {
    return isCacheable;
  }

  /* (non-Javadoc)
   * @see cern.gp.beans.PropertyInfo#isExpert()
   */
  public boolean isExpert() {
    return isExpert;
  }

  /* (non-Javadoc)
   * @see cern.gp.beans.PropertyInfo#isHidden()
   */
  public boolean isHidden() {
    return isHidden;
  }


  //
  // -- PROTECTED METHODS -----------------------------------------------
  //
  
  //
  // -- PRIVATE METHODS -----------------------------------------------
  //

}