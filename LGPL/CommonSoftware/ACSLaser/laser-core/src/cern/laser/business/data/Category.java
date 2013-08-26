/*
 * $Id: Category.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.data;

import cern.laser.business.definition.data.CategoryDefinition;

/**
 * 
 * 
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 * @author Katarina Sigerud
 */
public interface Category extends Cloneable {
  //
  public Integer getCategoryId();

  /**
   * 
   * @return the name
   */
  public String getName();

  /**
   * 
   * @return the descritpion
   */
  public String getDescription();

  /**
   * 
   * @return the parentId
   * 
   * @return Integer
   *
   */
  public Integer getParentId();

  /**
   * @param category
   */
  public void addChildCategory(Category category);

  public void removeChildCategory(Category category);

  public boolean isLeaf();

  //  /**
  public void addAlarm(Alarm alarm);

  /**
   * Removes the bi-directional relationship between this category and the given alarm.
   * @param alarm the alarm to remove. 
   * At the same time this category is removed from the alarm's collection of categories.
   */
  public void removeAlarm(Alarm alarm);

  /**
   * @param alarmId
   * @return
   */
  public boolean containsAlarm(Alarm alarm);

  /**
   * 
   * @return String
   * @return
   */
  public String getPath();

  /**
   * @return
   */
  public CategoryDefinition getDefinition();
  
  public void setDefinition(CategoryDefinition definition);

}