/*
 * $Id: CategoryImpl.java,v 1.5 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.5 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.data;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import cern.laser.business.definition.data.CategoryDefinition;

/**
 * 
 * 
 * 
 * @version $Revision: 1.5 $ $Date: 2006/09/25 08:52:36 $
 * @author Niall Stapley
 * @author Katarina Sigerud
 * 
 * @hibernate.class table="CATEGORY"
 */
public class CategoryImpl implements Serializable, Cloneable, Category {
  private Integer categoryId;
  private String name;
  private String description;
  private Integer parentId;
  private Set childrenIds;
  private Set alarmIds;
  private String path;
  private boolean isLeaf;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  /**
   * Empty constructor for Hibernate.
   *  
   */
  public CategoryImpl() {
  }

  /**
   * @param categoryDefinition
   */
  public CategoryImpl(CategoryDefinition definition) {
    setDefinition(definition);
  }

	/**
	 * @param categoryId
	 * @param name
	 * @param description
	 * @param path
	 * @param isLeaf
	 */
	public CategoryImpl(Integer categoryId, String name, String description,
			String path, boolean isLeaf) {
		this.categoryId = categoryId;
		this.name = name;
		this.description = description;
		this.path = path;
		this.isLeaf = isLeaf;
	}
  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  /**
   * 
   * @hibernate.id generator-class="sequence" column="CATEGORY_ID" unsaved-value="null"
   * @hibernate.generator-param name="sequence" value="SQ_PK_CATEGORY" 
   */
  public Integer getCategoryId() {
    return categoryId;
  }

  /**
   * 
   * @return the name
   * @hibernate.property name="getName" column="NAME" not-null="false" unique="false"
   */
  public String getName() {
    return name;
  }

  /**
   * 
   * @return the descritpion
   * @hibernate.property name="getDescription" column="DESCRIPTION" not-null="false" unique="false"
   */
  public String getDescription() {
    return description;
  }

  /**
   * 
   * @return the parentId
   * @hibernate.property name="getParentId" column="PARENT_ID" not-null="false" unique="false"
	 * 
	 * @return Integer
	 *
   */
  public Integer getParentId() {
    return parentId;
  }

  /**
   * @param category
   */
  public void addChildCategory(Category category) {
    ((CategoryImpl) category).setParentId(getCategoryId());
    getChildrenIds().add(category.getCategoryId());
    isLeaf = getChildrenIds().isEmpty();
  }

  public void removeChildCategory(Category category) {
    ((CategoryImpl) category).setParentId(null);
    getChildrenIds().remove(category.getCategoryId());
    isLeaf = getChildrenIds().isEmpty();
  }

  public boolean isLeaf() {
    return getChildrenIds().isEmpty(); //isLeaf;
  }

  /**
   * Sets up the bi-directional relationship between this category and the given alarm.
   * @param alarm the alarm to add. 
   * At the same time this category is added to the alarm's collection of categories.
   */
  public void addAlarm(Alarm alarm) {
    alarm.getCategories().add(this);
    getAlarmIds().add(alarm.getAlarmId());
  }
  
  /**
   * Removes the bi-directional relationship between this category and the given alarm.
   * @param alarm the alarm to remove. 
   * At the same time this category is removed from the alarm's collection of categories.
   */
  public void removeAlarm(Alarm alarm) {
    alarm.getCategories().remove(this);
    getAlarmIds().remove(alarm);
  }
 
  /**
   * @param alarmId
   * @return
   */
  public boolean containsAlarm(Alarm alarm) {
    return getAlarmIds().contains(alarm.getAlarmId());
  }

  /**
   * @hibernate.property name="getPath" column="PATH" not-null="false" unique="false"
	 * 
	 * @return String
   * @return
   */
  public String getPath() {
    return path;
  }

  /**
   * @return
   */
  public CategoryDefinition getDefinition() {
    return new CategoryDefinition(getPath(), getDescription());
  }


  public void setDefinition(CategoryDefinition definition) {
      int last_dot_position = definition.getPath().lastIndexOf(".");
      String category_name = definition.getPath().substring(last_dot_position + 1);
      setName(category_name);
      setDescription(definition.getDescription());
      setPath(definition.getPath());
  }

  public void setCategoryId(Integer newCategoryId) {
    categoryId = newCategoryId;
  }

  public void setName(String newName) {
    name = newName;
  }

  public void setDescription(String newDescription) {
    description = newDescription;
  }

  public void setParentId(Integer newParentId) {
    parentId = newParentId;
  }

  public void setPath(String newPath) {
    path = newPath;
  }

  public boolean equals(Object obj) {
    if ((obj == null) || (!(obj instanceof CategoryImpl))) { return false; }
    return getCategoryId().equals(((CategoryImpl) obj).getCategoryId());
  }

  public int hashCode() {
    return getCategoryId().hashCode();
  }

  public Object clone() {
    try {
      CategoryImpl category = (CategoryImpl) super.clone();
      category.setChildrenIds(getChildrenIds() == null ? null : new HashSet(getChildrenIds()));

      return category;
    } catch (Exception e) {
      throw new InternalError("unable to clone category "+getPath()+" : "+e.getMessage());
    }
  }

  public String toString() {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append("\nID : ");
    str_buf.append(categoryId);
    str_buf.append("\nNAME : ");
    str_buf.append(name);
    str_buf.append("\nDESCRIPTION : ");
    str_buf.append(description);
    str_buf.append("\nPATH : ");
    str_buf.append(getPath());
    str_buf.append("\nPARENT ID : ");
    str_buf.append(parentId);
    str_buf.append("\nCHILDREN ID : ");
    str_buf.append(getChildrenIds());

    return str_buf.toString();
  }

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //

  /**
   * @hibernate.set name="getChildrenIds" table="CATEGORY" inverse="true" cascade="all-delete-orphan" lazy="true"
   * @hibernate.collection-key column="PARENT_ID"
   * @hibernate.collection-element column="CATEGORY_ID" type="integer"
   * hib
 */
  public Set getChildrenIds() {
    return childrenIds == null ? new HashSet(0) : childrenIds;
  }

  public void setChildrenIds(Set newChildren) {
    childrenIds = newChildren;
    isLeaf = getChildrenIds().isEmpty();
  }

  /**
   * @hibernate.set name="getAlarmIds" table="ALARM_CATEGORY" inverse="false" lazy="true"
   * @hibernate.collection-key column="CATEGORY_ID"
   * @hibernate.collection-element column="ALARM_ID" type="string"
   */
  public Set getAlarmIds() {
    return alarmIds == null ? new HashSet(0) : alarmIds;
  }

  /**
   * @param newAlarms The alarms to set.
   */
  public void setAlarmIds(Set newAlarms) {
  	// this was made public, because Hibernate can set it, but ACS can't
    alarmIds = newAlarms;
  }
}