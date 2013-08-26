/*
 * $Id: CategoryImpl.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.impl.data;

import java.io.Serializable;

import cern.laser.client.data.Category;

public class CategoryImpl implements Category, Cloneable, Serializable {
  private Integer categoryId;
  private String name;
  private String description;
  private String path;
  private boolean leaf;

  public CategoryImpl(cern.laser.business.data.Category category) {
    if (category == null) { throw new IllegalArgumentException("parameter can not be null"); }
    categoryId = category.getCategoryId();
    name = category.getName();
    description = category.getDescription();
    path = category.getPath();
    leaf = category.isLeaf();
  }

  public Integer getCategoryId() {
    return categoryId;
  }

  public String getName() {
    return name;
  }

  public String getDescription() {
    return description;
  }

  public String getPath() {
    return path;
  }

  public boolean isLeaf() {
    return leaf;
  }

  public void setCategoryId(Integer newCategoryId) {
    categoryId = newCategoryId;
  }

  public void setDescription(String newDescription) {
    description = newDescription;
  }

  public void setLeaf(boolean newLeaf) {
    leaf = newLeaf;
  }

  public void setName(String newName) {
    name = newName;
  }

  public void setPath(String newPath) {
    path = newPath;
  }

  public boolean equals(Object obj) {
    if ((obj == null) || (!(obj instanceof Category))) { return false; }
    Category category = (Category) obj;

    return getCategoryId().equals(category.getCategoryId());
  }

  public int hashCode() {
    return getCategoryId().hashCode();
  }

  public String toString() {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append("\nID : ");
    str_buf.append(getCategoryId());
    str_buf.append("\nNAME : ");
    str_buf.append(getName());
    str_buf.append("\nDESCRIPTION : ");
    str_buf.append(getDescription());
    str_buf.append("\nPATH : ");
    str_buf.append(getPath());
    str_buf.append("\nLEAF : ");
    str_buf.append(isLeaf());

    return str_buf.toString();
  }

  public Object clone() throws CloneNotSupportedException {
    try {
      CategoryImpl category = (CategoryImpl) super.clone();

      return category;
    } catch (Exception e) {
      throw new CloneNotSupportedException("clone failed : " + e.getMessage());
    }
  }

}