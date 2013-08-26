/*
 * $Id: CategorySelectionImpl.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.impl.services.selection;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import cern.laser.client.data.Category;
import cern.laser.client.services.selection.CategorySelection;

public class CategorySelectionImpl implements CategorySelection, Serializable {
  private Set categorySet = null;

  public CategorySelectionImpl() {
    categorySet = new HashSet();
  }

  public boolean add(Category category) {
    if (category == null) { throw new IllegalArgumentException("parameter cannot be null"); }

    return categorySet.add(category);
  }

  public boolean addAll(Collection categories) {
    if (categories == null) { throw new IllegalArgumentException("parameter cannot be null"); }

    return categorySet.addAll(categories);
  }

  public boolean remove(Category category) {
    if (category == null) { throw new IllegalArgumentException("parameter cannot be null"); }

    return categorySet.remove(category);
  }

  public boolean contains(Category category) {
    if (category == null) { throw new IllegalArgumentException("parameter cannot be null"); }

    return categorySet.contains(category);
  }

  public Category[] list() {
    return (Category[]) categorySet.toArray(new Category[categorySet.size()]);
  }

  public String toString() {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append("\nSELECTED CATEGORIES : ");
    Object[] cats = categorySet.toArray();
    for (int i = 0; i < cats.length; str_buf.append(((Category) cats[i++]).getPath() + " "))
      ;

    return str_buf.toString();
  }

}