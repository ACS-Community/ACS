/*
 * $Id: SelectionImpl.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.impl.services.selection;

import java.io.Serializable;

import cern.laser.client.services.selection.CategorySelection;
import cern.laser.client.services.selection.FilterSelection;
import cern.laser.client.services.selection.Selection;

public class SelectionImpl implements Selection, Serializable {
  private CategorySelection categories;
  private FilterSelection filters;
  private boolean reducedMasked;

  public SelectionImpl() {
    categories = new CategorySelectionImpl();
    filters = new FilterSelectionImpl();
    reducedMasked = false;
  }

  public CategorySelection createCategorySelection() {
    return new CategorySelectionImpl();
  }

  public FilterSelection createFilterSelection() {
    return new FilterSelectionImpl();
  }

  public CategorySelection getCategorySelection() {
    return categories;
  }

  public void setCategorySelection(CategorySelection newCategories) {
    categories = newCategories;
  }

  public FilterSelection getFilterSelection() {
    return filters;
  }

  public void setFilterSelection(FilterSelection newFilters) {
    filters = newFilters;
  }

  public boolean getReducedMaskedSelection() {
    return reducedMasked;
  }

  public void setReducedMaskedSelection(boolean newReducedMasked) {
    reducedMasked = newReducedMasked;
  }

  public String toString() {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append(categories);
    str_buf.append(filters);
    str_buf.append("\nREDUCED-MASKED : ");
    str_buf.append(reducedMasked);

    return str_buf.toString();
  }

}