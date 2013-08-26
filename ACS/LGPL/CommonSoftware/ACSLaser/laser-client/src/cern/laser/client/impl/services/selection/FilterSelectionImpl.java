/*
 * $Id: FilterSelectionImpl.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
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
import java.util.Iterator;
import java.util.Set;

import cern.laser.client.services.selection.Filter;
import cern.laser.client.services.selection.FilterSelection;

public class FilterSelectionImpl implements FilterSelection, Serializable {
  private Set filterSet = null;

  public FilterSelectionImpl() {
    filterSet = new HashSet();
  }

  public boolean add(Filter filter) {
    if (filter == null) { throw new IllegalArgumentException("parameter can not be null"); }

    return filterSet.add(filter);
  }

  public boolean addAll(Collection filters) {
    if (filters == null) { throw new IllegalArgumentException("parameter can not be null"); }

    return filterSet.addAll(filters);
  }

  public boolean remove(Filter filter) {
    if (filter == null) { throw new IllegalArgumentException("parameter can not be null"); }

    return filterSet.remove(filter);
  }

  public boolean contains(Filter filter) {
    if (filter == null) { throw new IllegalArgumentException("parameter can not be null"); }

    return filterSet.contains(filter);
  }

  public Filter[] list() {
    return (Filter[]) filterSet.toArray(new Filter[filterSet.size()]);
  }

  public String toSQLString() {
    if ((filterSet == null) || (filterSet.size() == 0)) { return null; }
    StringBuffer str = new StringBuffer();
    Iterator iterator = filterSet.iterator();
    while (iterator.hasNext()) {
      FilterImpl filter = (FilterImpl) iterator.next();
      str.append(filter.getProperty());
      str.append(" ");
      str.append(filter.getOperator());
      str.append(" ");
      if (filter.isString()) {
        str.append("'");
        str.append(filter.getValue());
        str.append("'");
      } else {
        str.append(filter.getValue());
      }
      if (iterator.hasNext()) {
        str.append(" AND ");
      }
    }

    return str.toString();
  }

  public String toString() {
    StringBuffer str_buf = new StringBuffer();
    str_buf.append("\nSELECTED FILTERS : ");
    Object[] fs = filterSet.toArray();
    for (int i = 0; i < fs.length; str_buf.append(fs[i++] + " "))
      ;

    return str_buf.toString();
  }

}