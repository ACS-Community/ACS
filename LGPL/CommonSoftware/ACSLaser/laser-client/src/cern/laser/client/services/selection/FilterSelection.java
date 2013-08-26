/*
 * $Id: FilterSelection.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.services.selection;
import java.util.Collection;

/** The filters selection interface. It is the part of an alarm selection
 * containing the defined alarm property filters.
 */
public interface FilterSelection 
{
    /** Add a new filter.
     * @param filter the filter to add
     * @return true if this selection did not already contain the specified filter
     */    
  public boolean add(Filter filter);
  
    /** Adds all of the filters in the specified collection to this selection if they're not already present.
     * @return true if this selection changed as a result of the call
     * @param filters collection whose elements are to be added to this selection
     */    
  public boolean addAll(Collection filters);

  /** Remove a filter.
   * @param filter the filter to be removed
   * @return true if the selection contained the specified filter
   */  
  public boolean remove(Filter filter);
  
  /** Check if the filter is already defined.
   * @param filter the filter to check
   * @return true iff the filter is part of the selection
   */  
  public boolean contains(Filter filter);
  
  /** List the defined filters.
   * @return an array containing the defined alarm property filters
   */  
  public Filter[] list();

  /** Return the SQL-like representation of the filters selection.
   * @return the SQL string representation
   */  
  public String toSQLString();

}