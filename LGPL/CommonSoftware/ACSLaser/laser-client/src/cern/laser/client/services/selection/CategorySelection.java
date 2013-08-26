/*
 * $Id: CategorySelection.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.services.selection;
import java.util.Collection;

import cern.laser.client.data.Category;

/** The category selection interface. It is the part of an alarm selection
 * containing the interested categories.
 */
public interface CategorySelection 
{
  public boolean add(Category category);
    /** Adds all of the categories in the specified collection to this selection if they're not already present.
     * @return true if this selection changed as a result of the call
     * @param categories collection whose elements are to be added to this selection
     */    
  public boolean addAll(Collection categories);
  /** Remove a category from the selection.
   * @param category the category to be removed
   * @return true if the selection contained the specified category
   */  
  public boolean remove(Category category);
  /** Check if the category is part of the selection.
   * @param category the category to check
   * @return true iff the category is part of the selection
   */  
  public boolean contains(Category category);
  /** List the selected categories.
   * @return an array containing the selected categories
   */  
  public Category[] list();
}