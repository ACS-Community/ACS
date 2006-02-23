/*
 * $Id: Selection.java,v 1.1 2005/06/06 18:19:40 kzagar Exp $
 *
 * $Date: 2005/06/06 18:19:40 $ 
 * $Revision: 1.1 $ 
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.services.selection;


/** The alarm selection container class. It is made of a {@link CategorySelection}
 * and a {@link FilterSelection} part.
 */
public interface Selection
{
    /** Factory method. Create an empty category selection.
     * @return a new CategorySelection instance
     */    
  public CategorySelection createCategorySelection();

  /** Factory method. Create an empty filter selection.
   * @return a new filter selection instance
   */  
  public FilterSelection createFilterSelection();
  
  /** Accessor method.
   * @return the alarm categories selection
   */  
  public CategorySelection getCategorySelection();

  /** Accessor method.
   * @param newCategories the alarm categories selection
   */  
  public void setCategorySelection(CategorySelection newCategories);

  /** Accessor method.
   * @return the alarm property filters selection
   */  
  public FilterSelection getFilterSelection();

  /** Accessor method.
   * @param newFilters the alarm property filters selection
   */  
  public void setFilterSelection(FilterSelection newFilters);

  /** Accessor method.
   * @return the reduced&masked selection flag
   */  
  public boolean getReducedMaskedSelection();

  /** Accessor method.
   * @param newReducedMasked the reduced&masked selection flag. If set to TRUE, 
   * only the changes resulting from reduction
   * and masking analysis are distributed to the client, filtering
   * out all the changes being reduced or masked.
   */  
  public void setReducedMaskedSelection(boolean newReducedMasked);
  
}