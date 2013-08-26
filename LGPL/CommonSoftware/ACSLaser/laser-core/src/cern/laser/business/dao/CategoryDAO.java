/*
 * $Id: CategoryDAO.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.3 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.dao;

import cern.laser.business.data.Category;

/**
 * 
 * 
 * @version $Revision: 1.3 $ $Date: 2006/09/25 08:52:36 $
 * @author Katarina Sigerud
 */
public interface CategoryDAO {
  //public void setCategoryTreeRoot(String categoryTreeRoot);
  
  //public void setSurveillanceCategoryPath(String surveillanceCategoryPath);
  
  public Category findCategory(Integer identifier);
  
  //public Category findCategoryAlarmsInitialized(Integer identifier);
  
  //public Category findCategoryByPathInitialized(String path);
  
  public Category getCategory(Integer identifier);
  
  public Category findByCategoryTreeRoot();

  public Category findBySurveillanceCategory();
  
  //public Category findBySurveillanceCategoryInitialized();
  
  public Category[] findAllCategories();

  public Category findCategoryByPath(String path);
  
  public Category getCategoryByPathInitialized(String path);
  
  public Category getCategoryByPath(String path);
  
  public void saveCategory(Category category);
  
  public void updateCategory(Category category);
  
  public void deleteCategory(Category category);
  
  public String[] getAlarms(Integer categoryId);
  
  public Integer[] getChildren(Integer parentId);
  
  //public boolean hasAlarmsForSource(Integer categoryId, String sourceId);
  
  public void flushCategory();
}