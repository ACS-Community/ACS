/*
 * $Id: AdminUser.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.data;

import java.util.HashSet;
import java.util.Set;

/**
 * 
 * 
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 * @author Katarina Sigerud
 * @hibernate.subclass name="AdminUser" discriminator-value="true"
 */
public class AdminUser extends ConsoleUser {
  public Set administeredSourceIds;
  public Set administeredCategoryIds;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //
  
  private AdminUser() {
    super();
  }

  public AdminUser(String name, String password) {
      super(name, password);
  }

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  public void addAdministeredCategory(Category category) {
    getAdministeredCategoryIds().add(category.getCategoryId());
  }
  
  public void removeAdministeredCategory(Category category) {
    getAdministeredCategoryIds().remove(category.getCategoryId());
  }
  
  public boolean administersCategory(Integer categoryId) {
    return getAdministeredCategoryIds().contains(categoryId);
  }
  
   public void addAdministeredSource(Source source) {
     getAdministeredSourceIds().add(source.getSourceId());
   }
   
   public void removeAdministeredSource(Source source) {
     getAdministeredSourceIds().remove(source.getSourceId());
   }
   
   public boolean administersSource(Source source) {
     return getAdministeredSourceIds().contains(source.getSourceId());
   }

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //

  /**
   * @hibernate.set name="getAdministeredCategoryIds" table="CATEGORY_ADMIN_USER" 
   *                cascade="save-update" inverse="false" lazy="true" outer-join="false"
   * @hibernate.collection-key column="USER_ID"
   * @hibernate.collection-element column="CATEGORY_ID" type="integer"
 */
  private Set getAdministeredCategoryIds() {
    return administeredCategoryIds == null ? new HashSet(0) : administeredCategoryIds;
  }

  /**
   * @param administeredCategoryIds The administeredCategoryIds to set.
   */
  private void setAdministeredCategoryIds(Set newAdministeredCategoryIds) {
    administeredCategoryIds = newAdministeredCategoryIds;
  }

  /**
   * 
   * @hibernate.set name="getAdministeredSourceIds" table="SOURCE_ADMIN_USER" 
   *                cascade="save-update" inverse="false" lazy="true" outer-join="false"
   * @hibernate.collection-key column="USER_ID"
   * @hibernate.collection-element column="SOURCE_ID" type="string"
 */
  private Set getAdministeredSourceIds() {
    return administeredSourceIds == null ? new HashSet(0) : administeredSourceIds;
  }

  /**
   * @param administeredSourceIds The administeredSourceIds to set.
   */
  private void setAdministeredSourceIds(Set newAdministeredSourceIds) {
    administeredSourceIds = newAdministeredSourceIds;
  }
  }