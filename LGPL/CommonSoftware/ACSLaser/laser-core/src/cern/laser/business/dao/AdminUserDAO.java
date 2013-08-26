/*
 * $Id: AdminUserDAO.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.3 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.dao;

import cern.laser.business.data.AdminUser;

/**
 * 
 * 
 * @version $Revision: 1.3 $ $Date: 2006/09/25 08:52:36 $
 * @author Klemen Zagar
 */
public interface AdminUserDAO {
  //public void setLaserUserId(String laserUserId);

  public AdminUser findAdminUser(String identifier);
  
  //public AdminUser getAdminUser(String userId);

  public AdminUser findByLaserAdminUser();
  
  public AdminUser findAdminUserByNamePassword(String name, String password);
  
  //public AdminUser findAdminUserByName(String name);

  public AdminUser getAdminUserByName(String name);
 
  //public AdminUser findAdminUserSourcesInitialized(Integer userId);
  
  public String[] getAdministeredSources(String userId);
  
  public Integer[] getAdministeredCategories(String userId);
  
  public AdminUser[] findAllAdminUsers();
  
  public void saveAdminUser(AdminUser adminUser);
  
  public void deleteAdminUser(AdminUser adminUser);
  
  public void updateAdminUser(AdminUser admin_user);
}