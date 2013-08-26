/*
 * $Id: AdminUserDefinitionServiceImpl.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.3 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.pojo;

import java.util.ArrayList;
import java.util.Collection;

import org.apache.log4j.Logger;

import cern.laser.business.dao.AdminUserDAO;
import cern.laser.business.dao.CategoryDAO;
import cern.laser.business.data.AdminUser;
import cern.laser.business.definition.LaserDefinitionDuplicationException;
import cern.laser.business.definition.LaserDefinitionException;
import cern.laser.business.definition.LaserDefinitionNotFoundException;

/**
 * 
 * 
 * @version $Revision: 1.3 $ $Date: 2006/09/25 08:52:36 $
 * @author Katarina Sigerud
 */
public class AdminUserDefinitionServiceImpl {
  private static final Logger LOGGER = Logger.getLogger(AdminUserDefinitionServiceImpl.class.getName());
  private static final String SOURCE_CATEGORY_PATH_PREFIX = "CERN.SOURCES.";

  private CategoryDAO categoryDAO;
  private AdminUserDAO adminUserDAO;

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  public void setCategoryDAO(CategoryDAO categoryDAO) {
    this.categoryDAO = categoryDAO;
  }

  public void setAdminUserDAO(AdminUserDAO adminUserDAO) {
    this.adminUserDAO = adminUserDAO;
  }

  public String loginAdminUser(String name, String password) {
    AdminUser admin_user = adminUserDAO.findAdminUserByNamePassword(name, password);

    return admin_user.getUserId();
  }

  //  public AdminUser findAdminUser(Integer userId) throws LaserDefinitionException {
  //    if (userId == null) throw new LaserDefinitionNotAllowedException("userId is null");
  //
  //       AdminUser admin_user = adminUserDAO.getAdminUser(userId);
  //       if (admin_user == null) {
  //         throw new LaserDefinitionNotFoundException("unable to find admin user "+userId);
  //       }
  //
  //       return admin_user;
  //  }

  public Collection getAdminUsers() throws LaserDefinitionException {
    try {
      AdminUser[] users = adminUserDAO.findAllAdminUsers();
      Collection result = new ArrayList(users.length);

      for (int i = 0; i < users.length; i++) {
        result.add(users[i].getUserId());
      }

      return result;
    } catch (Exception e) {
      throw new LaserDefinitionException("unable to get admin users", e);
    }
  }

  public String createAdminUser(String name, String password) throws LaserDefinitionException {
    LOGGER.info("creating admin user : " + name + "/" + password);

    AdminUser admin_user = adminUserDAO.getAdminUserByName(name);
    if (admin_user != null) { throw new LaserDefinitionDuplicationException("a user " + name + " already exists"); }

    admin_user = new AdminUser(name, password);
    admin_user.addAdministeredCategory(categoryDAO.findByCategoryTreeRoot());

    adminUserDAO.saveAdminUser(admin_user);

    LOGGER.info("admin user " + name + "/" + password + " created");
    return admin_user.getUserId();
  }

  public void removeAdminUser(String name) throws LaserDefinitionException {
    LOGGER.info("removing admin user : " + name);
    AdminUser admin_user = adminUserDAO.getAdminUserByName(name);
    if (admin_user == null) { throw new LaserDefinitionNotFoundException("a user " + name + " does not exists"); }

    adminUserDAO.deleteAdminUser(admin_user);
    admin_user = null;
    LOGGER.info("user " + name + " removed");
  }

  //
  // -- PRIVATE METHODS ----------------------------------------------
  //
}