/*
 * $Id: CategoryDefinitionServiceImpl.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
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
import java.util.Iterator;

import org.apache.log4j.Logger;

import cern.laser.business.cache.AlarmCache;
import cern.laser.business.cache.AlarmCacheException;
import cern.laser.business.dao.AdminUserDAO;
import cern.laser.business.dao.AlarmDAO;
import cern.laser.business.dao.CategoryDAO;
import cern.laser.business.data.AdminUser;
import cern.laser.business.data.Alarm;
import cern.laser.business.data.Category;
import cern.laser.business.data.CategoryImpl;
import cern.laser.business.definition.LaserDefinitionDuplicationException;
import cern.laser.business.definition.LaserDefinitionException;
import cern.laser.business.definition.LaserDefinitionNotAllowedException;
import cern.laser.business.definition.LaserDefinitionNotFoundException;
import cern.laser.business.definition.LaserDefinitionNotValidException;
import cern.laser.business.definition.data.CategoryDefinition;
import cern.laser.business.definition.data.CategoryLink;

/**
 * 
 * 
 * @version $Revision: 1.3 $ $Date: 2006/09/25 08:52:36 $
 * @author Katarina Sigerud
 */
public class CategoryDefinitionServiceImpl {
  private static final Logger LOGGER = Logger.getLogger(CategoryDefinitionServiceImpl.class.getName());
  private static final String SOURCE_CATEGORY_PATH_PREFIX = "CERN.SOURCES.";

  private AdminUserDAO adminUserDAO;
  private AlarmDAO alarmDAO;
  private CategoryDAO categoryDAO;
  private AlarmCache alarmCache;
  private AdminUserDefinitionServiceImpl adminUserDefinitionService;

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  public void setAdminUserDAO(AdminUserDAO adminUserDAO) {
    this.adminUserDAO = adminUserDAO;
  }

  public void setCategoryDAO(CategoryDAO categoryDAO) {
    this.categoryDAO = categoryDAO;
  }

  public void setAlarmDAO(AlarmDAO alarmDAO) {
    this.alarmDAO = alarmDAO;
  }

  public void setAlarmCache(AlarmCache alarmCache) {
    this.alarmCache = alarmCache;
  }

  public CategoryDefinition getCategoryDefinition(Integer categoryId) throws LaserDefinitionException {
    if (categoryId == null) throw new LaserDefinitionNotValidException("category id is null");

    Category category = categoryDAO.getCategory(categoryId);

    if (category == null) { throw new LaserDefinitionNotFoundException("unable to find category " + categoryId); }
    return new CategoryDefinition(category.getPath(), category.getDescription());
  }

  public CategoryDefinition getCategoryDefinition(String path) throws LaserDefinitionException {
    if (path == null) throw new LaserDefinitionNotValidException("category path is null");

    Category category = categoryDAO.getCategoryByPath(path);

    if (category == null) { throw new LaserDefinitionNotFoundException("unable to find category with path " + path); }
    return new CategoryDefinition(category.getPath(), category.getDescription());
  }

  public Collection getCategoryDefinitions(String userId) throws LaserDefinitionException {
    AdminUser admin_user = adminUserDAO.findAdminUser(userId);

    LOGGER.info("getting administered categories for user : " + admin_user.getName());

    Integer[] administered_categories = adminUserDAO.getAdministeredCategories(userId);
    Collection result = new ArrayList(administered_categories.length);
    for (int i = 0; i < administered_categories.length; i++) {
      result.add(getCategoryDefinition(administered_categories[i]));
    }
    LOGGER.info("found " + result.size() + " administered categories");

    return result;
  }

  public void createCategory(String userId, CategoryDefinition categoryDefinition) throws LaserDefinitionException {
    if (categoryDefinition == null) { throw new LaserDefinitionNotValidException("category is null"); }

    Category category = categoryDAO.getCategoryByPath(categoryDefinition.getPath());
    if (category != null) { throw new LaserDefinitionDuplicationException("category with path "
        + categoryDefinition.getPath() + " already exist"); }

    AdminUser admin_user = adminUserDAO.findAdminUser(userId);
    AdminUser laser_user = adminUserDAO.findByLaserAdminUser();

    Category parent_category = categoryDAO.getCategoryByPath(categoryDefinition.getParentPath());
    if (parent_category == null) { throw new LaserDefinitionNotFoundException("parent category with path "
        + categoryDefinition.getParentPath() + " does not exist"); }
    if (!admin_user.administersCategory(parent_category.getCategoryId())) { throw new LaserDefinitionNotAllowedException(
        admin_user.getName() + " not an administrator for the parent of category : " + categoryDefinition); }

    LOGGER.info("user " + admin_user.getName() + " creating category : " + categoryDefinition);
    category = new CategoryImpl(categoryDefinition);
    parent_category.addChildCategory(category);

    categoryDAO.saveCategory(category);

    admin_user.addAdministeredCategory(category);
    adminUserDAO.updateAdminUser(admin_user);

    laser_user.addAdministeredCategory(category);
    adminUserDAO.updateAdminUser(laser_user);
    LOGGER.info("category created");
  }

  public void createCategories(String userId, Collection categories) throws LaserDefinitionException {
    if ((categories == null) || (categories.size() == 0)) { return; }
    LOGGER.info("creating " + categories.size() + " categories");

    Iterator iterator = categories.iterator();
    while (iterator.hasNext()) {
      createCategory(userId, (CategoryDefinition) iterator.next());
    }
    LOGGER.info("categories created");
  }

  public void updateCategory(String userId, CategoryDefinition categoryDefinition) throws LaserDefinitionException {
    if (categoryDefinition == null) { throw new LaserDefinitionNotValidException("category is null"); }

    AdminUser admin_user = adminUserDAO.findAdminUser(userId);

    Category category = categoryDAO.findCategoryByPath(categoryDefinition.getPath());
    if (!admin_user.administersCategory(category.getCategoryId())) { throw new LaserDefinitionNotAllowedException(
        "not in category administrators : " + categoryDefinition); }

    try {
      LOGGER.info("user " + admin_user.getName() + " updating category : " + categoryDefinition);

      Category old_parent_category = categoryDAO.findCategoryByPath(categoryDefinition.getParentPath());
      if (!category.getParentId().equals(old_parent_category.getCategoryId())) {
        if (!admin_user.administersCategory(old_parent_category.getCategoryId())
            || !admin_user.administersCategory(category.getParentId())) { throw new LaserDefinitionNotAllowedException(
            "not in category parent administrators : " + categoryDefinition); }

        old_parent_category.removeChildCategory(category);

        Category new_parent_category = categoryDAO.findCategoryByPath(categoryDefinition.getParentPath());
        new_parent_category.addChildCategory(category);
      }
      category.setDefinition(categoryDefinition);

      categoryDAO.updateCategory(category);

      LOGGER.info("category updated");
    } catch (Exception e) {
      throw new LaserDefinitionException("unable to update category " + categoryDefinition + " : " + e.getMessage(), e);
    }
  }

  public void updateCategories(String userId, Collection categories) throws LaserDefinitionException {
    if ((categories == null) || (categories.size() == 0)) { return; }
    LOGGER.info("updating " + categories.size() + " categories");
    Iterator iterator = categories.iterator();

    while (iterator.hasNext()) {
      updateCategory(userId, (CategoryDefinition) iterator.next());
    }
    LOGGER.info("categories updated");
  }

  public void removeCategory(String userId, CategoryDefinition categoryDefinition) throws LaserDefinitionException {
    if (categoryDefinition == null) { throw new LaserDefinitionNotValidException("category is null"); }
    Category category = categoryDAO.getCategoryByPath(categoryDefinition.getPath());
    if (category == null) {
      throw new LaserDefinitionNotFoundException("category with path "+categoryDefinition.getPath()+" does not exist");
    }      

    AdminUser admin_user = adminUserDAO.findAdminUser(userId);
    AdminUser laser_user = adminUserDAO.findByLaserAdminUser();

    if (!admin_user.administersCategory(category.getCategoryId())) { throw new LaserDefinitionNotAllowedException(
        "not in category administrators : " + categoryDefinition); }
    if (categoryDAO.getAlarms(category.getCategoryId()).length != 0) { throw new LaserDefinitionNotAllowedException(
        "category has attached alarms"); }
    try {
      LOGGER.info("user " + admin_user.getName() + " removing category : " + categoryDefinition);
      admin_user.removeAdministeredCategory(category);
      laser_user.removeAdministeredCategory(category);

      categoryDAO.deleteCategory(category);
      adminUserDAO.updateAdminUser(admin_user);
      adminUserDAO.updateAdminUser(laser_user);
      LOGGER.info("category removed");
    } catch (Exception e) {
      throw new LaserDefinitionException("unable to remove category " + categoryDefinition + " : " + e.getMessage(), e);
    }
  }

  public void removeCategories(String userId, Collection categories) throws LaserDefinitionException {
    if ((categories == null) || (categories.size() == 0)) { return; }
    LOGGER.info("removing " + categories.size() + " categories");
    Iterator iterator = categories.iterator();

    while (iterator.hasNext()) {
      removeCategory(userId, (CategoryDefinition) iterator.next());
    }
    LOGGER.info("caregories removed");
  }

  public void createCategoryLink(String userId, CategoryLink link) throws LaserDefinitionException {
    if (link == null) { throw new LaserDefinitionNotValidException("category/alarm link is null"); }
    if (link.getCategory() == null) { throw new LaserDefinitionNotValidException(
        "malformed category/alarm link: category is null"); }
    if (link.getAlarm() == null) { throw new LaserDefinitionNotValidException(
        "malformed category/alarm link: alarm is null"); }

    AdminUser admin_user = adminUserDAO.findAdminUser(userId);

    Category category = categoryDAO.getCategoryByPath(link.getCategory().getPath());
    if (category == null) {
      throw new LaserDefinitionNotFoundException("category with path "+link.getCategory().getPath()+" does not exists");
    }
    if (!admin_user.administersCategory(category.getCategoryId())) { throw new LaserDefinitionNotAllowedException(
        "not an administrators for the category : " + link.getCategory()); }

    Alarm alarm = alarmDAO.getAlarm(link.getAlarm().getAlarmId());
    if (alarm == null) {
      throw new LaserDefinitionNotFoundException("alarm with id "+link.getAlarm().getAlarmId()+" does not exist");
    }
    Alarm surveillance_alarm = alarmDAO.findAlarm(alarm.getSource().getSurveillanceAlarmId());
    if (category.containsAlarm(alarm)) { throw new LaserDefinitionDuplicationException(
        "alarm/category link already defined : " + link); }

    LOGGER.info("user " + admin_user.getName() + " creating alarm/category link : " + link);
    category.addAlarm(alarm);
    category.addAlarm(surveillance_alarm);
    categoryDAO.updateCategory(category);

    try {
      alarmCache.invalidate(alarm.getAlarmId());
      alarmCache.invalidate(surveillance_alarm.getAlarmId());
    } catch (AlarmCacheException e) {
      LOGGER.error("unable to propagate category link : " + link, e);
    }
    LOGGER.info("link created");
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.DefinitionServiceSessionEJB#createCategoryLinks(java.lang.Integer,
   *      java.util.Collection)
   */
  public void createCategoryLinks(String userId, Collection categoryLinks) throws LaserDefinitionException {
    if ((categoryLinks == null) || (categoryLinks.size() == 0)) { return; }
    LOGGER.info("creating " + categoryLinks.size() + " category links");

    Iterator iterator = categoryLinks.iterator();
    while (iterator.hasNext()) {
      createCategoryLink(userId, (CategoryLink) iterator.next());
    }
    LOGGER.info("category links created");
  }

  public void removeCategoryLink(String userId, CategoryLink link) throws LaserDefinitionException {
    if (link == null) { throw new LaserDefinitionNotValidException(
        "category/alarm link is null"); }
    if (link.getCategory() == null) { throw new LaserDefinitionNotValidException(
        "malformed category/alarm link: category is null"); }
    if (link.getAlarm() == null) { throw new LaserDefinitionNotValidException(
        "malformed category/alarm link: alarm is null"); }
    
    AdminUser admin_user = adminUserDAO.findAdminUser(userId);

    Category category = categoryDAO.getCategoryByPath(link.getCategory().getPath());
    if (category == null) {
      throw new LaserDefinitionNotFoundException("category with path "+link.getCategory().getPath()+" does not exist");
    }
    if (!admin_user.administersCategory(category.getCategoryId())) { throw new LaserDefinitionNotAllowedException(
        "not an administrators for the category : " + link.getCategory()); }

    Alarm alarm = alarmDAO.getAlarm(link.getAlarm().getAlarmId());
    if (alarm == null) {
      throw new LaserDefinitionNotFoundException("alarm "+link.getAlarm().getAlarmId()+" does not exist");
    }
    if (!category.containsAlarm(alarm)) { throw new LaserDefinitionNotFoundException(
        "category/alarm link not defined : " + link); }

    try {
      LOGGER.info("user " + admin_user.getName() + " removing category link : " + link);
      category.removeAlarm(alarm);
    categoryDAO.flushCategory();
      alarmCache.invalidate(alarm.getAlarmId());

//      Alarm surveillance_alarm = alarmDAO.findAlarm(alarm.getSource().getSurveillanceAlarmId());
//      if (!categoryDAO.hasAlarmsForSource(category.getCategoryId(), alarm.getSource().getSourceId())) {
//        category.removeAlarm(surveillance_alarm);
//        alarmCache.invalidate(surveillance_alarm.getAlarmId());
//      }
//      categoryDAO.updateCategory(category);

    } catch (AlarmCacheException e) {
      LOGGER.error("unable to propagate category/alarm link : " + link, e);
    }
    LOGGER.info("category/alarm link removed");
  }

  public void removeCategoryLinks(String userId, Collection categoryLinks) throws LaserDefinitionException {
    if ((categoryLinks == null) || (categoryLinks.size() == 0)) { return; }
    LOGGER.info("removing " + categoryLinks.size() + " category/alarm links");
    Iterator iterator = categoryLinks.iterator();

    while (iterator.hasNext()) {
      removeCategoryLink(userId, (CategoryLink) iterator.next());
    }
    LOGGER.info("category/alarm links removed");
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.DefinitionServiceSessionEJB#uploadCategories(java.lang.Integer, java.util.Collection,
   *      java.util.Collection, java.util.Collection)
   */
  public void uploadCategories(String userId, Collection toBeCreated, Collection toBeUpdated, Collection toBeRemoved)
      throws LaserDefinitionException {
    LOGGER.info("uploading category definitions : [userId=" + userId + ", toBeCreated="
        + ((toBeCreated == null) ? 0 : toBeCreated.size()) + ", toBeUpdated="
        + ((toBeUpdated == null) ? 0 : toBeUpdated.size()) + ", toBeRemoved="
        + ((toBeRemoved == null) ? 0 : toBeRemoved.size()) + "]");
    createCategories(userId, toBeCreated);
    updateCategories(userId, toBeUpdated);
    removeCategories(userId, toBeRemoved);
    LOGGER.info("category definitions uploaded");
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.DefinitionServiceSessionEJB#uploadCategoryLinks(java.lang.Integer,
   *      java.util.Collection, java.util.Collection)
   */
  public void uploadCategoryLinks(String userId, Collection toBeCreated, Collection toBeRemoved)
      throws LaserDefinitionException {
    LOGGER.info("uploading alarm/category link definitions : [userId=" + userId + ", toBeCreated="
        + ((toBeCreated == null) ? 0 : toBeCreated.size()) + ", toBeRemoved="
        + ((toBeRemoved == null) ? 0 : toBeRemoved.size()) + "]");
    createCategoryLinks(userId, toBeCreated);
    removeCategoryLinks(userId, toBeRemoved);
    LOGGER.info("alarm/category link definitions uploaded");
  }

  //
  // -- PRIVATE METHODS ----------------------------------------------
  //
}