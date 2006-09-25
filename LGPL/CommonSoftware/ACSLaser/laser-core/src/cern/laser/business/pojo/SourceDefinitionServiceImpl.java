/*
 * $Id: SourceDefinitionServiceImpl.java,v 1.3 2006/09/25 08:52:36 acaproni Exp $
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
import cern.laser.business.dao.AdminUserDAO;
import cern.laser.business.dao.AlarmDAO;
import cern.laser.business.dao.CategoryDAO;
import cern.laser.business.dao.ResponsiblePersonDAO;
import cern.laser.business.dao.SourceDAO;
import cern.laser.business.data.AdminUser;
import cern.laser.business.data.Alarm;
import cern.laser.business.data.AlarmImpl;
import cern.laser.business.data.Category;
import cern.laser.business.data.CategoryImpl;
import cern.laser.business.data.ResponsiblePerson;
import cern.laser.business.data.Source;
import cern.laser.business.definition.LaserDefinitionDuplicationException;
import cern.laser.business.definition.LaserDefinitionException;
import cern.laser.business.definition.LaserDefinitionNotAllowedException;
import cern.laser.business.definition.LaserDefinitionNotFoundException;
import cern.laser.business.definition.LaserDefinitionNotValidException;
import cern.laser.business.definition.data.AlarmDefinition;
import cern.laser.business.definition.data.CategoryDefinition;
import cern.laser.business.definition.data.SourceDefinition;

/**
 * 
 * 
 * @version $Revision: 1.3 $ $Date: 2006/09/25 08:52:36 $
 * @author Katarina Sigerud
 */
public class SourceDefinitionServiceImpl {
  private static final Logger LOGGER = Logger.getLogger(SourceDefinitionServiceImpl.class.getName());
  private static final String SOURCES_CATEGORY_ROOT_PATH = "CERN.SOURCES";
  private static final String SOURCE_CATEGORY_PATH_PREFIX = "CERN.SOURCES.";
  private static final AlarmDefinition SOURCE_SURVEILLANCE_ALARM_DEFINITION = new AlarmDefinition("SURVEILLANCE",
      "<FAULT MEMBER>", new Integer(1), "SURVEILLANCE", "<IDENTIFIER>", "Not connected", new Integer(1),
      "Backup not received", "Contact source responsible person", "Source status not guaranteed", Boolean.FALSE, null,
      "LASER", null, null, null, null, null, new Integer(0), null, null);

  private AdminUserDAO adminUserDAO;
  private AlarmDAO alarmDAO;
  private CategoryDAO categoryDAO;
  private ResponsiblePersonDAO responsiblePersonDAO;
  private SourceDAO sourceDAO;
  private AlarmCache alarmCache;
  private AlarmDefinitionServiceImpl alarmDefinitionService;
  private AdminUserDefinitionServiceImpl adminUserDefinitionService;

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  public void setAdminUserDAO(AdminUserDAO adminUserDAO) {
    this.adminUserDAO = adminUserDAO;
  }

  public void setResponsiblePersonDAO(ResponsiblePersonDAO responsiblePersonDAO) {
    this.responsiblePersonDAO = responsiblePersonDAO;
  }

  public void setCategoryDAO(CategoryDAO categoryDAO) {
    this.categoryDAO = categoryDAO;
  }

  public void setSourceDAO(SourceDAO sourceDAO) {
    this.sourceDAO = sourceDAO;
  }

  public void setAlarmDAO(AlarmDAO alarmDAO) {
    this.alarmDAO = alarmDAO;
  }

  public void setAlarmCache(AlarmCache alarmCache) {
    this.alarmCache = alarmCache;
  }
  
  public void setAlarmDefinitionService(AlarmDefinitionServiceImpl alarmDefinitionService) {
    this.alarmDefinitionService = alarmDefinitionService;
  }

  public SourceDefinition getSourceDefinition(String sourceId) throws LaserDefinitionException {
    if (sourceId == null) throw new LaserDefinitionNotValidException("source id is null");

    Source source = sourceDAO.getSource(sourceId);

    if (source == null) { throw new LaserDefinitionNotFoundException("unable to find source " + sourceId); }
    return source.getDefinition();
    //      new SourceDefinition(source.getName(), source.getDescription(), source.getHostName(), source
    //          .getConnectionTimeout(), source.getResponsiblePerson().getResponsibleId());
  }

  public Collection getSourceDefinitions(String userId) throws LaserDefinitionException {
    LOGGER.info("getting administered sources for user : " + userId);

    AdminUser admin_user = adminUserDAO.findAdminUser(userId);
    
    String[] sources = adminUserDAO.getAdministeredSources(userId);
    Collection result = new ArrayList(sources.length);
    for (int i = 0; i < sources.length; i++) {
      result.add(getSourceDefinition(sources[i]));
    }
    LOGGER.info("found " + result.size() + " administered sources");

    return result;
  }

  public void createSource(String userId, SourceDefinition sourceDefinition) throws LaserDefinitionException {
    if (sourceDefinition == null) { throw new LaserDefinitionNotValidException("source is null"); }

    Source source = sourceDAO.getSource(sourceDefinition.getSourceId());
    if (source != null) {
      throw new LaserDefinitionDuplicationException("source "+sourceDefinition.getSourceId()+" already exist");
    }
    
    if (sourceDefinition.getResponsiblePersonId() == null) { throw new LaserDefinitionNotValidException(
        "responsible id for the source definition is null"); }
    ResponsiblePerson responsible = responsiblePersonDAO.getResponsiblePerson(sourceDefinition.getResponsiblePersonId());
    if (responsible == null) { throw new LaserDefinitionNotValidException(
    "responsible with id "+sourceDefinition.getResponsiblePersonId()+" does not exist"); }

    AdminUser admin_user = adminUserDAO.findAdminUser(userId);
    AdminUser laser_user = adminUserDAO.findByLaserAdminUser();

    LOGGER.info("user " + admin_user.getName() + " creating source : " + sourceDefinition);
    // create the source
    source = new Source(sourceDefinition, responsible);

    CategoryDefinition category_definition = new CategoryDefinition(SOURCE_CATEGORY_PATH_PREFIX
        + sourceDefinition.getName(), "Category for source " + sourceDefinition.getName());
    Category parent_category = categoryDAO.findCategoryByPath(category_definition.getParentPath());
    Category category = new CategoryImpl(category_definition);
    parent_category.addChildCategory(category);
    categoryDAO.saveCategory(category);

    admin_user.addAdministeredCategory(category);
    laser_user.addAdministeredCategory(category);
    if (LOGGER.isDebugEnabled())
        LOGGER.debug("default category " + category_definition.getPath() + "created for source "
            + sourceDefinition.getName());

    source.setSurveillanceAlarmId(alarmDAO.findLaserSurveillanceAlarmId());
    sourceDAO.saveSource(source);

    // create the source surveillance alarm
    AlarmDefinition surveillance_alarm_definition = (AlarmDefinition) SOURCE_SURVEILLANCE_ALARM_DEFINITION.clone();
    surveillance_alarm_definition.setFaultMember(sourceDefinition.getName());
    surveillance_alarm_definition.setIdentifier(sourceDefinition.getName());
    Alarm surveillance_alarm = new AlarmImpl(surveillance_alarm_definition, source, responsible);

    Category surveillance_category = categoryDAO.findBySurveillanceCategory();
    surveillance_category.addAlarm(surveillance_alarm);
    category.addAlarm(surveillance_alarm);
    
    alarmDAO.saveAlarm(surveillance_alarm);
    categoryDAO.updateCategory(surveillance_category);
    categoryDAO.updateCategory(category);
    if (LOGGER.isDebugEnabled()) LOGGER.debug("surveillance alarm created for source " + sourceDefinition.getName());

    source.setSurveillanceAlarmId(surveillance_alarm.getAlarmId());
    sourceDAO.updateSource(source);
    
    admin_user.addAdministeredSource(source);
    laser_user.addAdministeredSource(source);

    adminUserDAO.updateAdminUser(admin_user);
    adminUserDAO.updateAdminUser(laser_user);
    LOGGER.info("source created");
  }

  public void createSources(String userId, Collection sources) throws LaserDefinitionException {
    if ((sources == null) || (sources.size() == 0)) { return; }
    LOGGER.info("creating " + sources.size() + " sources");
    Iterator iterator = sources.iterator();

    while (iterator.hasNext()) {
      createSource(userId, (SourceDefinition) iterator.next());
    }
    LOGGER.info("sources created");
  }

  public void updateSource(String userId, SourceDefinition sourceDefinition) throws LaserDefinitionException {
    if (sourceDefinition == null) { throw new LaserDefinitionNotValidException("source is null"); }
    Source source = null;

    source = sourceDAO.findSource(sourceDefinition.getSourceId());
    if (!sourceDefinition.getName().equals(source.getName())) { throw new LaserDefinitionNotValidException(
        "source name cannot be changed"); }
    AdminUser admin_user = adminUserDAO.findAdminUser(userId);

    if (!admin_user.administersSource(source)) { throw new LaserDefinitionNotAllowedException(
        "not an administrator for source : " + sourceDefinition); }
    try {
      LOGGER.info("user " + admin_user.getName() + " updating source : " + sourceDefinition);

      source.setDefinition(sourceDefinition);
      sourceDAO.updateSource(source);

      String[] alarms = sourceDAO.getAlarms(source.getSourceId());
      for (int i = 0; i < alarms.length; i++) {
        try {
          alarmCache.invalidate(alarms[i]);
        } catch (Exception e) {
          LOGGER.error("unable to propagate alarm source update", e);
        }
      }
      LOGGER.info("source updated");
    } catch (Exception e) {
      throw new LaserDefinitionException("unable to update source " + sourceDefinition + " : " + e.getMessage(), e);
    }
  }

  public void updateSources(String userId, Collection sources) throws LaserDefinitionException {
    if ((sources == null) || (sources.size() == 0)) { return; }
    LOGGER.info("updating " + sources.size() + " sources");
    Iterator iterator = sources.iterator();

    while (iterator.hasNext()) {
      updateSource(userId, (SourceDefinition) iterator.next());
    }
    LOGGER.info("sources updated");
  }

  public void removeSource(String userId, SourceDefinition sourceDefinition) throws LaserDefinitionException {
    if (sourceDefinition == null) { throw new LaserDefinitionNotValidException("source is null"); }
    AdminUser admin_user = adminUserDAO.findAdminUser(userId);
    AdminUser laser_user = adminUserDAO.findByLaserAdminUser();
    Source source = sourceDAO.findSource(sourceDefinition.getSourceId());

    if (!admin_user.administersSource(source)) { throw new LaserDefinitionNotAllowedException(
        "not in source administrators"); }
    if (sourceDAO.getAlarms(sourceDefinition.getSourceId()).length > 1) { throw new LaserDefinitionNotAllowedException(
        "source has attached alarms"); }
    try {
      LOGGER.info("user " + admin_user.getName() + " removing source : " + sourceDefinition);
      alarmDefinitionService.removeAlarm(adminUserDAO.findByLaserAdminUser().getUserId(), alarmDAO.findAlarm(
          source.getSurveillanceAlarmId()).getDefinition());

      admin_user.removeAdministeredSource(source);
      laser_user.removeAdministeredSource(source);
      sourceDAO.deleteSource(source);
      source = null;

      String category_path = new String(SOURCE_CATEGORY_PATH_PREFIX + sourceDefinition.getName());
      Category parent_category = categoryDAO.findCategoryByPath(SOURCES_CATEGORY_ROOT_PATH);
      Category category = categoryDAO.findCategoryByPath(category_path);
      parent_category.removeChildCategory(category);
      
      admin_user.removeAdministeredCategory(category);
      laser_user.removeAdministeredCategory(category);
      categoryDAO.deleteCategory(category);
      if (LOGGER.isDebugEnabled())
          LOGGER.debug("removed default category " + category_path + " for source " + sourceDefinition.getName());
      
      adminUserDAO.updateAdminUser(admin_user);
      adminUserDAO.updateAdminUser(laser_user);
      LOGGER.info("removed source " + sourceDefinition.getName());
    } catch (Exception e) {
      throw new LaserDefinitionException("unable to remove source " + sourceDefinition + " : " + e.getMessage(), e);
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.DefinitionServiceSessionEJB#removeSources(java.lang.Integer, java.util.Collection)
   */
  public void removeSources(String userId, Collection sources) throws LaserDefinitionException {
    if ((sources == null) || (sources.size() == 0)) { return; }
    LOGGER.info("removing " + sources.size() + " sources");
    Iterator iterator = sources.iterator();

    while (iterator.hasNext()) {
      removeSource(userId, (SourceDefinition) iterator.next());
    }
    LOGGER.info("sources removed");
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.DefinitionServiceSessionEJB#uploadSources(java.lang.Integer, java.util.Collection,
   *      java.util.Collection, java.util.Collection)
   */
  public void uploadSources(String userId, Collection toBeCreated, Collection toBeUpdated, Collection toBeRemoved)
      throws LaserDefinitionException {
    LOGGER.info("uploading source definitions : [userId=" + userId + ", toBeCreated="
        + ((toBeCreated == null) ? 0 : toBeCreated.size()) + ", toBeUpdated="
        + ((toBeUpdated == null) ? 0 : toBeUpdated.size()) + ", toBeRemoved="
        + ((toBeRemoved == null) ? 0 : toBeRemoved.size()) + "]");
    createSources(userId, toBeCreated);
    updateSources(userId, toBeUpdated);
    removeSources(userId, toBeRemoved);
    LOGGER.info("source definitions uploaded");
  }

  //
  // -- PRIVATE METHODS ----------------------------------------------
  //
}