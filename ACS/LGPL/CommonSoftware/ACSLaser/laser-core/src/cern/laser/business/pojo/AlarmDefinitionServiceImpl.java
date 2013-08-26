/*
 * $Id: AlarmDefinitionServiceImpl.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.pojo;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.apache.log4j.Logger;

import cern.laser.business.LaserRuntimeException;
import cern.laser.business.cache.AlarmCache;
import cern.laser.business.cache.AlarmCacheException;
import cern.laser.business.dao.AdminUserDAO;
import cern.laser.business.dao.AlarmDAO;
import cern.laser.business.dao.CategoryDAO;
import cern.laser.business.dao.ResponsiblePersonDAO;
import cern.laser.business.dao.SourceDAO;
import cern.laser.business.data.AdminUser;
import cern.laser.business.data.Alarm;
import cern.laser.business.data.AlarmChange;
import cern.laser.business.data.AlarmImpl;
import cern.laser.business.data.Building;
import cern.laser.business.data.Category;
import cern.laser.business.data.ResponsiblePerson;
import cern.laser.business.data.Source;
import cern.laser.business.definition.LaserDefinitionDuplicationException;
import cern.laser.business.definition.LaserDefinitionException;
import cern.laser.business.definition.LaserDefinitionNotAllowedException;
import cern.laser.business.definition.LaserDefinitionNotFoundException;
import cern.laser.business.definition.LaserDefinitionNotValidException;
import cern.laser.business.definition.data.AlarmDefinition;
import cern.laser.business.definition.data.MultiplicityThreshold;
import cern.laser.business.definition.data.ReductionLink;

/**
 * 
 * 
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 * @author Katarina Sigerud
 */
public class AlarmDefinitionServiceImpl {
  private static final Logger LOGGER = Logger.getLogger(AlarmDefinitionServiceImpl.class.getName());
  private static final String SOURCE_CATEGORY_PATH_PREFIX = "CERN.SOURCES.";

  private AdminUserDAO adminUserDAO;
  private AlarmDAO alarmDAO;
  private CategoryDAO categoryDAO;
  private ResponsiblePersonDAO responsiblePersonDAO;
  private SourceDAO sourceDAO;
  private AlarmCache alarmCache;
  private AlarmMessageProcessorImpl alarmMessageProcessor;
  private AlarmPublisherImpl alarmPublisher;
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

  public void setSourceDAO(SourceDAO sourceDAO) {
    this.sourceDAO = sourceDAO;
  }

  public void setResponsiblePersonDAO(ResponsiblePersonDAO responsiblePersonDAO) {
    this.responsiblePersonDAO = responsiblePersonDAO;
  }

  public void setAlarmCache(AlarmCache alarmCache) {
    this.alarmCache = alarmCache;
  }

  public void setAlarmMessageProcessor(AlarmMessageProcessorImpl alarmMessageProcessor) {
    this.alarmMessageProcessor = alarmMessageProcessor;
  }

  public void setAlarmPublisher(AlarmPublisherImpl alarmPublisher) {
    this.alarmPublisher = alarmPublisher;
  }

  public AlarmDefinition getAlarmDefinition(String alarmId) throws LaserDefinitionException {
    if (alarmId == null) throw new LaserDefinitionNotValidException("alarm id is null");

    try {
      return alarmCache.getReference(alarmId).getDefinition();
    } catch (AlarmCacheException e) {
      throw new LaserDefinitionNotFoundException("unable to get the alarm " + alarmId);
    }

    //      new AlarmDefinition(alarm.getTriplet().getFaultFamily(), alarm.getTriplet().getFaultMember(), alarm
    //          .getTriplet().getFaultCode(), alarm.getSystemName(), alarm.getIdentifier(), alarm.getProblemDescription(),
    //          alarm.getPriority(), alarm.getCause(), alarm.getAction(), alarm.getConsequence(), alarm.getInstant(), (alarm
    //              .getHelpURL() == null ? null : alarm.getHelpURL().toExternalForm()), alarm.getSource().getName(), alarm
    //              .getLocation().getBuilding(), alarm.getLocation().getFloor(), alarm.getLocation().getRoom(), alarm
    //              .getLocation().getMnemonic(), alarm.getLocation().getPosition(),
    //          (alarm.getResponsiblePerson() == null ? null : alarm.getResponsiblePerson().getResponsibleId()), alarm
    //              .getPiquetGSM(), alarm.getPiquetEmail());
  }

  public Collection getAlarmDefinitions(String userId) throws LaserDefinitionException {
    if (userId == null) { throw new LaserDefinitionNotValidException("user id is null"); }
    AdminUser admin_user = adminUserDAO.findAdminUser(userId);

    LOGGER.info("getting administered alarms for user : " + admin_user.getName());
    Collection result = new ArrayList();
    String[] administered_sources = adminUserDAO.getAdministeredSources(userId);
    for (int i = 0; i < administered_sources.length; i++) {
      Source source = sourceDAO.findSource(administered_sources[i]);
      String[] alarms = sourceDAO.getAlarms(source.getSourceId());
      for (int j = 0; j < alarms.length; j++) {
        result.add(getAlarmDefinition(alarms[j]));
      }
    }
    LOGGER.info("found " + result.size() + " administered alarms");

    return result;
  }

  public void createAlarm(String userId, AlarmDefinition alarmDefinition) throws LaserDefinitionException {
    Alarm alarm = createAlarmInternal(userId, alarmDefinition);
    alarmDAO.saveAlarm(alarm);

    String category_path = SOURCE_CATEGORY_PATH_PREFIX + alarm.getSource().getName();
    Category category = categoryDAO.findCategoryByPath(category_path);
    category.addAlarm(alarm);
    categoryDAO.updateCategory(category);

    if (LOGGER.isDebugEnabled()) LOGGER.debug("alarm added to default category " + category_path);
    LOGGER.info("alarm created");
  }

  public void createAlarms(String userId, Collection alarms) throws LaserDefinitionException {
    Set categories_to_update = new HashSet();

    if ((alarms == null) || (alarms.size() == 0)) { return; }
    LOGGER.info("creating " + alarms.size() + " alarms");

    Iterator iterator = alarms.iterator();
    while (iterator.hasNext()) {
      Alarm alarm = createAlarmInternal(userId, (AlarmDefinition) iterator.next());
      alarmDAO.saveAlarm(alarm);

      String category_path = SOURCE_CATEGORY_PATH_PREFIX + alarm.getSource().getName();
      Category category = categoryDAO.findCategoryByPath(category_path);
      category.addAlarm(alarm);
      if (LOGGER.isDebugEnabled()) LOGGER.debug("alarm added to default category " + category_path);
      categories_to_update.add(category);
    }

    for (Iterator iter = categories_to_update.iterator(); iter.hasNext();) {
      Category category_to_update = (Category) iter.next();
      categoryDAO.updateCategory(category_to_update);
    }

    LOGGER.info("alarms created");
  }

  public void updateAlarm(String userId, AlarmDefinition alarmDefinition) throws LaserDefinitionException {
    if (alarmDefinition == null) { throw new LaserDefinitionNotValidException("alarm is null"); }
    AdminUser admin_user = adminUserDAO.findAdminUser(userId);
    
    Alarm alarm = alarmDAO.findAlarm(alarmDefinition.getAlarmId());
    Source new_source = sourceDAO.findSource(alarmDefinition.getSourceName());
    Source old_source = alarm.getSource();

    if (!(admin_user.administersSource(new_source) && admin_user.administersSource(old_source))) { throw new LaserDefinitionNotAllowedException(
        "not an administrator for the alarm : " + alarmDefinition); }

    if (!new_source.equals(old_source)) {
      //      old_source.removeAlarm(alarm);
      new_source.addAlarm(alarm);

      String old_category_path = SOURCE_CATEGORY_PATH_PREFIX + old_source.getName();
      Category old_category = categoryDAO.findCategory(new Integer(old_category_path.hashCode()));
      old_category.removeAlarm(alarm);

      String new_category_path = SOURCE_CATEGORY_PATH_PREFIX + alarmDefinition.getSourceName();
      Category new_category = categoryDAO.findCategory(new Integer(new_category_path.hashCode()));
      new_category.addAlarm(alarm);
      if (LOGGER.isDebugEnabled())
          LOGGER.debug("alarm removed from category " + old_category_path + " and added to category "
              + new_category_path);

      sourceDAO.updateSource(old_source);
      sourceDAO.updateSource(new_source);
      categoryDAO.updateCategory(old_category);
      categoryDAO.updateCategory(new_category);
    }
    LOGGER.info("user " + admin_user.getName() + " updating alarm : " + alarmDefinition);

    if (!alarm.getResponsiblePerson().getResponsibleId().equals(alarmDefinition.getResponsiblePersonId())) {
      //        ResponsiblePerson old_responsible = alarm.getResponsiblePerson();
      //        old_responsible.getAlarmIds().remove(alarm.getAlarmId());

      ResponsiblePerson new_responsible = alarmDefinition.getResponsiblePersonId() == null ? new_source
          .getResponsiblePerson() : responsiblePersonDAO.getResponsiblePerson(alarmDefinition.getResponsiblePersonId());
      //        new_responsible.getAlarmIds().add(alarm.getAlarmId());

      alarm.setResponsiblePerson(new_responsible);

      //        session.update(old_responsible);
      //        session.update(new_responsible);
    }

    alarm.setDefinition(alarmDefinition);

    alarmDAO.updateAlarm(alarm);

    try {
      alarmCache.invalidate(alarmDefinition.getAlarmId());
    } catch (Exception e) {
      LOGGER.error("unable to propagate alarm update : " + alarmDefinition, e);
    }
    LOGGER.info("alarm updated");
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.DefinitionServiceSessionEJB#updateAlarms(java.lang.Integer, java.util.Collection)
   */
  public void updateAlarms(String userId, Collection alarms) throws LaserDefinitionException {
    if ((alarms == null) || (alarms.size() == 0)) { return; }
    LOGGER.info("updating " + alarms.size() + " alarms");
    Iterator iterator = alarms.iterator();

    while (iterator.hasNext()) {
      updateAlarm(userId, (AlarmDefinition) iterator.next());
    }
    LOGGER.info("alarms updated");
  }

  public void removeAlarm(String userId, AlarmDefinition alarm) throws LaserDefinitionException {
    removeAlarmInternal(userId, alarm);
    
    Alarm alarm_obj = null;
    try {
      alarm_obj = alarmCache.getCopy(alarm.getAlarmId());

      invalidateAlarm(alarm_obj);
      propagateRemovedAlarm(alarm_obj);
    } catch (Exception e) {
      LOGGER.warn("unable to handle removed alarm " + alarm_obj.getTriplet() + " : " + e.getMessage());
    }
  }

  public void removeAlarms(String userId, Collection alarms) throws LaserDefinitionException {
    if ((alarms == null) || (alarms.size() == 0)) { return; }

    removeAlarmsInternal(userId, alarms);
    Iterator iterator = alarms.iterator();
    Alarm alarm_obj = null;
    while (iterator.hasNext()) {
      try {
        alarm_obj = alarmCache.getCopy(((AlarmDefinition) iterator.next()).getAlarmId());
        invalidateAlarm(alarm_obj);
        propagateRemovedAlarm(alarm_obj);
      } catch (Exception e) {
        LOGGER.warn("unable to handle removed alarm " + alarm_obj.getTriplet() + " : " + e.getMessage());
      }
    }
  }

  public void createMultiplicityLink(String userId, ReductionLink link) throws LaserDefinitionException {
    createMultiplicityLinkInternal(userId, link);
    invalidateReductionLink(link);
    propagateMultiplicityLink(link);
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.DefinitionServiceSessionEJB#createMultiplicityLinks(java.lang.Integer,
   *      java.util.Collection)
   */
  public void createMultiplicityLinks(String userId, Collection reductionLinks) throws LaserDefinitionException {
    if ((reductionLinks == null) || (reductionLinks.size() == 0)) { return; }

    createMultiplicityLinksInternal(userId, reductionLinks);
    Iterator iterator = reductionLinks.iterator();
    while (iterator.hasNext()) {
      invalidateReductionLink((ReductionLink) iterator.next());
    }
    iterator = reductionLinks.iterator();
    while (iterator.hasNext()) {
      propagateMultiplicityLink((ReductionLink) iterator.next());
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.DefinitionServiceSessionEJB#createNodeLink(java.lang.Integer,
   *      cern.laser.business.definition.data.ReductionLink)
   */
  public void createNodeLink(String userId, ReductionLink link) throws LaserDefinitionException {
    createNodeLinkInternal(userId, link);
    invalidateReductionLink(link);
    propagateNodeLink(link);
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.DefinitionServiceSessionEJB#createNodeLinks(java.lang.Integer, java.util.Collection)
   */
  public void createNodeLinks(String userId, Collection reductionLinks) throws LaserDefinitionException {
    if ((reductionLinks == null) || (reductionLinks.size() == 0)) { return; }

    createNodeLinksInternal(userId, reductionLinks);
    Iterator iterator = reductionLinks.iterator();
    while (iterator.hasNext()) {
      invalidateReductionLink((ReductionLink) iterator.next());
    }
    iterator = reductionLinks.iterator();
    while (iterator.hasNext()) {
      propagateNodeLink((ReductionLink) iterator.next());
    }
  }

  public void setMultiplicityThreshold(String userId, MultiplicityThreshold threshold) throws LaserDefinitionException {
    setMultiplicityThresholdEJB(userId, threshold);
    propagateMultiplicityThreshold(threshold);
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.DefinitionServiceSessionEJB#setMultiplicityThresholds(java.lang.Integer,
   *      java.util.Collection)
   */
  public void setMultiplicityThresholds(String userId, Collection thresholds) throws LaserDefinitionException {
    if ((thresholds == null) || (thresholds.size() == 0)) { return; }
    LOGGER.info("setting " + thresholds.size() + " multiplicity thresholds");

    Iterator iterator = thresholds.iterator();
    while (iterator.hasNext()) {
      setMultiplicityThresholdEJB(userId, (MultiplicityThreshold) iterator.next());
    }
    LOGGER.info("multiplicity thresholds set");

    iterator = thresholds.iterator();
    while (iterator.hasNext()) {
      try {
        propagateMultiplicityThreshold((MultiplicityThreshold) iterator.next());
      } catch (Exception e) {
        LOGGER.error("unable to propagate multiplicity threshold", e);
      }
    }
    LOGGER.info("multiplicity thresholds propagated");
  }

  public void removeMultiplicityLink(String userId, ReductionLink link) throws LaserDefinitionException {
    removeMultiplicityLinkInternal(userId, link);
    invalidateReductionLink(link);
    propagateMultiplicityLink(link);
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.DefinitionServiceSessionEJB#removeMultiplicityLinks(java.lang.Integer,
   *      java.util.Collection)
   */
  public void removeMultiplicityLinks(String userId, Collection reductionLinks) throws LaserDefinitionException {
    if ((reductionLinks == null) || (reductionLinks.size() == 0)) { return; }

    removeMultiplicityLinksInternal(userId, reductionLinks);
    Iterator iterator = reductionLinks.iterator();
    while (iterator.hasNext()) {
      invalidateReductionLink((ReductionLink) iterator.next());
    }
    iterator = reductionLinks.iterator();
    while (iterator.hasNext()) {
      propagateMultiplicityLink((ReductionLink) iterator.next());
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.DefinitionServiceSessionEJB#removeNodeLink(java.lang.Integer,
   *      cern.laser.business.definition.data.ReductionLink)
   */
  public void removeNodeLink(String userId, ReductionLink link) throws LaserDefinitionException {
    removeNodeLinkInternal(userId, link);
    invalidateReductionLink(link);
    propagateNodeLink(link);
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.DefinitionServiceSessionEJB#removeNodeLinks(java.lang.Integer, java.util.Collection)
   */
  public void removeNodeLinks(String userId, Collection reductionLinks) throws LaserDefinitionException {
    if ((reductionLinks == null) || (reductionLinks.size() == 0)) { return; }

    removeNodeLinksInternal(userId, reductionLinks);
    Iterator iterator = reductionLinks.iterator();
    while (iterator.hasNext()) {
      invalidateReductionLink((ReductionLink) iterator.next());
    }
    iterator = reductionLinks.iterator();
    while (iterator.hasNext()) {
      propagateNodeLink((ReductionLink) iterator.next());
    }
  }

  public void uploadAlarms(String userId, Collection toBeCreated, Collection toBeUpdated, Collection toBeRemoved)
      throws LaserDefinitionException {
    LOGGER.info("uploading alarm definitions : [userId=" + userId + ", toBeCreated="
        + ((toBeCreated == null) ? 0 : toBeCreated.size()) + ", toBeUpdated="
        + ((toBeUpdated == null) ? 0 : toBeUpdated.size()) + ", toBeRemoved="
        + ((toBeRemoved == null) ? 0 : toBeRemoved.size()) + "]");
    createAlarms(userId, toBeCreated);
    updateAlarms(userId, toBeUpdated);
    removeAlarms(userId, toBeRemoved);
    LOGGER.info("alarm definitions uploaded");
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.DefinitionServiceSessionEJB#uploadMultiplicity(java.lang.Integer,
   *      java.util.Collection, java.util.Collection, java.util.Collection)
   */
  public void uploadMultiplicity(String userId, Collection toBeCreated, Collection toBeRemoved, Collection thresholds)
      throws LaserDefinitionException {
    LOGGER.info("uploading multiplicity reduction definitions : [userId=" + userId + ", toBeCreated="
        + ((toBeCreated == null) ? 0 : toBeCreated.size()) + ", toBeRemoved="
        + ((toBeRemoved == null) ? 0 : toBeRemoved.size()) + ", thresholds="
        + ((thresholds == null) ? 0 : thresholds.size()) + "]");
    createMultiplicityLinks(userId, toBeCreated);
    removeMultiplicityLinks(userId, toBeRemoved);
    setMultiplicityThresholds(userId, thresholds);
    LOGGER.info("multiplicity reduction definitions uploaded");
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.DefinitionServiceSessionEJB#uploadNode(java.lang.Integer, java.util.Collection,
   *      java.util.Collection)
   */
  public void uploadNode(String userId, Collection toBeCreated, Collection toBeRemoved) throws LaserDefinitionException {
    LOGGER.info("uploading node reduction definitions : [userId=" + userId + ", toBeCreated="
        + ((toBeCreated == null) ? 0 : toBeCreated.size()) + ", toBeRemoved="
        + ((toBeRemoved == null) ? 0 : toBeRemoved.size()) + "]");
    createNodeLinks(userId, toBeCreated);
    removeNodeLinks(userId, toBeRemoved);
    LOGGER.info("node reduction definitions uploaded");
  }

  //
  // -- PRIVATE METHODS ----------------------------------------------
  //

  private Alarm createAlarmInternal(String userId, AlarmDefinition alarmDefinition) throws LaserDefinitionException {
    if (alarmDefinition == null) { throw new LaserDefinitionNotValidException("alarm is null"); }

    Alarm alarm = alarmDAO.getAlarm(alarmDefinition.getAlarmId());
    if (alarm != null) { throw new LaserDefinitionDuplicationException("alarm " + alarmDefinition.getAlarmId()
        + " does already exist"); }

    if (alarmDefinition.getSourceName() == null) { throw new LaserDefinitionNotValidException(
        "source name for the alarm definition is null"); }
    Source source = sourceDAO.getSource(alarmDefinition.getSourceName());
    if (source == null) { throw new LaserDefinitionNotFoundException("source " + alarmDefinition.getSourceName()
        + " does not exist"); }

    if (alarmDefinition.getResponsiblePersonId() == null) { throw new LaserDefinitionNotValidException(
        "responsible id for the alarm definition is null"); }
    ResponsiblePerson responsible = responsiblePersonDAO.getResponsiblePerson(alarmDefinition.getResponsiblePersonId());
    if (responsible == null) { throw new LaserDefinitionNotFoundException("responsible with id "
        + alarmDefinition.getResponsiblePersonId() + " does not exist"); }

    AdminUser admin_user = adminUserDAO.findAdminUser(userId);

    if (!admin_user.administersSource(source)) { throw new LaserDefinitionNotAllowedException(
        "not an administrator for the alarm : " + alarmDefinition); }
    LOGGER.info("user " + admin_user.getName() + " creating alarm : " + alarmDefinition);

    alarm = new AlarmImpl(alarmDefinition, source, responsible);
    String building_number = alarmDefinition.getBuilding();
    if (building_number != null && !building_number.equals("")) {
      Building building = alarmDAO.findBuilding(building_number);
      alarm.getLocation().setBuilding(building);
    }
    return alarm;

  }

  private void removeAlarmInternal(String userId, AlarmDefinition alarmDefinition) throws LaserDefinitionException {
    if (alarmDefinition == null) { throw new LaserDefinitionNotValidException("alarm is null"); }

    Alarm alarm = alarmDAO.getAlarm(alarmDefinition.getAlarmId());
    if (alarm == null) { throw new LaserDefinitionNotFoundException("alarm " + alarmDefinition.getAlarmId()
        + " does not exist"); }

    AdminUser admin_user = adminUserDAO.findAdminUser(userId);

    if (!admin_user.administersSource(alarm.getSource())) { throw new LaserDefinitionNotAllowedException(
        "not in source administrators"); }
    try {
      LOGGER.info("user " + admin_user.getName() + " removing alarm : " + alarmDefinition.getAlarmId());
      alarmDAO.deleteAlarm(alarm);
      LOGGER.info("alarm removed");
    } catch (Exception e) {
      throw new LaserRuntimeException(
          "unable to remove alarm " + alarmDefinition.getAlarmId() + " : " + e.getMessage(), e);
    }
  }

  private void removeAlarmsInternal(String userId, Collection alarms) throws LaserDefinitionException {
    LOGGER.info("removing " + alarms.size() + " alarms");
    Iterator iterator = alarms.iterator();

    while (iterator.hasNext()) {
      removeAlarmInternal(userId, (AlarmDefinition) iterator.next());
    }
    LOGGER.info("alarms removed");
  }

  private void invalidateAlarm(Alarm alarm) {
    try {
      if (LOGGER.isDebugEnabled()) LOGGER.debug("invalidating alarm " + alarm.getTriplet() + " and its relatives...");
      alarmCache.invalidate(alarm.getAlarmId());
      String[] alarm_ids = alarm.getMultiplicityChildren();
      for (int i = 0; i < alarm_ids.length; i++) {
        alarmCache.invalidate(alarm_ids[i]);
      }
      alarm_ids = alarm.getMultiplicityParents();
      for (int i = 0; i < alarm_ids.length; i++) {
        alarmCache.invalidate(alarm_ids[i]);
      }
      alarm_ids = alarm.getNodeChildren();
      for (int i = 0; i < alarm_ids.length; i++) {
        alarmCache.invalidate(alarm_ids[i]);
      }
      alarm_ids = alarm.getNodeParents();
      for (int i = 0; i < alarm_ids.length; i++) {
        alarmCache.invalidate(alarm_ids[i]);
      }
      if (LOGGER.isDebugEnabled()) LOGGER.debug("invalidated");
    } catch (Exception e) {
      LOGGER.error("unable to invalidate alarm " + alarm.getTriplet() + " : " + e.getMessage());
    }
  }

  private void propagateRemovedAlarm(Alarm alarm) {
    try {
      if (LOGGER.isDebugEnabled()) LOGGER.debug("propagating removed alarm : " + alarm.getTriplet());
      if (alarm.getStatus().getActive().booleanValue()) {
        Alarm terminated = (Alarm) ((AlarmImpl) alarm).clone();

        terminated.getStatus().setActive(Boolean.FALSE);
        terminated.getStatus().setSourceHostname("LASER");
        Timestamp now = new Timestamp(System.currentTimeMillis());

        terminated.getStatus().setSourceTimestamp(now);
        terminated.getStatus().setSystemTimestamp(now);
        terminated.getStatus().setUserTimestamp(now);
        terminated.getStatus().getProperties().clear();
        terminated.getStatus().getProperties().setProperty("REMOVED_BOOL", "TRUE");
        alarmPublisher.publish(new AlarmChange(terminated, alarm));
        alarmMessageProcessor.notifyReductionRelatives(alarm);
      }
      if (LOGGER.isDebugEnabled()) LOGGER.debug("propagated");
    } catch (Exception e) {
      LOGGER.error("unable to propagate removed alarm " + alarm.getTriplet() + " : " + e.getMessage());
    }
  }

  private void invalidateReductionLink(ReductionLink link) {
    try {
      if (LOGGER.isDebugEnabled()) LOGGER.debug("invalidating reduction link : " + link);
      invalidateAlarm(alarmCache.getCopy(link.getParent().getAlarmId()));
      invalidateAlarm(alarmCache.getCopy(link.getChild().getAlarmId()));
      if (LOGGER.isDebugEnabled()) LOGGER.debug("invalidated");
    } catch (Exception e) {
      LOGGER.error("unable to invalidate reduction link " + link + " : " + e.getMessage());
    }
  }

  private void propagateMultiplicityLink(ReductionLink link) {
    try {
      if (LOGGER.isDebugEnabled()) LOGGER.debug("propagating multiplicity reduction link : " + link);
      alarmMessageProcessor.updateMultiplicityNode(alarmCache.getCopy(link.getParent().getAlarmId()));
      alarmMessageProcessor.updateReductionStatus(alarmCache.getCopy(link.getChild().getAlarmId()));
      if (LOGGER.isDebugEnabled()) LOGGER.debug("propagated");
    } catch (Exception e) {
      LOGGER.error("unable to propagate multiplicity reduction link " + link, e);
    }
  }

  private void propagateMultiplicityThreshold(MultiplicityThreshold threshold) {
    try {
      if (LOGGER.isDebugEnabled()) LOGGER.debug("propagating multiplicity threshold : " + threshold);
      alarmCache.invalidate(threshold.getParent().getAlarmId());
      alarmMessageProcessor.updateMultiplicityNode(alarmCache.getCopy(threshold.getParent().getAlarmId()));
      if (LOGGER.isDebugEnabled()) LOGGER.debug("propagated");
    } catch (Exception e) {
      LOGGER.error("unable to propagate multiplicity threshold " + threshold, e);
    }
  }

  private void propagateNodeLink(ReductionLink link) {
    try {
      if (LOGGER.isDebugEnabled()) LOGGER.debug("propagating node reduction link : " + link);
      //getAlarmMessageProcessorSessionEJBLocal().updateReductionStatus(alarmCache.getCopy(link.getParent().getAlarmId()));
      alarmMessageProcessor.updateReductionStatus(alarmCache.getCopy(link.getChild().getAlarmId()));
      if (LOGGER.isDebugEnabled()) LOGGER.debug("propagated");
    } catch (Exception e) {
      LOGGER.error("unable to propagate node reduction link " + link, e);
    }
  }

  private void removeMultiplicityLinkInternal(String userId, ReductionLink link) throws LaserDefinitionException {
    Alarm[] parent_child = validateReductionLink(link);

    AdminUser admin_user = adminUserDAO.findAdminUser(userId);

    LOGGER.info("user " + admin_user.getName() + " removing multiplicity link : " + link);
    Alarm parent = parent_child[0]; //alarmDAO.findAlarm(link.getParent().getAlarmId());
    Alarm child = parent_child[1]; //alarmDAO.findAlarm(link.getChild().getAlarmId());

    parent.removeMultiplicityChild(child);
    alarmDAO.updateAlarm(parent);

    LOGGER.info("multiplicity link removed");
  }

  private void removeMultiplicityLinksInternal(String userId, Collection links) throws LaserDefinitionException {
    LOGGER.info("removing " + links.size() + " multiplicity links");
    Iterator iterator = links.iterator();

    while (iterator.hasNext()) {
      removeMultiplicityLinkInternal(userId, (ReductionLink) iterator.next());
    }
    LOGGER.info("links removed");
  }

  private void removeNodeLinkInternal(String userId, ReductionLink link) throws LaserDefinitionException {
    Alarm[] parent_child = validateReductionLink(link);

    AdminUser admin_user = adminUserDAO.findAdminUser(userId);

    LOGGER.info("user " + admin_user.getName() + " removing node link : " + link);
    Alarm parent = parent_child[0]; //alarmDAO.findAlarm(link.getParent().getAlarmId());
    Alarm child = parent_child[1]; //alarmDAO.findAlarm(link.getChild().getAlarmId());

    parent.removeNodeChild(child);
    alarmDAO.updateAlarm(parent);

    LOGGER.info("node link removed");
  }

  private void removeNodeLinksInternal(String userId, Collection links) throws LaserDefinitionException {
    LOGGER.info("removing " + links.size() + " node links");
    Iterator iterator = links.iterator();
    while (iterator.hasNext()) {
      removeNodeLinkInternal(userId, (ReductionLink) iterator.next());
    }
    LOGGER.info("links removed");
  }

  private Alarm[] validateReductionLink(ReductionLink link) throws LaserDefinitionException {
    if (link == null) { throw new LaserDefinitionNotValidException("reduction link is null"); }
    if (link.getParent() == null) { throw new LaserDefinitionNotValidException(
        "malformed reduction link: parent is null"); }
    if (link.getChild() == null) { throw new LaserDefinitionNotValidException("malformed reduction link: child is null"); }

    Alarm parent = alarmDAO.getAlarm(link.getParent().getAlarmId());
    if (parent == null) { throw new LaserDefinitionNotFoundException("reduction parent "
        + link.getParent().getAlarmId() + " does not exist"); }
    Alarm child = alarmDAO.getAlarm(link.getChild().getAlarmId());
    if (child == null) { throw new LaserDefinitionNotFoundException("reduction child " + link.getChild().getAlarmId()
        + " does not exist"); }

    return new Alarm[] { parent, child};
  }

  private void createMultiplicityLinkInternal(String userId, ReductionLink link) throws LaserDefinitionException {
    Alarm[] parent_child = validateReductionLink(link);
    
    Alarm parent = parent_child[0]; //alarmDAO.findAlarm(link.getParent().getAlarmId());
    Alarm child = parent_child[1]; //alarmDAO.findAlarm(link.getChild().getAlarmId());
    if (((AlarmImpl)parent).getMultiplicityChildrenIds().contains(child.getAlarmId())) { throw new LaserDefinitionDuplicationException("reduction child " + link.getChild().getAlarmId()
        + " does not exist"); }

    AdminUser admin_user = adminUserDAO.findAdminUser(userId);

    LOGGER.info("user " + admin_user.getName() + " creating multiplicity link : " + link);

    parent.addMultiplicityChild(child);
    alarmDAO.updateAlarm(parent);

    LOGGER.info("multiplicity link created");
  }

  private void createMultiplicityLinksInternal(String userId, Collection links) throws LaserDefinitionException {

    LOGGER.info("creating " + links.size() + " multiplicity links");
    Iterator iterator = links.iterator();

    while (iterator.hasNext()) {
      createMultiplicityLinkInternal(userId, (ReductionLink) iterator.next());
    }
    LOGGER.info("links created");
  }

  private void createNodeLinkInternal(String userId, ReductionLink link) throws LaserDefinitionException {
    Alarm[] parent_child = validateReductionLink(link);

    AdminUser admin_user = adminUserDAO.findAdminUser(userId);

    try {
      LOGGER.info("user " + admin_user.getName() + " creating node link : " + link);
      Alarm parent = parent_child[0]; //alarmDAO.findAlarm(link.getParent().getAlarmId());
      Alarm child = parent_child[1]; //alarmDAO.findAlarm(link.getChild().getAlarmId());

      parent.addNodeChild(child);
      alarmDAO.updateAlarm(parent);

      LOGGER.info("node link created");
    } catch (Exception e) {
      throw new LaserRuntimeException("unable to create the node link " + link + " : " + e.getMessage(), e);
    }
  }

  private void createNodeLinksInternal(String userId, Collection links) throws LaserDefinitionException {
    LOGGER.info("creating " + links.size() + " node links");
    Iterator iterator = links.iterator();

    while (iterator.hasNext()) {
      createNodeLinkInternal(userId, (ReductionLink) iterator.next());
    }
    LOGGER.info("links created");
  }

  private void setMultiplicityThresholdEJB(String userId, MultiplicityThreshold threshold)
      throws LaserDefinitionException {
    if ((threshold == null) || (threshold.getParent() == null) || (threshold.getThreshold() == null)
        || (threshold.getThreshold().intValue() <= 0))
        throw new LaserDefinitionNotValidException("null parameter or negative threshold");

    AdminUser admin_user = adminUserDAO.findAdminUser(userId);
    LOGGER.info("user " + admin_user.getName() + " setting multiplicity threshold : " + threshold);
    Alarm parent = alarmDAO.findAlarm(threshold.getParent().getAlarmId());

    parent.setMultiplicityThreshold(threshold.getThreshold());
    alarmDAO.updateAlarm(parent);

    LOGGER.info("multiplicity threshold set");
  }
}