/*
 * $Id: CoreServiceImpl.java,v 1.5 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.5 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.business.pojo;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;

import org.apache.log4j.Logger;

import cern.laser.business.LaserProcessingException;
import cern.laser.business.LaserRuntimeException;
import cern.laser.business.ProcessingController;
import cern.laser.business.cache.AlarmCache;
import cern.laser.business.cache.AlarmCacheException;
import cern.laser.business.dao.AlarmDAO;
import cern.laser.business.dao.CategoryDAO;
import cern.laser.business.dao.ResponsiblePersonDAO;
import cern.laser.business.dao.SourceDAO;
import cern.laser.business.data.Alarm;
import cern.laser.business.data.Category;
import cern.laser.business.data.CategoryActiveList;
import cern.laser.business.data.ResponsiblePerson;
import cern.laser.business.data.Source;
import cern.laser.business.data.Triplet;

/**
 * @version $Revision: 1.5 $ $Date: 2006/09/25 08:52:36 $
 * @author Francesco Calderini
 * @author Katarina Sigerud
 */
public class CoreServiceImpl {
  private static final Logger LOGGER = Logger.getLogger(CoreServiceImpl.class.getName());

  private static final long DEFAULT_HEARTBEAT_FREQUENCY = 60000;
  private static final String DEFAULT_HEARTBEAT_TOPIC = "CMW.ALARM_SYSTEM.HEARTBEAT";

  private CategoryDAO categoryDAO;
  private AlarmDAO alarmDAO;
  private SourceDAO sourceDAO;
  private ResponsiblePersonDAO responsiblePersonDAO;
  private AlarmCache alarmCache;
  private AlarmPublisherImpl alarmPublisher;

  private String clientRootTopic;
  private int rootCategoryPK;
  private String heartbeatTopic = DEFAULT_HEARTBEAT_TOPIC;
  private long heartbeatFrequency = DEFAULT_HEARTBEAT_FREQUENCY;
  private String searchRootTopic;

//  private DataSource dataSource;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  public void setAlarmDAO(AlarmDAO alarmDAO) {
    this.alarmDAO = alarmDAO;
  }

  public void setCategoryDAO(CategoryDAO categoryDAO) {
    this.categoryDAO = categoryDAO;
  }

  public void setResponsiblePersonDAO(ResponsiblePersonDAO responsiblePersonDAO) {
    this.responsiblePersonDAO = responsiblePersonDAO;
  }

  public void setSourceDAO(SourceDAO sourceDAO) {
    this.sourceDAO = sourceDAO;
  }

  public void setAlarmCache(AlarmCache alarmCache) {
    this.alarmCache = alarmCache;
  }
  
  public void setAlarmPublisher(AlarmPublisherImpl alarmPublisher) {
    this.alarmPublisher = alarmPublisher;
  }

  /*public void setDataSource(DataSource dataSource) {
    this.dataSource = dataSource;
  }*/

  public void setClientRootTopic(String clientRootTopic) {
    this.clientRootTopic = clientRootTopic;
  }

  public void setRootCategoryPK(int rootCategoryPK) {
    this.rootCategoryPK = rootCategoryPK;
  }

  public void setHeartbeatTopic(String heartbeatTopic) {
    this.heartbeatTopic = heartbeatTopic;
  }

  public void setHeartbeatFrequency(long heartbeatFrequency) {
    this.heartbeatFrequency = heartbeatFrequency;
  }

  public void setSearchRootTopic(String searchRootTopic) {
    this.searchRootTopic = searchRootTopic;
  }

  public Collection getActiveMultiplicityChildren(String parentId) {
    Collection result = null;
    try {
      Alarm parent = alarmCache.getReference(parentId);
      String[] children = parent.getMultiplicityChildren();
      result = new ArrayList(children.length);
      for (int i = 0; i < children.length; i++) {
        Alarm alarm = alarmCache.getReference(children[i]);
        if (alarm.getStatus().getActive().booleanValue()) result.add(alarm);
      }
    } catch (AlarmCacheException onf) {
      LOGGER.warn("multiplicity parent " + parentId + " not found");
      result = new ArrayList();
    }

    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.CoreServiceSessionEJB#getActiveNodeChildren(java.lang.Integer)
   */
  public Collection getActiveNodeChildren(String parentId) {
    Collection result = null;
    try {
      Alarm parent = alarmCache.getReference(parentId);
      String[] children = parent.getNodeChildren();
      result = new ArrayList(children.length);
      for (int i = 0; i < children.length; i++) {
        Alarm alarm = alarmCache.getReference(children[i]);
        if (alarm.getStatus().getActive().booleanValue()) result.add(alarm);
      }
    } catch (AlarmCacheException onf) {
      LOGGER.warn("node parent " + parentId + " not found");
      result = new ArrayList();
    }

    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.CoreServiceSessionEJB#getAlarmById(java.lang.Integer)
   */
  public Alarm getAlarmById(String id) {
    Alarm result = null;
    try {
      result = alarmCache.getCopy(id);
    } catch (AlarmCacheException e) {
      LOGGER.warn("unable to get alarm by id " + id, e);
    }
  
    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.CoreServiceSessionEJB#getAlarmByTriplet(java.lang.String, java.lang.String,
   *      java.lang.Integer)
   */
  public Alarm getAlarmByTriplet(String ff, String fm, Integer fc) {
    Alarm result = null;
    try {
      result = alarmCache.getCopy(Triplet.toIdentifier(ff, fm, fc));
    } catch (Exception e) {
      LOGGER.warn("unable to get alarm by triplet : " + ff + ":" + fm + ":" + fc, e);
    }

    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.CoreServiceSessionEJB#getAlarmsByCategory(java.lang.Integer)
   */
  public Collection getAlarmsByCategory(Integer categoryId) {
    Collection result = null;
    //    Category category = categoryDAO.getCategory(categoryId);
    //    if (category != null) {
    String[] alarms = categoryDAO.getAlarms(categoryId);
    result = new ArrayList(alarms.length);
    for (int i = 0; i < alarms.length; i++) {
      Alarm alarm;
      try {
        alarm = alarmCache.getReference(alarms[i]);
      } catch (AlarmCacheException e) {
        LOGGER.error("unable to get alarm " + alarms[i] + " from cache", e);
        throw new LaserRuntimeException("unable to get alarm " + alarms[i] + " from cache", e);
      }
      result.add(alarm);
    }
    //    } else {
    //      result = new ArrayList(0);
    //    }

    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.CoreServiceSessionEJB#getAlarmsByPriority(java.lang.Integer)
   */
  public Collection getAlarmsByPriority(Integer priority) {
    Collection result = null;
    try {
      String[] alarm_ids = alarmDAO.findAlarmIdsByPriority(priority);
      result = new ArrayList(alarm_ids.length);
      for (int i = 0; i < alarm_ids.length; i++) {
        Alarm alarm = alarmCache.getReference(alarm_ids[i]);
        result.add(alarm);
      }
    } catch (Exception e) {
      LOGGER.error("unable to get alarms by priority " + priority, e);
      //throw new EJBException("unable to get alarms by priority " + priority, e);
    }

    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.CoreServiceSessionEJB#getAlarmsByResponsiblePerson(java.lang.Integer)
   */
  public Collection getAlarmsByResponsiblePerson(Integer responsibleId) {
    Collection result = null;
    try {
      String[] alarm_ids = responsiblePersonDAO.getAlarms(responsibleId);
      result = new ArrayList(alarm_ids.length);
      for (int i = 0; i < alarm_ids.length; i++) {
        Alarm alarm = alarmCache.getReference(alarm_ids[i]);
        result.add(alarm);
      }
    } catch (Exception e) {
      LOGGER.error("unable to get alarms by responsible person " + responsibleId, e);
      //throw new EJBException("unable to get alarms by responsible person " + responsibleId, e);
    }

    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.CoreServiceSessionEJB#getAlarmsBySource(java.lang.Integer)
   */
  public Collection getAlarmsBySource(String sourceId) {
    Collection result = null;
    try {
      String[] alarms = sourceDAO.getAlarms(sourceId);
      result = new ArrayList(alarms.length);
      for (int i = 0; i < alarms.length; i++) {
        Alarm alarm = alarmCache.getReference(alarms[i]);
        result.add(alarm);
      }
    } catch (Exception e) {
      LOGGER.error("unable to get alarms by source " + sourceId, e);
      //throw new EJBException("unable to get alarms by source " + sourceId, e);
    }

    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.CoreServiceSessionEJB#getCategories()
   */
  public Collection getCategories() {
    Collection result = null;
    Category[] categories = categoryDAO.findAllCategories();
    result = new ArrayList(categories.length);
    for (int i = 0; i < categories.length; i++) {
      result.add(categories[i]);
    }

    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.CoreServiceSessionEJB#getCategoryByPath(java.lang.String)
   */
  public Category getCategoryByPath(String path) {
    return categoryDAO.getCategoryByPathInitialized(path);
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.CoreServiceSessionEJB#getCategoryChildren(java.lang.Integer)
   */
  public Collection getCategoryChildren(Integer nodeId) {
    Collection result = null;
    //    Category node = categoryDAO.getCategory(nodeId);
    //    if (node != null) {
    Integer[] children_ids = categoryDAO.getChildren(nodeId);
    result = new ArrayList(children_ids.length);
    for (int i = 0; i < children_ids.length; i++) {
      result.add(categoryDAO.findCategory((Integer) children_ids[i]));
    }
    //    } else {
    //      result = new ArrayList(0);
    //    }

    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.CoreServiceSessionEJB#getCategoryParent(java.lang.Integer)
   */
  public Category getCategoryParent(Integer nodeId) {
    Category node = categoryDAO.getCategory(nodeId);
    if (node != null) { return categoryDAO.getCategory(node.getParentId()); }
    return null;
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.CoreServiceSessionEJB#getCategoryRootTopic()
   */
  public String getCategoryRootTopic() {
    try {
      return alarmPublisher.getCategoryRootTopic();
    } catch (Exception e) {
      LOGGER.error("unable to get category root topic", e);
      //throw new EJBException("unable to get category root topic", e);
      return null;
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.CoreServiceSessionEJB#getCategoryTreeRoot()
   */
  public Category getCategoryTreeRoot() {
    return categoryDAO.findByCategoryTreeRoot();
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.CoreServiceSessionEJB#getClientRootTopic()
   */
  public String getClientRootTopic() {
    return clientRootTopic;
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.CoreServiceSessionEJB#getMultiplicityChildren(java.lang.Integer)
   */
  public Collection getMultiplicityChildren(String parentId) {
    Collection result = null;
    try {
      Alarm parent = alarmCache.getReference(parentId);
      String[] children = parent.getMultiplicityChildren();
      result = new ArrayList(children.length);
      for (int i = 0; i < children.length; i++) {
        Alarm alarm = alarmCache.getReference(children[i]);
        result.add(alarm);
      }
    } catch (AlarmCacheException onf) {
      LOGGER.warn("multiplicity parent " + parentId + " not found");
      result = new ArrayList();
    } catch (Exception e) {
      LOGGER.error("unable to get multiplicity children for " + parentId, e);
      //throw new EJBException("unable to get multiplicity children for " + parentId, e);
    }

    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.CoreServiceSessionEJB#getMultiplicityParents(java.lang.Integer)
   */
  public Collection getMultiplicityParents(String childId) {
    Collection result = null;
    try {
      Alarm child = alarmCache.getReference(childId);
      String[] parents = child.getMultiplicityParents();
      result = new ArrayList(parents.length);
      for (int i = 0; i < parents.length; i++) {
        Alarm alarm = alarmCache.getReference(parents[i]);
        result.add(alarm);
      }
    } catch (AlarmCacheException onf) {
      LOGGER.warn("multiplicity child " + childId + " not found");
      result = new ArrayList();
    } catch (Exception e) {
      LOGGER.error("unable to get multiplicity parents for child " + childId, e);
      //throw new EJBException("unable to get multiplicity parents for child " + childId, e);
    }

    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.CoreServiceSessionEJB#getMultiplicityThreshold(java.lang.Integer)
   */
  public Integer getMultiplicityThreshold(String parentId) {
    Integer result = new Integer(0);
    try {
      Alarm parent = alarmCache.getReference(parentId);
      result = parent.getMultiplicityThreshold();
    } catch (AlarmCacheException onf) {
      LOGGER.warn("multiplicity parent " + parentId + " not found");
    } catch (Exception e) {
      LOGGER.error("unable to get multiplicity threshold for " + parentId, e);
      //throw new EJBException("unable to get multiplicity threshold for " + parentId, e);
    }

    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.CoreServiceSessionEJB#getNodeChildren(java.lang.Integer)
   */
  public Collection getNodeChildren(String parentId) {
    Collection result = null;
    try {
      Alarm parent = alarmCache.getReference(parentId);
      String[] children = parent.getNodeChildren();
      result = new ArrayList(children.length);
      for (int i = 0; i < children.length; i++) {
        Alarm alarm = alarmCache.getReference(children[i]);
        result.add(alarm);
      }
    } catch (AlarmCacheException onf) {
      LOGGER.warn("node parent " + parentId + " not found");
      result = new ArrayList();
    } catch (Exception e) {
      LOGGER.error("unable to get node children for parent " + parentId, e);
      //throw new EJBException("unable to get node children for parent " + parentId, e);
    }

    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.CoreServiceSessionEJB#getNodeParents(java.lang.Integer)
   */
  public Collection getNodeParents(String childId) {
    Collection result = null;
    try {
      Alarm child = alarmCache.getReference(childId);
      String[] parents = child.getNodeParents();
      result = new ArrayList(parents.length);
      for (int i = 0; i < parents.length; i++) {
        Alarm alarm = alarmCache.getReference(parents[i]);
        result.add(alarm);
      }
    } catch (AlarmCacheException onf) {
      LOGGER.warn("node child " + childId + " not found");
      result = new ArrayList();
    } catch (Exception e) {
      LOGGER.error("unable to get node parents for child " + childId, e);
      //throw new EJBException("unable to get node parents for child " + childId, e);
    }

    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.CoreServiceSessionEJB#getResponsiblePersons()
   */
  public Collection getResponsiblePersons() {
    Collection result = null;
    try {
      ResponsiblePerson[] responsibles_ejb = responsiblePersonDAO.findAllResponsiblePersons();
      result = new ArrayList(responsibles_ejb.length);
      for (int i = 0; i < responsibles_ejb.length; i++) {
        result.add(responsibles_ejb[i]);
      }
    } catch (Exception e) {
      LOGGER.error("unable to get alarm responsibles", e);
      //throw new EJBException("unable to get alarm responsibles", e);
    }

    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.CoreServiceSessionEJB#getSources()
   */
  public Collection getSources() {
    Collection result = null;
    try {
      Source[] sources = sourceDAO.findAllSources();
      result = new ArrayList(sources.length);
      for (int i = 0; i < sources.length; i++) {
        result.add(sources[i]);
      }
    } catch (Exception e) {
      LOGGER.error("unable to get alarm sources", e);
      //throw new EJBException("unable to get alarm sources", e);
    }

    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.CoreServiceSessionEJB#select(java.util.Collection, java.lang.String)
   */
  public void select(Collection categoryIds, String client) throws LaserProcessingException {
    LOGGER.info("querying active alarms for [" + client + "] on categories [" + categoryIds + "]");
    Iterator category_iterator = categoryIds.iterator();
    while (category_iterator.hasNext()) {
      select((Integer) category_iterator.next(), client);
    }
    String destination = getClientRootTopic() + "." + client;
    alarmPublisher.sendInitFinished(destination);
  }

  public String getHeartbeatTopic() {
    return heartbeatTopic;
  }

  public long getHeartbeatFrequency() {
    return heartbeatFrequency;
  }

  /**
   * @return
   */
  public String getSearchRootTopic() {
    return searchRootTopic;
  }

  /**
   * @param categoryIds
   * @param sql
   * @param clientId
   */
  public void search(Integer[] categoryIds, String sql, String clientId) {
    String destination = getSearchRootTopic() + "." + clientId;
    String category_constraints = buildCategorySQLFilter(categoryIds);
    String select_sql = "select alarm_id from alarm_definition where " + sql + " and alarm_id in "
        + "(select alarm_id from alarm_category where " + category_constraints + ")";

    if (LOGGER.isDebugEnabled()) LOGGER.debug("search sql : " + select_sql);
    Collection search_result = alarmDAO.search(select_sql);
    if (LOGGER.isDebugEnabled()) LOGGER.debug("found "+search_result.size()+" alarms");
    Collection found_alarms = new ArrayList(search_result.size());
    try {
      for (Iterator iter = search_result.iterator(); iter.hasNext();) {
        Alarm alarm = alarmCache.getReference((String) iter.next());
        found_alarms.add(alarm);
      }
    } catch (AlarmCacheException e) {
      throw new LaserRuntimeException("unable to search alarms", e);
    }
    alarmPublisher.sendSearch(found_alarms, destination);
  }

  /**
   * @param categoryIds
   * @param sql
   * @param clientId
   */
  public void archiveSearch(Integer[] categoryIds, String sql, String clientId) {
    String destination = getSearchRootTopic() + "." + clientId;
    String category_constraints = buildCategorySQLFilter(categoryIds);
    String select_sql = "select alarm_id from alarm_definition where " + sql + " and alarm_id in "
        + "(select alarm_id from alarm_category where " + category_constraints + ")";

    if (LOGGER.isDebugEnabled()) LOGGER.debug("search sql " + select_sql);
    Collection search_result = alarmDAO.archiveSearch(select_sql);
    Collection found_alarms = new ArrayList(search_result.size());
    try {
      for (Iterator iter = search_result.iterator(); iter.hasNext();) {
        Alarm alarm = alarmCache.getReference((String) iter.next());
        found_alarms.add(alarm);
      }
    } catch (AlarmCacheException e) {
      throw new LaserRuntimeException("unable to search alarms", e);
    }
    alarmPublisher.sendSearch(found_alarms, destination);
  }

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //

  /*
   * (non-Javadoc)
   * 
   * @see cern.laser.business.ejb.CoreServiceSessionEJB#select(java.lang.Integer, java.lang.String)
   */
  private void select(Integer categoryId, String clientId) throws LaserProcessingException {
	  System.out.println("*** CoreServiceImpl::select: selecting category "+categoryId.toString()+" for client "+clientId);
    ProcessingController processingController = ProcessingController.getInstance();
    if (!processingController.isProcessing()) {
      throw new LaserProcessingException("server not initialized");
    }
    try {
      if (LOGGER.isInfoEnabled()) {
        Category category = categoryDAO.findCategory(categoryId);
        String category_path = category.getPath();
        LOGGER.info("requested category : " + category_path);
      }
      String destination = getClientRootTopic() + "." + clientId;
      CategoryActiveList active_list = alarmCache.getActiveListReference(categoryId);
      String[] active_alarms = active_list.getActiveAlarms();
      if (active_alarms.length > 0) {
        Collection init_alarms = new HashSet(active_alarms.length);
        for (int i = 0; i < active_alarms.length; i++) {
          Alarm alarm = alarmCache.getReference(active_alarms[i]);
          init_alarms.add(alarm);
        }
        LOGGER.info("found " + init_alarms.size() + " matching alarm(s)");
        alarmPublisher.sendInit(init_alarms, destination);
      }
    } catch (AlarmCacheException e) {
    	System.err.println("*** Got an exception! "+e.getMessage());
    	e.printStackTrace(System.out);
    	System.err.println("*** Exception masked");
      //throw new EJBException("unable to select alarms", e);
    }
  }

  /**
   * @param categoryIds
   * @return
   */
  private String buildCategorySQLFilter(Integer[] categoryIds) {
    StringBuffer category_constraints = new StringBuffer();
    for (int i = 0; i < categoryIds.length; i++) {
      category_constraints.append("category_id=");
      category_constraints.append(categoryIds[i]);
      if (i == categoryIds.length) category_constraints.append(" or");
    }

    return category_constraints.toString();
  }

  private boolean initialized;
}