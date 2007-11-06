/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *                 and Cosylab
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 *    MA 02111-1307  USA
 */
package com.cosylab.acs.laser;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.logging.Logger;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.Session;
import javax.jms.TextMessage;
import javax.jms.Topic;
import javax.jms.TopicConnection;
import javax.jms.TopicSession;
import javax.jms.TopicSubscriber;

import org.omg.CosPropertyService.Property;

import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;

import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;
import alma.alarmsystem.Alarm;
import alma.alarmsystem.AlarmServiceOperations;
import alma.alarmsystem.Category;
import alma.alarmsystem.LaserProcessingException;
import alma.alarmsystem.Location;
import alma.alarmsystem.ResponsiblePerson;
import alma.alarmsystem.Source;
import alma.alarmsystem.Status;
import alma.alarmsystem.Timestamp;
import alma.alarmsystem.Triplet;

import cern.cmw.mom.pubsub.impl.ACSJMSTopicConnectionImpl;
import cern.laser.business.cache.AlarmCacheException;
import cern.laser.business.cache.AlarmCacheListener;
import cern.laser.business.cache.AlarmCacheListenerImpl;
import cern.laser.business.pojo.AdminUserDefinitionServiceImpl;
import cern.laser.business.pojo.AlarmCacheServerImpl;
import cern.laser.business.pojo.AlarmDefinitionServiceImpl;
import cern.laser.business.pojo.AlarmMessageProcessorImpl;
import cern.laser.business.pojo.AlarmPublisherImpl;
import cern.laser.business.pojo.AlarmSourceMonitorImpl;
import cern.laser.business.pojo.CategoryDefinitionServiceImpl;
import cern.laser.business.pojo.CoreServiceImpl;
import cern.laser.business.pojo.HeartbeatImpl;
import cern.laser.business.pojo.MailAndSmsServerImpl;
import cern.laser.business.pojo.SourceDefinitionServiceImpl;
import cern.laser.business.definition.data.CategoryDefinition;
import cern.laser.business.definition.data.SourceDefinition;
import cern.laser.business.definition.data.AlarmDefinition;
import cern.laser.business.ProcessingController;

import com.cosylab.acs.jms.ACSJMSTopic;
import com.cosylab.acs.jms.ACSJMSTopicConnectionFactory;
import com.cosylab.acs.laser.dao.ACSAdminUserDAOImpl;
import com.cosylab.acs.laser.dao.ACSAlarmCacheImpl;
import com.cosylab.acs.laser.dao.ACSAlarmDAOImpl;
import com.cosylab.acs.laser.dao.ACSCategoryDAOImpl;
import com.cosylab.acs.laser.dao.ACSResponsiblePersonDAOImpl;
import com.cosylab.acs.laser.dao.ACSSourceDAOImpl;
import com.cosylab.acs.laser.dao.ConfigurationAccessor;
import com.cosylab.acs.laser.dao.ConfigurationAccessorFactory;


public class LaserComponent extends ComponentImplBase
		implements AlarmServiceOperations {
	ContainerServices contSvcs;
	
	Logger logger=null;

	ACSAdminUserDAOImpl adminUserDAO;

	ACSAlarmDAOImpl alarmDAO;

	ACSCategoryDAOImpl categoryDAO;

	ACSResponsiblePersonDAOImpl responsiblePersonDAO;

	ACSSourceDAOImpl sourceDAO;

	AdminUserDefinitionServiceImpl adminUserDefinitionService;

	AlarmCacheServerImpl alarmCacheServer;

	AlarmDefinitionServiceImpl alarmDefinitionService;

	AlarmMessageProcessorImpl alarmMessageProcessor;

	AlarmPublisherImpl alarmPublisher;

	AlarmSourceMonitorImpl alarmSourceMonitor;

	CategoryDefinitionServiceImpl categoryDefinitionService;

	CoreServiceImpl coreService;

	HeartbeatImpl heartbeat;

	MailAndSmsServerImpl mailAndSmsServer;

	SourceDefinitionServiceImpl sourceDefinitionService;

	AlarmCacheListener alarmCacheListener;

	ACSAlarmCacheImpl alarmCache;

	ACSJMSTopicConnectionFactory defaultTopicConnectionFactory;

	public void initialize(ContainerServices cont)
			throws ComponentLifecycleException {
		super.initialize(cont);
		if (cont==null) {
			throw new ComponentLifecycleException("Invalid null ContainerServices in initialize");
		}
		this.contSvcs = cont;
		this.logger=contSvcs.getLogger();
		
		defaultTopicConnectionFactory = new ACSJMSTopicConnectionFactory(cont);

		TopicConnection tc;
		TopicSession ts;
		
		try {
			tc = defaultTopicConnectionFactory
					.createTopicConnection();

			ts = tc.createTopicSession(false,
					Session.AUTO_ACKNOWLEDGE);

			Topic topicAdminCacheLoader = new ACSJMSTopic(
					"CMW.ALARM_SYSTEM.ADMIN_CACHE_LOADER");

			TopicSubscriber subscriberAdminCacheLoader = ts
					.createSubscriber(topicAdminCacheLoader);

			subscriberAdminCacheLoader
					.setMessageListener(new MessageListener() {
						public void onMessage(Message message) {
							if (message instanceof TextMessage) {
								try {
									logger.log(AcsLogLevel.DEBUG,"Received a JMS message");
								} catch(Exception e) {}
							} else {
								logger.log(AcsLogLevel.DEBUG,"Received a non text JMS message");
							}
							try {
								alarmMessageProcessor.process(message);
							} catch (Exception e) {
								logger.log(AcsLogLevel.ERROR," *** Exception processing a message:"+e.getMessage());
								// XXX what to do???
							}
						}
					});
			logger.log(AcsLogLevel.DEBUG,"JMS initialized");
		} catch (JMSException e) {
			logger.log(AcsLogLevel.ERROR,"Error initializing JMS");
			throw new ComponentLifecycleException("Failed to initialize JMS", e);
		}
		

		ConfigurationAccessor conf;
		try {
			conf = ConfigurationAccessorFactory.getInstance(cont);
		} catch (AcsJContainerServicesEx e) {
			throw new ComponentLifecycleException("Failed to get CDB", e);
		}

		adminUserDAO = new ACSAdminUserDAOImpl();
		alarmDAO = new ACSAlarmDAOImpl(logger);
		categoryDAO = new ACSCategoryDAOImpl(logger,alarmDAO);
		responsiblePersonDAO = new ACSResponsiblePersonDAOImpl();
		
		adminUserDefinitionService = new AdminUserDefinitionServiceImpl();
		alarmCacheServer = new AlarmCacheServerImpl();
		alarmDefinitionService = new AlarmDefinitionServiceImpl();
		alarmMessageProcessor = new AlarmMessageProcessorImpl();
		alarmPublisher = new AlarmPublisherImpl();
		alarmSourceMonitor = new AlarmSourceMonitorImpl();
		categoryDefinitionService = new CategoryDefinitionServiceImpl();
		coreService = new CoreServiceImpl();
		heartbeat = new HeartbeatImpl();
		mailAndSmsServer = new MailAndSmsServerImpl();
		sourceDefinitionService = new SourceDefinitionServiceImpl();
		alarmCacheListener = new AlarmCacheListenerImpl(alarmCacheServer);
		alarmCache = new ACSAlarmCacheImpl(alarmDAO, alarmCacheListener);

		alarmDAO.setConfAccessor(conf);
		alarmDAO.setSurveillanceAlarmId("SURVEILLANCE:SOURCE:1");
		alarmDAO.setResponsiblePersonDAO(responsiblePersonDAO);

		categoryDAO.setConfAccessor(conf);
//		categoryDAO.setCategoryTreeRoot("ACS");
		categoryDAO.setCategoryTreeRoot("ROOT");
		categoryDAO.setSurveillanceCategoryPath("ACS.SURVEILLANCE");

		responsiblePersonDAO.setAlarmDAO(alarmDAO);

		

		adminUserDefinitionService.setCategoryDAO(categoryDAO);
		adminUserDefinitionService.setAdminUserDAO(adminUserDAO);

		alarmCacheServer.setAlarmDAO(alarmDAO);
		alarmCacheServer.setAlarmPublisher(alarmPublisher);
		alarmCacheServer.setMailAndSmsServer(mailAndSmsServer);

		alarmDefinitionService.setAlarmCache(alarmCache);
		alarmDefinitionService.setAdminUserDAO(adminUserDAO);
		alarmDefinitionService.setAlarmDAO(alarmDAO);
		alarmDefinitionService.setCategoryDAO(categoryDAO);
		alarmDefinitionService.setResponsiblePersonDAO(responsiblePersonDAO);
		alarmDefinitionService.setSourceDAO(sourceDAO);
		alarmDefinitionService.setAlarmMessageProcessor(alarmMessageProcessor);
		alarmDefinitionService.setAlarmPublisher(alarmPublisher);

		alarmMessageProcessor.setAlarmCache(alarmCache);
		alarmMessageProcessor.setSourceDAO(sourceDAO);

		alarmPublisher.setTopicConnectionFactory(defaultTopicConnectionFactory);
		alarmPublisher.setCategoryRootTopic("CMW.ALARM_SYSTEM.CATEGORIES");

		alarmSourceMonitor.setSourceDAO(sourceDAO);
		alarmSourceMonitor.setAlarmMessageProcessor(alarmMessageProcessor);
		alarmSourceMonitor.setSourceMonitorFrequency(60000);

		categoryDefinitionService.setAlarmCache(alarmCache);
		categoryDefinitionService.setAdminUserDAO(adminUserDAO);
		categoryDefinitionService.setAlarmDAO(alarmDAO);
		categoryDefinitionService.setCategoryDAO(categoryDAO);

		coreService.setAlarmCache(alarmCache);
		coreService.setResponsiblePersonDAO(responsiblePersonDAO);
		coreService.setSourceDAO(sourceDAO);
		coreService.setAlarmDAO(alarmDAO);
		coreService.setCategoryDAO(categoryDAO);
		coreService.setAlarmPublisher(alarmPublisher);
		// coreService.setDataSource(defaultDataSource);
		coreService.setClientRootTopic("CMW.ALARM_SYSTEM.CLIENTS");
		coreService.setRootCategoryPK(2064926);
		coreService.setHeartbeatTopic("CMW.ALARM_SYSTEM.HEARTBEAT");
		coreService.setHeartbeatFrequency(60000);
		coreService.setSearchRootTopic("CMW.ALARM_SYSTEM.SEARCH");

		heartbeat.setCoreService(coreService);
		heartbeat.setTopicConnectionFactory(defaultTopicConnectionFactory);

		sourceDefinitionService.setAlarmCache(alarmCache);
		sourceDefinitionService.setAdminUserDAO(adminUserDAO);
		sourceDefinitionService.setAlarmDAO(alarmDAO);
		sourceDefinitionService.setCategoryDAO(categoryDAO);
		sourceDefinitionService.setResponsiblePersonDAO(responsiblePersonDAO);
		sourceDefinitionService.setSourceDAO(sourceDAO);
		sourceDefinitionService
				.setAlarmDefinitionService(alarmDefinitionService);

		
		try {
			alarmDAO.loadAlarms();
		} catch (Exception ace) {
			throw new ComponentLifecycleException("Error initializing alarms",ace);
		}
		try {
			categoryDAO.loadCategories();
		} catch (Exception ex) {
			throw new ComponentLifecycleException("Error loading categories",ex);
		}
		sourceDAO = new ACSSourceDAOImpl(logger,alarmDAO.getSources());
		sourceDAO.setConfAccessor(conf);
		sourceDAO.setLaserSourceId("LASER");
		sourceDAO.setAlarmDAO(alarmDAO);
		sourceDAO.setResponsiblePersonDAO(responsiblePersonDAO);
		
		String[] allSources=sourceDAO.getAllSourceIDs();
		if (allSources!=null) {
			for (int a=0; a<allSources.length; a++) {
				Topic topicAdminCacheLoader = new ACSJMSTopic(
				"CMW.ALARM_SYSTEM.ALARMS.SOURCES."+allSources[a]);

				System.out.println("CMW.ALARM_SYSTEM.ALARMS.SOURCES."+allSources[a]);
				
				try {
					TopicSubscriber subscriberAdminCacheLoader = ts
							.createSubscriber(topicAdminCacheLoader);
			
					subscriberAdminCacheLoader
							.setMessageListener(new MessageListener() {
								public void onMessage(Message message) {
									if (message instanceof TextMessage) {
										try {
											logger.log(AcsLogLevel.DEBUG,"Received a JMS message");
										} catch(Exception e) {}
									} else {
										logger.log(AcsLogLevel.DEBUG,"Received a non text JMS message");
									}
									try {
										alarmMessageProcessor.process(message);
									} catch (Exception e) {
										// XXX what to do???
										logger.log(AcsLogLevel.ERROR," *** Exception processing a message:"+e.getMessage());
										e.printStackTrace();
									}
								}
							});
				} catch (JMSException e) {
					throw new ComponentLifecycleException("Failed to init JMS listeners: "+e.getMessage(), e);
				}
			}
		}
	}

	public void execute() throws ComponentLifecycleException {
		super.execute();
		heartbeat.start();
		ProcessingController.getInstance().startProcessing();
	}

	public void cleanUp() {
		heartbeat.stop();
		super.cleanUp();
	}

	public void aboutToAbort() {
		heartbeat.stop();
		super.aboutToAbort();
	}
	
	/************************** CoreService **************************/
	
	
	/**
	 * Helper method which converts EJB business Category to CORBA Category structure.
	 * @param category
	 * @return
	 */
	private static Category fromBusinessCategory(cern.laser.business.data.Category category)
	{
		return new Category(category.getCategoryId().intValue(),
				category.getName(), category.getDescription(),
				category.getPath(),	category.isLeaf());
	}
	
	/**
	 * Helper method.
	 * @param categories
	 * @return
	 */
	private static Category[] fromBusinessCategoryCollection(Collection categories) {
		if (categories == null)
			return new Category[0];

		Category[] retVal = new Category[categories.size()];
        int pos = 0;
        for (Iterator iter = categories.iterator(); iter.hasNext(); )
        {
        	cern.laser.business.data.Category category = (cern.laser.business.data.Category)iter.next();
        	retVal[pos++] = fromBusinessCategory(category);
        }
		return retVal;
	}


	/**
	 * Helper method which converts EJB business ResponsiblePerson to CORBA ResponsiblePerson structure.
	 * @param person
	 * @return
	 */
	private static ResponsiblePerson fromBusinessResponsiblePerson(cern.laser.business.data.ResponsiblePerson person)
	{
		return new ResponsiblePerson(person.getResponsibleId().intValue(),
				person.getFirstName(), person.getFamilyName(),
				person.getEMail(), person.getGsmNumber(),
				person.getPhoneNumber());
	}
	
	/**
	 * Helper method which converts EJB business Source to CORBA Source structure.
	 * @param source
	 * @return
	 */
	private static Source fromBusinessSource(cern.laser.business.data.Source source)
	{
		return new Source(source.getSourceId(), source.getDescription(),
						  fromBusinessResponsiblePerson(source.getResponsiblePerson()));
	}

	/**
	 * Helper method which converts EJB business Alarm to CORBA Alarm structure.
	 * @param alarm
	 * @return
	 */
	private static Alarm fromBusinessAlarm(cern.laser.business.data.Alarm alarm)
	{
		cern.laser.business.data.Triplet bt = alarm.getTriplet();
		cern.laser.business.data.Location bl = alarm.getLocation();
		cern.laser.business.data.Status bs = alarm.getStatus();
		
		return new Alarm(
				alarm.getAlarmId(),
				new Triplet(bt.getFaultFamily(),
							bt.getFaultMember(),
							bt.getFaultCode().intValue()),
				alarm.getSystemName(),
				alarm.getIdentifier(),
				alarm.getProblemDescription(),
				alarm.getPriority().intValue(),
				alarm.getCause(),
				alarm.getAction(),
				alarm.getConsequence(),
				fromBusinessSource(alarm.getSource()),
				alarm.getHelpURL().toExternalForm(),
				alarm.getPiquetGSM(),
				alarm.getPiquetEmail(),
				fromBusinessResponsiblePerson(alarm.getResponsiblePerson()),
				new Location(bl.getFloor(),
							 bl.getRoom(),
							 bl.getPosition(),
							 bl.getMnemonic(),
							 bl.getBuilding().getBuildingNumber(),
							 bl.getBuilding().getSite(),
							 bl.getBuilding().getZone().intValue(),
							 bl.getBuilding().getMap()),
				fromBusinessCategoryCollection(alarm.getCategories()),
				new Status(bs.getActive().booleanValue(),
						   bs.getMasked().booleanValue(),
						   bs.getReduced().booleanValue(),
						   new Timestamp(bs.getSourceTimestamp().getTime(),
						   				 bs.getSourceTimestamp().getNanos()),
						   bs.getSourceHostname(),
						   new Timestamp(bs.getUserTimestamp().getTime(),
						   				 bs.getUserTimestamp().getNanos()),
						   new Timestamp(bs.getSystemTimestamp().getTime(),
						   				 bs.getSystemTimestamp().getNanos()),
						   new Property[0]), // TODO !!!!
				alarm.getInstant().booleanValue(),
				alarm.hasNodeParents(),
				alarm.hasMultiplicityParents(),
				alarm.hasNodeChildren(),
				alarm.hasMultiplicityChildren()
		);
	}

	/**
	 * Helper method.
	 * @param alarms
	 * @return
	 */
	private static Alarm[] fromBusinessAlarmCollection(Collection alarms) {
		if (alarms == null)
			return new Alarm[0];
		
		Alarm[] retVal = new Alarm[alarms.size()];
        int pos = 0;
        for (Iterator iter = alarms.iterator(); iter.hasNext(); )
        {
        	cern.laser.business.data.Alarm alarm = (cern.laser.business.data.Alarm)iter.next();
        	retVal[pos++] = fromBusinessAlarm(alarm);
        }
		return retVal;
	}

	
	
	
	
	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getActiveMultiplicityChildren(java.lang.String)
	 */
	public Alarm[] getActiveMultiplicityChildren(String parentId) {
		Collection alarms = coreService.getActiveMultiplicityChildren(parentId);
		return fromBusinessAlarmCollection(alarms);
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getActiveNodeChildren(java.lang.String)
	 */
	public Alarm[] getActiveNodeChildren(String parentId) {
		Collection alarms = coreService.getActiveNodeChildren(parentId);
		return fromBusinessAlarmCollection(alarms);
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getAlarmById(java.lang.String)
	 */
	public Alarm getAlarmById(String id) {
		cern.laser.business.data.Alarm alarm = coreService.getAlarmById(id);
		return fromBusinessAlarm(alarm);
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getAlarmByTriplet(java.lang.String, java.lang.String, int)
	 */
	public Alarm getAlarmByTriplet(String ff, String fm, int fc) {
		cern.laser.business.data.Alarm alarm = coreService.getAlarmByTriplet(ff, fm, new Integer(fc));
		return fromBusinessAlarm(alarm);
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getAlarmsByCategory(int)
	 */
	public Alarm[] getAlarmsByCategory(int categoryId) {
		Collection alarms = coreService.getAlarmsByCategory(new Integer(categoryId));
		return fromBusinessAlarmCollection(alarms);
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getAlarmsByPriority(int)
	 */
	public Alarm[] getAlarmsByPriority(int priority) {
		Collection alarms = coreService.getAlarmsByPriority(new Integer(priority));
		return fromBusinessAlarmCollection(alarms);
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getAlarmsByResponsiblePerson(int)
	 */
	public Alarm[] getAlarmsByResponsiblePerson(int responsibleId) {
		Collection alarms = coreService.getAlarmsByResponsiblePerson(new Integer(responsibleId));
		return fromBusinessAlarmCollection(alarms);
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getAlarmsBySource(java.lang.String)
	 */
	public Alarm[] getAlarmsBySource(String sourceId) {
		Collection alarms = coreService.getAlarmsBySource(sourceId);
		return fromBusinessAlarmCollection(alarms);
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getCategories()
	 */
	public Category[] getCategories() {
        Collection categories = coreService.getCategories();
        return fromBusinessCategoryCollection(categories);
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getCategoryByPath(java.lang.String)
	 */
	public Category getCategoryByPath(String path) {
        cern.laser.business.data.Category category = coreService.getCategoryByPath(path);
        return fromBusinessCategory(category);
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getCategoryChildren(int)
	 */
	public Category[] getCategoryChildren(int nodeId) {
        Collection categories = coreService.getCategoryChildren(new Integer(nodeId));
        return fromBusinessCategoryCollection(categories);
	}
	

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getCategoryParent(int)
	 */
	public Category getCategoryParent(int nodeId) {
        cern.laser.business.data.Category category = coreService.getCategoryParent(new Integer(nodeId));
        return fromBusinessCategory(category);
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getCategoryRootTopic()
	 */
	public String getCategoryRootTopic() {
		return coreService.getCategoryRootTopic();
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getCategoryTreeRoot()
	 */
	public Category getCategoryTreeRoot() {
        cern.laser.business.data.Category category = coreService.getCategoryTreeRoot();
        return fromBusinessCategory(category);
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getClientRootTopic()
	 */
	public String getClientRootTopic() {
		return coreService.getClientRootTopic();
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getHeartbeatFrequency()
	 */
	public long getHeartbeatFrequency() {
		return coreService.getHeartbeatFrequency();
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getHeartbeatTopic()
	 */
	public String getHeartbeatTopic() {
		return coreService.getHeartbeatTopic();
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getMultiplicityChildren(java.lang.String)
	 */
	public Alarm[] getMultiplicityChildren(String parentId) {
		Collection alarms = coreService.getMultiplicityChildren(parentId);
		return fromBusinessAlarmCollection(alarms);
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getMultiplicityParents(java.lang.String)
	 */
	public Alarm[] getMultiplicityParents(String childId) {
		Collection alarms = coreService.getMultiplicityParents(childId);
		return fromBusinessAlarmCollection(alarms);
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getMultiplicityThreshold(java.lang.String)
	 */
	public int getMultiplicityThreshold(String parentId) {
		return coreService.getMultiplicityThreshold(parentId).intValue();
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getNodeChildren(java.lang.String)
	 */
	public Alarm[] getNodeChildren(String parentId) {
		Collection alarms = coreService.getNodeChildren(parentId);
		return fromBusinessAlarmCollection(alarms);
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getNodeParents(java.lang.String)
	 */
	public Alarm[] getNodeParents(String childId) {
		Collection alarms = coreService.getNodeParents(childId);
		return fromBusinessAlarmCollection(alarms);
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getResponsiblePersons()
	 */
	public ResponsiblePerson[] getResponsiblePersons() {
        Collection responsiblePersons = coreService.getResponsiblePersons();
        ResponsiblePerson[] retVal = new ResponsiblePerson[responsiblePersons.size()];
        int pos = 0;
        for (Iterator iter = responsiblePersons.iterator(); iter.hasNext(); )
        {
        	cern.laser.business.data.ResponsiblePerson responsiblePerson =
        					(cern.laser.business.data.ResponsiblePerson)iter.next();
        	retVal[pos++] = fromBusinessResponsiblePerson(responsiblePerson);
        }
        
        return retVal;
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getSearchRootTopic()
	 */
	public String getSearchRootTopic() {
		return coreService.getSearchRootTopic();
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getSources()
	 */
	public Source[] getSources() {
        Collection sources = coreService.getSources();
        Source[] retVal = new Source[sources.size()];
        int pos = 0;
        for (Iterator iter = sources.iterator(); iter.hasNext(); )
        {
        	cern.laser.business.data.Source source = (cern.laser.business.data.Source)iter.next();
        	retVal[pos++] = fromBusinessSource(source);
        }
        
        return retVal;
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#archiveSearch(int[], java.lang.String, java.lang.String)
	 */
	public void archiveSearch(int[] categoryIds, String sql, String clientId) {
		Integer[] ids = new Integer[categoryIds.length];
		for (int i = 0; i < categoryIds.length; i++)
			ids[i] = new Integer(categoryIds[i]);
		coreService.archiveSearch(ids, sql, clientId);
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#search(int[], java.lang.String, java.lang.String)
	 */
	public void search(int[] categoryIds, String sql, String clientId) {
		Integer[] ids = new Integer[categoryIds.length];
		for (int i = 0; i < categoryIds.length; i++)
			ids[i] = new Integer(categoryIds[i]);
		coreService.search(ids, sql, clientId);
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#select(int[], java.lang.String)
	 */
	public void select(int[] categoryIds, String client) throws LaserProcessingException {
		
		try
		{
			Collection ids = new ArrayList(categoryIds.length);
			for (int i = 0; i < categoryIds.length; i++) {
				ids.add(new Integer(categoryIds[i]));
			}
			coreService.select(ids, client);
		} catch (cern.laser.business.LaserProcessingException lpe) {
			throw new LaserProcessingException(lpe.getMessage());
		}
	}

	 public String createAdminUser(String name, String password) throws alma.alarmsystem.LaserDefinitionException {
		 try
		 {
			 return adminUserDefinitionService.createAdminUser(name, password);
		 }
		 catch (cern.laser.business.definition.LaserDefinitionException e)
		 {
			 throw new alma.alarmsystem.LaserDefinitionException(e.getMessage());
		 }
	}

	public String loginAdminUser(String name, String password)  {
		return adminUserDefinitionService.loginAdminUser(name, password);
	}
	
	public void createCategory(String userId, alma.alarmsystem.CategoryDefinition definition) throws alma.alarmsystem.LaserDefinitionException {
		try
		{	
			categoryDefinitionService.createCategory(userId, new CategoryDefinition(definition.path, definition.description));
		}
		catch (cern.laser.business.definition.LaserDefinitionException e)
		{
			throw new alma.alarmsystem.LaserDefinitionException(e.getMessage());
		}
	}
	
	public void createSource(String userId, alma.alarmsystem.SourceDefinition definition) throws alma.alarmsystem.LaserDefinitionException {
		try
		{
			sourceDefinitionService.createSource(userId, new SourceDefinition(
					definition.name, 
					definition.description, 
					definition.hostName, 
					new Integer(definition.connectionTimeout), 
					new Integer(definition.responsibleId)
				)
			);
		}
		catch (cern.laser.business.definition.LaserDefinitionException e)
		{
			throw new alma.alarmsystem.LaserDefinitionException(e.getMessage());
		}
    }
	
	public void createAlarm(String userId, alma.alarmsystem.AlarmDefinition definition) throws alma.alarmsystem.LaserDefinitionException {
		try
		{
			alarmDefinitionService.createAlarm(userId, new AlarmDefinition(
					definition.faultFamily, 
					definition.faultMember, 
					new Integer(definition.faultCode), 
					definition.systemName,
					definition.identifier, 
					definition.problemDescription, 
					new Integer(definition.priority), 
				    definition.cause, 
				    definition.action, 
				    definition.consequence,
				    new Boolean(definition.instant), 
				    definition.helpURL, 
				    definition.sourceName, 
				    definition.building, 
				    definition.floor, 
				    definition.room, 
				    definition.mnemonic,
				    definition.position, 
				    new Integer(definition.responsiblePersonId), 
				    definition.piquetGSM, 
				    definition.piquetEmail
				)
			);
		}
		catch (cern.laser.business.definition.LaserDefinitionException e)
		{
			throw new alma.alarmsystem.LaserDefinitionException(e.getMessage());
		}
    }
}
