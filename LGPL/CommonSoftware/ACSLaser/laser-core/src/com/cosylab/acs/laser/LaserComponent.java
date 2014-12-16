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
import java.util.Set;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jms.Message;
import javax.jms.MessageListener;
import javax.jms.Session;
import javax.jms.TextMessage;
import javax.jms.Topic;
import javax.jms.TopicConnection;
import javax.jms.TopicSession;
import javax.jms.TopicSubscriber;

import org.omg.CORBA.Any;
import org.omg.CosPropertyService.Property;

import alma.ACSErrTypeCommon.BadParameterEx;
import alma.ACSErrTypeCommon.UnexpectedExceptionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJBadParameterEx;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalArgumentEx;
import alma.ACSErrTypeCommon.wrappers.AcsJUnexpectedExceptionEx;
import alma.Logging.IllegalLogLevelsEx;
import alma.Logging.LoggerDoesNotExistEx;
import alma.Logging.LoggingConfigurablePackage.LogLevels;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.ClientLogManager;
import alma.acs.logging.config.LogConfig;
import alma.acs.logging.config.LogConfigException;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.acs.util.UTCUtility;
import alma.alarmsystem.Alarm;
import alma.alarmsystem.CERNAlarmServicePOA;
import alma.alarmsystem.Category;
import alma.alarmsystem.LaserProcessingException;
import alma.alarmsystem.Location;
import alma.alarmsystem.ResponsiblePerson;
import alma.alarmsystem.Source;
import alma.alarmsystem.Status;
import alma.alarmsystem.Timestamp;
import alma.alarmsystem.Triplet;
import alma.alarmsystem.corbaservice.AlarmSystemContainerServices;
import alma.acs.alarmsystem.corbaservice.AlarmSystemCorbaServer;
import alma.acstime.Epoch;
import alma.acstime.EpochHelper;
import alma.alarmsystem.core.alarms.LaserCoreFaultState;
import alma.alarmsystem.core.alarms.LaserCoreFaultState.LaserCoreFaultCodes;
import alma.alarmsystem.core.mail.ACSMailAndSmsServer;
import alma.alarmsystem.source.ACSFaultState;
import alma.alarmsystem.statistics.StatsCalculator;
import alma.maci.loggingconfig.UnnamedLogger;

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
import cern.laser.business.data.Building;
import cern.laser.business.definition.data.CategoryDefinition;
import cern.laser.business.definition.data.SourceDefinition;
import cern.laser.business.definition.data.AlarmDefinition;
import cern.laser.business.ProcessingController;
import cern.laser.source.alarmsysteminterface.impl.configuration.ASIConfiguration;
import cern.laser.source.alarmsysteminterface.impl.message.ASIMessage;
import cern.laser.source.alarmsysteminterface.impl.message.FaultState;
import cern.laser.source.alarmsysteminterface.impl.message.FaultStates;
import cern.laser.source.alarmsysteminterface.impl.ASIMessageHelper;
import cern.laser.source.alarmsysteminterface.impl.AlarmSystemInterfaceProxy;
import cern.laser.source.alarmsysteminterface.impl.Configurator;
import cern.laser.source.alarmsysteminterface.impl.FaultStateImpl;
import cern.laser.source.alarmsysteminterface.impl.XMLMessageHelper;

import com.cosylab.acs.jms.ACSJMSTextMessage;
import com.cosylab.acs.jms.ACSJMSTopic;
import com.cosylab.acs.jms.ACSJMSTopicConnectionFactory;
import com.cosylab.acs.laser.AlarmSourcesListener.SourceListener;
import com.cosylab.acs.laser.dao.ACSAdminUserDAOImpl;
import com.cosylab.acs.laser.dao.ACSAlarmCacheImpl;
import com.cosylab.acs.laser.dao.ACSAlarmDAOImpl;
import com.cosylab.acs.laser.dao.ACSCategoryDAOImpl;
import com.cosylab.acs.laser.dao.ACSResponsiblePersonDAOImpl;
import com.cosylab.acs.laser.dao.ACSSourceDAOImpl;
import com.cosylab.acs.laser.dao.ConfigurationAccessor;
import com.cosylab.acs.laser.dao.ConfigurationAccessorFactory;

import alma.acs.util.IsoDateFormat;


public class LaserComponent extends CERNAlarmServicePOA implements SourceListener {
	
	/**
	 * A class to terminate the alarm service asynchronously.
	 * <P>
	 * The alarm service is stopped by calling the shutdown IDL method.
	 * But inside such a method, the ORB can't be closed.
	 * This class shuts down the servant outside of the ORB thread.
	 * 
	 * @author acaproni
	 *
	 */
	public class LaserComponentTerminator implements Runnable {
		public void run() {
			statistcsCalculator.shutdown();
			sourcesListener.shutdown();
			alarmCacheListener.close();
			alarmCacheListener=null;
			alarmSourceMonitor.stop();
			heartbeat.stop();
			alarmMessageProcessor.stop();
			corbaServer.shutdown();
			logger.log(AcsLogLevel.DEBUG,"See you soon :-)");
			
		}
	}
	
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
	
	/**
	 * If an error happens during initialization, the alarm service
	 * sends one or more alarms.
	 * <P>
	 * In case of errors during startup <code>initialize()</code> adds alarm
	 * in this vector.
	 * When the component becomes operation, <code>execute()</code> send all the
	 * alarms in the <code>coreAlarms</code> vector.
	 */
	private Vector<LaserCoreFaultCodes> coreAlarms = new Vector<LaserCoreFaultCodes>(); 
	
	/**
	 * The subscribers to the sources NC
	 */
	private AlarmSourcesListenerCached sourcesListener;
	
	/**
	 * The CORBA server
	 */
	private final AlarmSystemCorbaServer corbaServer;
	
	/**
	 * The implementation of the {@link ContainerServicesBase}
	 */
	private final AlarmSystemContainerServices alSysContSvcs;
	
	/**
	 * Set to <code>true</code> if the alarm service has been shut down
	 */
	private volatile boolean closed=false;
	
	private StatsCalculator statistcsCalculator; 
	
	/**
	 * Constructor
	 */
	public LaserComponent(AlarmSystemCorbaServer corbaServer, AlarmSystemContainerServices alSysContSvcs) throws Exception {
		if (corbaServer==null) {
			throw new IllegalArgumentException("The AlarmSystemCorbaServer can't be null");
		}
		if (alSysContSvcs==null) {
			throw new IllegalArgumentException("The AlarmSystemContainerServices can't be null");
		}
		this.corbaServer=corbaServer;
		this.alSysContSvcs=alSysContSvcs;
		initialize();
		execute();
	}

	public void initialize() {
		this.logger=corbaServer.getLogger();
		
		statistcsCalculator = new StatsCalculator(this.logger);
		statistcsCalculator.start();
		
		defaultTopicConnectionFactory = new ACSJMSTopicConnectionFactory(alSysContSvcs);

		TopicConnection tc;
		TopicSession ts=null;
		
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
								} catch(Throwable t) {}
							} else {
								logger.log(AcsLogLevel.DEBUG,"Received a non text JMS message");
							}
							try {
								alarmMessageProcessor.process(message);
							} catch (Exception e) {
								logger.log(AcsLogLevel.WARNING," *** Exception processing a message:"+e.getMessage());
								// XXX what to do???
							}
						}
					});
			logger.log(AcsLogLevel.DEBUG,"JMS initialized");
		} catch (Throwable t) {
			System.err.println("Error initing JMS, "+t.getMessage());
			t.printStackTrace(System.err);
			logger.log(AcsLogLevel.ERROR,"Error initializing JMS",t);
			coreAlarms.add(LaserCoreFaultCodes.JMS_INIT);
		}
		

		ConfigurationAccessor conf=null;
		try {
			conf = ConfigurationAccessorFactory.getInstance(alSysContSvcs);
		} catch (Throwable t) {
			System.err.println("Error getting CDB: "+t.getMessage());
			t.printStackTrace(System.err);
			logger.log(AcsLogLevel.WARNING,"Error getting CDB",t);
			coreAlarms.add(LaserCoreFaultCodes.CDB_UNAVAILABLE);
		}

		adminUserDAO = new ACSAdminUserDAOImpl();
		alarmDAO = new ACSAlarmDAOImpl(logger);
		alarmDAO.setConfAccessor(conf);
		categoryDAO = new ACSCategoryDAOImpl(logger,alarmDAO);
		categoryDAO.setConfAccessor(conf);
		responsiblePersonDAO = new ACSResponsiblePersonDAOImpl();
		
		try {
			alarmDAO.loadAlarms();
		} catch (Throwable t) {
			System.err.println("Error loading alarms: "+t.getMessage());
			t.printStackTrace(System.err);
			logger.log(AcsLogLevel.CRITICAL,"Error loading alarms from CDB",t);
			coreAlarms.add(LaserCoreFaultCodes.ALARMS_CDB);
		}
		try {
			categoryDAO.loadCategories();
		} catch (Throwable t) {
			System.err.println("Error loading categories: "+t.getMessage());
			t.printStackTrace(System.err);
			logger.log(AcsLogLevel.CRITICAL,"Error loading categories from CDB",t);
			coreAlarms.add(LaserCoreFaultCodes.CATEGORIES_CDB);
		}
		sourceDAO = new ACSSourceDAOImpl(logger,alarmDAO.getSources());
		sourceDAO.setConfAccessor(conf);
		sourceDAO.setLaserSourceId("LASER");
		sourceDAO.setAlarmDAO(alarmDAO);
		sourceDAO.setResponsiblePersonDAO(responsiblePersonDAO);
		
		adminUserDefinitionService = new AdminUserDefinitionServiceImpl();
		alarmCacheServer = new AlarmCacheServerImpl();
		alarmDefinitionService = new AlarmDefinitionServiceImpl();
		alarmMessageProcessor = new AlarmMessageProcessorImpl(this);
		alarmPublisher = new AlarmPublisherImpl(logger);
		alarmSourceMonitor = new AlarmSourceMonitorImpl();
		categoryDefinitionService = new CategoryDefinitionServiceImpl();
		coreService = new CoreServiceImpl();
		heartbeat = new HeartbeatImpl();
		mailAndSmsServer = new ACSMailAndSmsServer(logger);
		sourceDefinitionService = new SourceDefinitionServiceImpl();
		alarmCacheListener = new AlarmCacheListenerImpl(alarmCacheServer);
		alarmCache = new ACSAlarmCacheImpl(alarmDAO, categoryDAO,alarmCacheListener,logger);
		
		alarmDAO.setSurveillanceAlarmId("SURVEILLANCE:SOURCE:1");
		alarmDAO.setResponsiblePersonDAO(responsiblePersonDAO);
		
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
		sourceDefinitionService.setAlarmDefinitionService(alarmDefinitionService);

		alarmDAO.setAlarmProcessor(alarmMessageProcessor);
		
		// Subscribe to all the source NCs
		sourcesListener = new AlarmSourcesListenerCached(alSysContSvcs,logger,this);
		sourcesListener.start();
		try {
			sourcesListener.connectSources(sourceDAO.getAllSourceIDs());
			logger.log(AcsLogLevel.DEBUG,"Successfully connected to sources NCs"); 
		} catch (Throwable t) {
			System.err.println("Error setting source listener: "+t.getMessage());
			t.printStackTrace(System.err);
			logger.log(AcsLogLevel.WARNING,"Error setting the source listener",t);
			coreAlarms.add(LaserCoreFaultCodes.SOURCE_LISTENER);
		}
	}
	
	/**
	 * Shutdown the alarm service
	 */
	public synchronized void shutdown() {
		if (closed) {
			return;
		}
		closed=true;
		logger.log(AcsLogLevel.DEBUG,"Shutting down");
		
		Thread t = new Thread(new LaserComponentTerminator(),"LaserComponentTerminator");
		t.start();
	}
	
	/**
	 * Send a core alarm in a dedicated thread.
	 * <P>
	 * This method is useful when a core alarm must be sent without waiting
	 * for its processing to be completed by the alarm message processor.
	 * <BR>A typical example is a core alarm generated by the alarm
	 * message processor itself.
	 * <P>
	 * sendCoreAlarmAsync delegates to {@link #sentCoreAlarms(Vector)}
	 * that is invoked in a dedicated thread.
	 * 
	 * @param alarm The core alarm to send
	 * @param <code>true</code> if the alarm must be set and <code>false</code> otherwise
	 */
	public void sendCoreAlarmAsync(final LaserCoreFaultCodes alarm, final boolean active) {
		
		Thread thread = new Thread(new Runnable() {
			public void run() {
				cern.laser.source.alarmsysteminterface.FaultState faultState= LaserCoreFaultState.createFaultState(alarm, active);
				TextMessage msg;
				try {
					msg= LaserCoreFaultState.createJMSMessage(faultState, alSysContSvcs);
				} catch (Throwable t) {
					System.err.println("Error creating a core alarm of type "+alarm);
					t.printStackTrace(System.err);
					logger.log(AcsLogLevel.ERROR,"Error creating a core alarm of type "+alarm);
					return;
				}
				try {
					onMessage(msg.getText());
				} catch (Throwable t) {
					logger.log(AcsLogLevel.ERROR,"Error processing core alarm: "+alarm);
				}
			}
		});
		thread.setDaemon(true);
		thread.start();
	}
	
	/**
	 * Send alarm services alarms and return when all the alarms 
	 * have been processed by the alarm message processor. 
	 * <P>
	 * The alarms generated by the alarm service are injected in the
	 * component by executing the <code>onMessage</code>
	 * 
	 * @param alarms The alarms to send 
	 *  			 <code>alarms</code> can be <code>null</code> or empty;
	 */
	public void sentCoreAlarms(Vector<LaserCoreFaultCodes> alarms) {
		if (alarms==null || alarms.isEmpty()) {
			return;
		}
		for (LaserCoreFaultCodes alarm: alarms) {
			logger.log(AcsLogLevel.ALERT, "Laser core alarm <"+LaserCoreFaultState.FaultFamily+", "+LaserCoreFaultState.FaultMember+", "+alarm.faultCode+">");
			cern.laser.source.alarmsysteminterface.FaultState fs = LaserCoreFaultState.createFaultState(alarm, true);
			TextMessage msg;
			try {
				msg= LaserCoreFaultState.createJMSMessage(fs, alSysContSvcs);
			} catch (Throwable t) {
				System.err.println("Error creating a core alarm of type "+alarm);
				t.printStackTrace(System.err);
				logger.log(AcsLogLevel.ERROR,"Error creating a core alarm of type "+alarm);
				continue;
			}
			try {
				onMessage(msg.getText());
			} catch (Throwable t) {
				logger.log(AcsLogLevel.ERROR,"Error processing core alarm: "+alarm);
			}
		}
	}
	
	/**
	 * @see MessageListener
	 */
	@Override
	public synchronized void onMessage(String message) {
		try {
			alarmMessageProcessor.process(message);
		} catch (Exception e) {
			// XXX what to do???
			logger.log(AcsLogLevel.ERROR," *** Exception processing a message:"+e.getMessage(),e);
			e.printStackTrace();
		}
	}

	public void execute() throws ComponentLifecycleException {
		heartbeat.start();
		alarmMessageProcessor.start();
		ProcessingController.getInstance().startProcessing();
		sentCoreAlarms(coreAlarms);
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
	
	private static String getString(String str) {
		return (str!=null)?str:"";
	}

	/**
	 * Helper method which converts EJB business Alarm to CORBA Alarm structure.
	 * @param alarm
	 * @return
	 */
	private Alarm fromBusinessAlarm(cern.laser.business.data.Alarm alarm)
	{
		cern.laser.business.data.Triplet bt = alarm.getTriplet();
		cern.laser.business.data.Location bl = alarm.getLocation();
		if (bl.getBuilding()==null) {
			bl.setBuilding(new Building("","",1,""));
		}
		cern.laser.business.data.Status bs = alarm.getStatus();
		
		// Build the properties
		Property[] props;
		if (alarm.getStatus().getProperties()!=null) {
			props = new Property[alarm.getStatus().getProperties().size()];
			Set keys = alarm.getStatus().getProperties().keySet();
			int t=0;
			for (Object key: keys) {
				String name = (String)key;
				String value = alarm.getStatus().getProperties().getProperty(name);
				Any any=corbaServer.getORB().create_any();
				any.insert_string(value);
				props[t++] = new Property(name,any);
			}
		} else {
			props = new Property[0];
		}
		
		Alarm newAlarm = new Alarm(
				alarm.getAlarmId(),
				new Triplet(bt.getFaultFamily(),
							bt.getFaultMember(),
							bt.getFaultCode().intValue()),
				getString(alarm.getSystemName()),
				getString(alarm.getIdentifier()),
				getString(alarm.getProblemDescription()),
				alarm.getPriority().intValue(),
				getString(alarm.getCause()),
				getString(alarm.getAction()),
				getString(alarm.getConsequence()),
				fromBusinessSource(alarm.getSource()),
				getString(alarm.getHelpURL().toExternalForm()),
				getString(alarm.getPiquetGSM()),
				getString(alarm.getPiquetEmail()),
				fromBusinessResponsiblePerson(alarm.getResponsiblePerson()),
				new Location(
						bl.getLocationId(),
						bl.getFloor(),
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
						   props), 
				alarm.getInstant().booleanValue(),
				alarm.hasNodeParents(),
				alarm.hasMultiplicityParents(),
				alarm.hasNodeChildren(),
				alarm.hasMultiplicityChildren()
		);
		return newAlarm;
	}

	/**
	 * Helper method.
	 * @param alarms
	 * @return
	 */
	private Alarm[] fromBusinessAlarmCollection(Collection alarms) {
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
		Alarm[] als=fromBusinessAlarmCollection(alarms);
		return als;
	}

	/* (non-Javadoc)
	 * @see alma.alarmsystem.CoreServiceOperations#getMultiplicityParents(java.lang.String)
	 */
	public Alarm[] getMultiplicityParents(String childId) {
		Collection alarms = coreService.getMultiplicityParents(childId);
		Alarm[] als=fromBusinessAlarmCollection(alarms);
		return als;
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
        Collection<cern.laser.business.data.Source> sources = coreService.getSources();
        Source[] retVal = new Source[sources.size()];
        int pos = 0;
        for (cern.laser.business.data.Source source: sources )
        {
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
	
	public boolean isACSAlarmService() {
		return false;
	}
	
	/**
	 * IDL method: submit an alarm without.
	 * <P>
	 * Build a message to sent to the {@link AlarmMessageProcessorImpl#process(Message)}.
	 * 
	 * @param triplet The triplet of the alarm
	 * @param active if <code>true</code> the alarm is active
	 * @param sourceHostName The name of the host of the source
	 * @param timestamp The timestamp of the source
	 * @param alarmProperties Additional user-defined properties of the alarm
	 */
	public synchronized void submitAlarm(
			Triplet triplet,
			boolean active,
			String sourceHostName,
			String sourceName,
			long timestamp,
			Property[] alarmProperties) throws BadParameterEx, UnexpectedExceptionEx {
		cern.laser.source.alarmsysteminterface.impl.message.FaultState fs = new cern.laser.source.alarmsysteminterface.impl.message.FaultState();
		fs.setCode(triplet.faultCode);
		fs.setMember(triplet.faultMember);
		fs.setFamily(triplet.faultFamily);
		logger.log(AcsLogLevel.DEBUG, "Submitting alarm <"+triplet.faultFamily+
				", "+triplet.faultMember+
				", "+triplet.faultCode+"> active="+active);
		// Properties
		cern.laser.source.alarmsysteminterface.impl.message.Properties props = new cern.laser.source.alarmsysteminterface.impl.message.Properties();
		if (alarmProperties!=null) {
			for (Property p: alarmProperties) {
				cern.laser.source.alarmsysteminterface.impl.message.Property propToAdd = new cern.laser.source.alarmsysteminterface.impl.message.Property();
				propToAdd.setName(p.property_name);
				propToAdd.setValue(p.property_value.toString());
				props.addProperty(propToAdd);
			}
		}
		fs.setUserProperties(props);
		// Timestamp
		long javaTime = UTCUtility.utcOmgToJava(timestamp);
		long seconds = javaTime/1000;
		long milliseconds = javaTime % 1000;
		
		String tStamp = IsoDateFormat.formatCurrentDate();
		fs.setUserTimestamp(tStamp);
		// Descriptor
		if (active) {
			fs.setDescriptor(ACSFaultState.ACTIVE);
		} else {
			fs.setDescriptor(ACSFaultState.TERMINATE);
		}
		// Build the message
		TextMessage message;
		try {
			message= buildMessage(fs, sourceHostName, sourceName);
		} catch (IllegalArgumentException ie) {
			AcsJBadParameterEx ex = new AcsJBadParameterEx(ie);
			throw ex.toBadParameterEx();
			
		} catch (Throwable t) {
			AcsJUnexpectedExceptionEx ex = new AcsJUnexpectedExceptionEx(t);
			throw ex.toUnexpectedExceptionEx();
		}
		// Inject the message
		try {
			onMessage(message.getText());
		} catch (Throwable t) {
			AcsJUnexpectedExceptionEx ex = new AcsJUnexpectedExceptionEx(t);
			throw ex.toUnexpectedExceptionEx();
		}
	}
	
	/**
	 * Build the {@link ACSJMSTextMessage} for a given fault state
	 * 
	 * @param state The fault state
	 * @param hostName The host name
	 * 
	 * @see AlarmSystemInterfaceProxy#publish
	 */
	private TextMessage buildMessage(cern.laser.source.alarmsysteminterface.impl.message.FaultState state, String hostName, String sourceName) throws Exception {
		if (state==null) {
			throw new IllegalArgumentException("The fault state can't be null");
		}
		if (hostName==null || hostName.isEmpty()) {
			throw new IllegalArgumentException("Invalid host name");
		}
		if (sourceName==null || sourceName.isEmpty()) {
			throw new IllegalArgumentException("Invalid source name");
		}
		Collection<FaultStateImpl> tempStates = new Vector<FaultStateImpl>();
		cern.laser.source.alarmsysteminterface.impl.message.FaultState tempState = new FaultState();
	    ASIMessage asi_message = ASIMessageHelper.marshal(tempStates);
	    FaultStates states = new FaultStates();
	    states.addFaultState(state);
	    asi_message.setFaultStates(states);
	    asi_message.setSourceName("ALARM_SYSTEM_SOURCES");
	    asi_message.setSourceHostname(hostName);
	    asi_message.setSourceTimestamp(IsoDateFormat.formatCurrentDate());
	    asi_message.setBackup(false);
	    
	    Configurator configurator = new Configurator();
	    ASIConfiguration configuration = configurator.getConfiguration();
	    asi_message.setVersion(configuration.getASIVersion());

	    ACSJMSTextMessage tm = new ACSJMSTextMessage(alSysContSvcs);
	    tm.setText(XMLMessageHelper.marshal(asi_message));
	    tm.setStringProperty(configuration.getSourceNameProperty(), sourceName);
	    tm.setStringProperty(configuration.getSourceHostnameProperty(), hostName);
	    tm.setStringProperty(configuration.getBackupProperty(), String.valueOf(false));
	    tm.setStringProperty(configuration.getAlarmsNumberProperty(), String.valueOf(1));

	    return tm;
	}
	
    // ///////////////////////////////////////////////////////////
	// LoggingConfigurable interface
	// ///////////////////////////////////////////////////////////

	// init logging 
	LogConfig logConfig = ClientLogManager.getAcsLogManager().getLogConfig();

	/**
	 * Gets the log levels of the default logging configuration. These levels
	 * are used by all loggers that have not been configured individually.
	 */
	public LogLevels get_default_logLevels() {
		LogLevels logLevels = new LogLevels();
		logLevels.useDefault = false;
		logLevels.minLogLevel = (short) logConfig.getDefaultMinLogLevel().value;
		logLevels.minLogLevelLocal = (short) logConfig.getDefaultMinLogLevelLocal().value;
		return logLevels;
	}

	/**
	 * Sets the log levels of the default logging configuration. These levels
	 * are used by all loggers that have not been configured individually.
	 */
	public void set_default_logLevels(LogLevels levels) throws IllegalLogLevelsEx {
		try {
			logConfig.setDefaultMinLogLevel(AcsLogLevelDefinition.fromInteger(levels.minLogLevel));
			logConfig.setDefaultMinLogLevelLocal(AcsLogLevelDefinition.fromInteger(levels.minLogLevelLocal));
		} catch (AcsJIllegalArgumentEx ex) {
			//throw ex.toIllegalArgumentEx();
			IllegalLogLevelsEx ille = new IllegalLogLevelsEx(ex.getErrorDesc());
			throw ille;
		}
	}

	/**
	 * Gets the names of all loggers, to allow configuring their levels
	 * individually. The names are those that appear in the log records in the
	 * field "SourceObject". This includes the container logger, ORB logger,
	 * component loggers, and (only C++) GlobalLogger.
	 * <p>
	 * The returned logger names are randomly ordered.
	 */
	public String[] get_logger_names() {
		Set<String> loggerNames = logConfig.getLoggerNames();
		return loggerNames.toArray(new String[loggerNames.size()]);
	}

	/**
	 * Gets log levels for a particular named logger. If the returned field
	 * LogLevels.useDefault is true, then the logger uses the default levels,
	 * see get_default_logLevels(); otherwise the returned local and remote
	 * levels apply.
	 * <p>
	 * For possible convenience, the default levels are returned in addition to 
	 * setting {@link LogLevels#useDefault} to <code>true</code>.
	 */
	public LogLevels get_logLevels(String logger_name) throws LoggerDoesNotExistEx {
		UnnamedLogger xsdLevels = logConfig.getNamedLoggerConfig(logger_name);
		boolean useDefault = !logConfig.hasCustomConfig(logger_name); 
		LogLevels ret = AcsLogLevelDefinition.createIdlLogLevelsFromXsd(useDefault, xsdLevels);
		return ret;
	}

	/**
	 * Sets log levels for a particular named logger. If levels.useDefault is
	 * true, then the logger will be reset to using default levels; otherwise it
	 * will use the supplied local and remote levels.
	 */
	public void set_logLevels(String logger_name, LogLevels levels) throws LoggerDoesNotExistEx, IllegalLogLevelsEx {
		if (levels.useDefault) {
			logConfig.clearNamedLoggerConfig(logger_name);
		}
		else {
			try {
				UnnamedLogger config = AcsLogLevelDefinition.createXsdLogLevelsFromIdl(levels);
				logConfig.setNamedLoggerConfig(logger_name, config);
			} catch (AcsJIllegalArgumentEx ex) {
				//throw ex.toIllegalArgumentEx();
				IllegalLogLevelsEx ille = new IllegalLogLevelsEx(ex.getErrorDesc());
				throw ille;
			}
		}
	}

	/**
	 * Commands the container or manager to read in again the logging
	 * configuration from the CDB and to reconfigure the loggers accordingly.
	 * This allows for persistent changes in the logging configuration to become
	 * effective, and also for changes of more advanced parameters.
	 * <p>
	 * Note that unlike for the logging initialization in {@link #initialize()},
	 * now we give precedence to the CDB values over any previous settings.
	 */
	public void refresh_logging_config() {
		try {
			logConfig.initialize(true);
		} catch (LogConfigException ex) {
			// if the CDB can't be read, we still want to run the container, thus we only log the problem here
			logger.log(Level.FINE, "Failed to configure logging (default values will be used).", ex);
		}
	}


		
	/** ************************ END LoggingConfigurable ************************ */
	
}
