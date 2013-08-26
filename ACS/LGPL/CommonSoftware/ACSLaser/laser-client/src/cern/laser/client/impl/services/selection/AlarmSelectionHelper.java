/*
 * $Id: AlarmSelectionHelper.java,v 1.11 2011/04/13 15:45:42 acaproni Exp $
 *
 * $Date: 2011/04/13 15:45:42 $ 
 * $Revision: 1.11 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.impl.services.selection;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.jms.JMSException;
import javax.jms.Message;
import javax.jms.TextMessage;

import org.apache.log4j.Logger;
import org.omg.CORBA.ORB;

import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogger;
import alma.alarmsystem.CERNAlarmService;

import com.cosylab.acs.jms.ACSJMSTextMessage;

import cern.cmw.mom.pubsub.ExceptionListener;
import cern.cmw.mom.pubsub.MOMException;
import cern.cmw.mom.pubsub.PubSubFactory;
import cern.cmw.mom.pubsub.Subscriber;
import cern.cmw.mom.pubsub.SubscriptionListener;
import cern.laser.client.LaserConnectionException;
import cern.laser.client.LaserException;
import cern.laser.client.LaserTimeOutException;
import cern.laser.client.data.Category;
import cern.laser.client.impl.common.AlarmServiceSingleton;
import cern.laser.client.impl.data.AlarmImpl;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.LaserSelectionException;
import cern.laser.client.services.selection.Selection;
import cern.laser.util.UUIDGenerator;

import alma.alarmsystem.alarmmessage.AlarmMessageConversion;

/**
 * 
 * 
 * @version $Revision: 1.11 $ $Date: 2011/04/13 15:45:42 $
 * @author Katarina Sigerud
 */
class AlarmSelectionHelper implements SubscriptionListener, ExceptionListener {
  private static final Logger LOGGER = Logger.getLogger(AlarmSelectionHelper.class.getName());

  private static final String LASER_INIT_PROPERTY = "LASER_INIT";
  private static final long INIT_DELAY = 1000;
  private static final long INIT_TIMEOUT = 30000;

  private boolean initialized = false;
  private long initWaitTime = 0;

 private Subscriber cmwSubscriber;

  private String clientRootTopic;

  private boolean cmwConnected = true;

  private AlarmSelectionListener selectionListener;
  private Map initialSelection = null;

  private SubscriptionListener initialSelectionListener;

  private String categoryRootTopic;

  // The AlarmService component
  private CERNAlarmService m_laser;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  AlarmSelectionHelper(AlarmSelectionListener selectionListener, ORB orb, AcsLogger logger) throws LaserException {
  	this.selectionListener = selectionListener;
    initialSelection = Collections.synchronizedMap(new HashMap());

    try {
        this.m_laser = AlarmServiceSingleton.getInstance(orb,logger);
    } catch (Exception e) {
      throw new LaserException("unable to setup initial selection", e);
    }

  }

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  public Map subscribe(Selection selection) throws LaserConnectionException, LaserException, LaserTimeOutException {
  	
  	String sql_filter = buildSQLFilter(selection);

    Collection category_ids = setupCategorySubscriptions(selection, sql_filter);

    // start init subscription
    setInitialized(false);

    String console_id = "";
    String host_name = "";
    try {
      console_id = UUIDGenerator.getInstance().getUUID().toString();
      host_name = InetAddress.getLocalHost().getHostName();
    } catch (UnknownHostException e) {
      throw new LaserException("unable to get a unique id for the console", e);
    }
    String init_topic = getClientRootTopic() + "." + console_id;

    String init_sql_filter = buildInitSQLFilter(sql_filter);

    long init_subscription;
    try {
      init_subscription = getSubscriber().subscribe(init_topic, getInitialSelectionListener(), init_sql_filter);
    } catch (Exception e) {
      LOGGER.error("unable to subscribe to topic " + init_topic, e);
      throw new LaserException("unable to subscribe to topic " + init_topic, e);
    }

        try {
          	if (m_laser!=null) {
          		int[] cis = new int[category_ids.size()];
          		int pos = 0;
          		for (Iterator iter = category_ids.iterator(); iter.hasNext(); ) {
          			cis[pos++] = ((Integer)iter.next()).intValue();
          		}
                m_laser.select(cis, console_id);
            } else {
                throw new NullPointerException("AlarmSystem component is null");
            }
        } catch (alma.alarmsystem.LaserProcessingException lpe) {
          try {
            getSubscriber().unSubscribe(init_subscription);
          } catch (JMSException e1) {
            // Intentionally left blank
          } catch (LaserException e1) {
            // Intentionally left blank
          }
          LOGGER.error("server is initializing");
         throw new LaserConnectionException("server is initializing", lpe);
  		} catch (Exception e3) {
  			throw new LaserException("unable to perform initial selection at host " + host_name, e3);
        }

    resetInitWaitTime();
    waitForInit();

    try {
      // stop init subscription
      getSubscriber().unSubscribe(init_subscription);
    } catch (Exception e) {
      LOGGER.info("exception when unsubscribing", e);
    }

    if (isInitialized()) {
      return getInitialSelection();
    } else {
      //throw new LaserTimeOutException("initial selection timed out");
    }
    return getInitialSelection();
  }

  public void resetSelection() throws LaserException {
    try {
      Subscriber subscriber=getSubscriber();
      subscriber.unSubscribeAll();
    } catch (Exception e) {
      throw new LaserException("unable to unsubscribe all");
    }
    if (cmwSubscriber != null) {
      cmwSubscriber.close();
//      cmwSubscriber = null;
    }
  }

  //
  // -- implements ExceptionListener ---------------------------------
  //

  /**
   * DOCUMENT ME!
   * 
   * @param e DOCUMENT ME!
   */
  public void onException(MOMException e) {
    if (e.testException(MOMException.CONNECTION_LOST_EXCEPTION)) {
      cmwConnected = false;
      if (selectionListener != null) {
        selectionListener.onException(new LaserSelectionException(
            cern.laser.client.services.selection.LaserSelectionException.CONNECTION_DROPPED));
      }
    } else {
      if (e.testException(MOMException.CONNECTION_RECOVERED_EXCEPTION)) {
        cmwConnected = true;
        if (selectionListener != null) {
          selectionListener.onException(new LaserSelectionException(
              cern.laser.client.services.selection.LaserSelectionException.CONNECTION_REESTABILISHED));
        }
      }
    }
  }

  //
  // -- implements SubscriptionListener ---------------------------------
  //

  /**
   * DOCUMENT ME!
   * 
   * @param msg DOCUMENT ME!
   */
  public void onMessage(Message msg) {
	  ACSJMSTextMessage acsMsg = (ACSJMSTextMessage)msg;
	  
    if (selectionListener != null) {
      try {
        cern.laser.business.data.AlarmImpl business_alarm = AlarmMessageConversion.getAlarm(((ACSJMSTextMessage)msg).getText());
        AlarmImpl alarm = new AlarmImpl(business_alarm);
        selectionListener.onAlarm(alarm);
      } catch (Exception e) {
        LOGGER.warn("error on message", e);
      }
    }
  }

  //
  // -- PROTECTED METHODS -------------------------------------------
  //

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //

  private void waitForInit() {
    while (!isInitTimedOut()) {
      try {
        Thread.sleep(INIT_DELAY);
      } catch (InterruptedException ie) {
      }
      if (isInitialized()) {
        break;
      }
      increaseInitWaitTime();
    }
  }

  private String buildSQLFilter(Selection selection) {
    String sql_filter = selection.getFilterSelection().toSQLString();
    if (sql_filter == null) {
      if (selection.getReducedMaskedSelection()) {
        sql_filter = "REDUCED_MASKED_SET = TRUE";
      } else {
        sql_filter = "NOT_REDUCED_MASKED_SET = TRUE";
      }
    } else {
      StringBuffer sql_filter_buffer = new StringBuffer();
      sql_filter_buffer.append("(");
      sql_filter_buffer.append(sql_filter);
      sql_filter_buffer.append(")");
      if (selection.getReducedMaskedSelection()) {
        sql_filter_buffer.append(" AND REDUCED_MASKED_SET = TRUE");
      } else {
        sql_filter_buffer.append(" AND NOT_REDUCED_MASKED_SET = TRUE");
      }
      sql_filter = sql_filter_buffer.toString();
    }
    if (LOGGER.isDebugEnabled()) LOGGER.debug("sql filter " + sql_filter);
    return sql_filter;
  }

  private String buildInitSQLFilter(String sql_filter) {
    StringBuffer init_sql_filter_buffer = new StringBuffer();
    init_sql_filter_buffer.append("(");
    init_sql_filter_buffer.append(sql_filter);
    init_sql_filter_buffer.append(") OR ");
    init_sql_filter_buffer.append(LASER_INIT_PROPERTY);
    init_sql_filter_buffer.append(" = TRUE");
    String init_sql_filter = init_sql_filter_buffer.toString();
    if (LOGGER.isDebugEnabled()) LOGGER.debug("init sql filter " + init_sql_filter);
    return init_sql_filter;
  }

  private Collection setupCategorySubscriptions(Selection selection, String sql_filter)
      throws LaserConnectionException, LaserException {
    Category[] categories = selection.getCategorySelection().list();
    Collection category_ids = new ArrayList(categories.length);
    for (int i = 0; i < categories.length; i++) {
      Category category = categories[i];
      String topic = getCategoryRootTopic() + "." + category.getPath();
      if (selectionListener != null) {
        try {
          getSubscriber().subscribe(topic, this, sql_filter);
        } catch (Exception e1) {
          new LaserException("unable to subscribe to topic " + topic, e1);
        }
      }
      category_ids.add(category.getCategoryId());
    }
    return category_ids;
  }

  private synchronized void setInitialized(boolean value) {
    initialized = value;
  }

  private synchronized boolean isInitialized() {
    return initialized;
  }

  private synchronized void increaseInitWaitTime() {
    initWaitTime += INIT_DELAY;
  }

  private void resetInitWaitTime() {
    initWaitTime = 0;
  }

  private boolean isInitTimedOut() {
    return initWaitTime > INIT_TIMEOUT;
  }

  private Map getInitialSelection() {
    Map result = new HashMap(initialSelection);
    initialSelection.clear();

    return result;
  }

  private String getCategoryRootTopic() throws LaserConnectionException, LaserException {
  	if (categoryRootTopic == null) {
        try {
          	if (m_laser!=null) {
          		categoryRootTopic = m_laser.getCategoryRootTopic();
            } else {
                throw new NullPointerException("AlarmSystem component is null");
            }
        } catch (Exception e1) {
          throw new LaserException("unable to find category root topic", e1);
        }
    }

    return categoryRootTopic;
  }

  private String getClientRootTopic() throws LaserException, LaserConnectionException {
  	if (clientRootTopic == null) {
        try {
          	if (m_laser!=null) {
                clientRootTopic = m_laser.getClientRootTopic();
            } else {
                throw new NullPointerException("AlarmSystem component is null");
            }
        } catch (Exception e1) {
          throw new LaserException("unable to find client root topic", e1);
        }
    }

    return clientRootTopic;
  }

  private SubscriptionListener getInitialSelectionListener() {
    if (initialSelectionListener == null) {
      initialSelectionListener = new SubscriptionListener() {
        public void onMessage(Message msg) {
          try {
            if (msg instanceof TextMessage) {
            	ACSJMSTextMessage acsMsg = (ACSJMSTextMessage)msg;
              if (acsMsg.getText().trim().length()>0) {
            	  cern.laser.business.data.Alarm business_alarm = AlarmMessageConversion.getAlarm(acsMsg.getText());
            	  initialSelection.put(business_alarm.getAlarmId(), new AlarmImpl(business_alarm));
              }
            }
            if (msg.propertyExists(LASER_INIT_PROPERTY)) {
              setInitialized(true);
            }
          } catch (Exception e) {
            LOGGER.warn("error on message", e);
          }
        }
      };
    }

    return initialSelectionListener;
  }

  private Subscriber getSubscriber() throws LaserException {
    try {
      if (cmwSubscriber == null) {
        cmwSubscriber = PubSubFactory.subscriber();
        cmwSubscriber.setExceptionListener(this);
      }
    } catch (Exception e) {
      throw new LaserException("unable to create the CMW subscriber", e);
    }

    return cmwSubscriber;
  }

}
