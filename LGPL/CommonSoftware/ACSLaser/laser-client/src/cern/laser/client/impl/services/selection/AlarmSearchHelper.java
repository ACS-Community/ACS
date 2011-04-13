/*
 * $Id: AlarmSearchHelper.java,v 1.7 2011/04/13 15:45:42 acaproni Exp $
 *
 * $Date: 2011/04/13 15:45:42 $ 
 * $Revision: 1.7 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.impl.services.selection;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Collection;

import javax.jms.Message;
import javax.jms.ObjectMessage;

import org.apache.log4j.Logger;
import org.omg.CORBA.ORB;

import alma.acs.container.ContainerServicesBase;
import alma.acs.logging.AcsLogger;
import alma.alarmsystem.CERNAlarmService;

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
import cern.laser.client.services.selection.AlarmSearchListener;
import cern.laser.client.services.selection.LaserSearchException;
import cern.laser.client.services.selection.Selection;
import cern.laser.util.UUIDGenerator;

/**
 * 
 * 
 * @version $Revision: 1.7 $ $Date: 2011/04/13 15:45:42 $
 * @author Katarina Sigerud
 */
class AlarmSearchHelper implements ExceptionListener {
  private static final Logger LOGGER = Logger.getLogger(AlarmSearchHelper.class.getName());

  private static final String LASER_SEARCH_PROPERTY = "LASER_SEARCH";
  private static final long SEARCH_DELAY = 1000;
  private static final long SEARCH_TIMEOUT = 60000;

  private boolean searchFinished = false;
  private long searchWaitTime = 0;

  private Subscriber cmwSubscriber;

  private String searchRootTopic;
  private boolean cmwConnected = true;
  private AlarmSearchListener searchListener;
  private SubscriptionListener initialSearchListener;

  // The AlarmService component
  private CERNAlarmService m_laser;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  AlarmSearchHelper(AlarmSearchListener selectionListener, ORB orb, AcsLogger logger) throws LaserException {
  	this.searchListener = selectionListener;

    try {
        this.m_laser = AlarmServiceSingleton.getInstance(orb,logger);
    } catch (Exception e) {
      throw new LaserException("unable to setup initial selection", e);
    }

  }

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  public void search(Selection selection, int nbOfRows) throws LaserConnectionException, LaserException,
  LaserTimeOutException {

    searchFinished(false);

    String console_id = "";
    String host_name = "";
    try {
      console_id = UUIDGenerator.getInstance().getUUID().toString();
      host_name = InetAddress.getLocalHost().getHostName();
    } catch (UnknownHostException e) {
      throw new LaserException("unable to get a unique id for the console : " + e.getMessage());
    }
    String init_topic = getSearchRootTopic() + "." + console_id;

    long init_subscription;
    try {
      init_subscription = getSubscriber().subscribe(init_topic, getSearchListener(), null);
    } catch (Exception e) {
      throw new LaserException("unable to subscribe to topic " + init_topic + " : " + e.getMessage());
    }
    // activate the selection on the BT
    Integer[] category_ids = getCategoryIds(selection);
    String sql = buildSQLFilter(selection, nbOfRows);
    try {
      	if (m_laser!=null) {
      		int[] cis = new int[category_ids.length];
      		for (int i = 0; i < category_ids.length; i++)
      			cis[i] = category_ids[i].intValue();
            m_laser.search(cis, sql, console_id);
        } else {
            throw new NullPointerException("AlarmSystem component is null");
        }
    } catch (Exception e) {
      throw new LaserException("unable to perform initial selection at host " + host_name + " : " + e.getMessage());
    }

    resetInitWaitTime();
    waitForInit();

    try {
      // stop init subscription
      getSubscriber().unSubscribe(init_subscription);
    } catch (Exception e) {
      // Intentionally left blank
    }
  }

  public void resetSelection() throws LaserException {
    try {
      getSubscriber().unSubscribeAll();
    } catch (Exception e) {
      throw new LaserException("unable to unsubscribe all");
    }
    if (cmwSubscriber != null) {
      cmwSubscriber.close();
      cmwSubscriber = null;
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
      if (searchListener != null) {
        searchListener.onSearchException(new LaserSearchException(
            cern.laser.client.services.selection.LaserSelectionException.CONNECTION_DROPPED));
      }
    } else {
      if (e.testException(MOMException.CONNECTION_RECOVERED_EXCEPTION)) {
        cmwConnected = true;
        if (searchListener != null) {
          searchListener.onSearchException(new LaserSearchException(
              cern.laser.client.services.selection.LaserSelectionException.CONNECTION_REESTABILISHED));
        }
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
    while (!isSearchTimedOut()) {
      try {
        Thread.sleep(SEARCH_DELAY);
      } catch (InterruptedException ie) {
      }
      if (isSearchFinished()) {
        break;
      }
      increaseSearchWaitTime();
    }
  }

  private String buildSQLFilter(Selection selection, int nbOfRows) {
    String sql = selection.getFilterSelection().toSQLString();
    StringBuffer buffer = null;
    if (sql == null) {
      buffer = new StringBuffer("");
    } else {
      buffer = new StringBuffer(sql);
      buffer.append(" AND ");
    }
    buffer.append("rownum<=");
    buffer.append(nbOfRows);

    if (LOGGER.isDebugEnabled()) LOGGER.debug("sql " + buffer.toString());
    return buffer.toString();
  }

  private synchronized void searchFinished(boolean value) {
    searchFinished = value;
    if (searchListener != null) {
      searchListener.searchFinished();
    }
    if (cmwSubscriber != null) {
      cmwSubscriber.close();
    }
  }

  private synchronized boolean isSearchFinished() {
    return searchFinished;
  }

  private synchronized void increaseSearchWaitTime() {
    searchWaitTime += SEARCH_DELAY;
  }

  private void resetInitWaitTime() {
    searchWaitTime = 0;
  }

  private boolean isSearchTimedOut() {
    return searchWaitTime > SEARCH_TIMEOUT;
  }

  private String getSearchRootTopic() throws LaserException, LaserConnectionException {
  	if (searchRootTopic == null) {
        try {
          	if (m_laser!=null) {
          		searchRootTopic = m_laser.getSearchRootTopic();
            } else {
                throw new NullPointerException("AlarmSystem component is null");
            }
        } catch (Exception e1) {
          throw new LaserException("unable to find client root topic : " + e1.getMessage());
        }
    }

    return searchRootTopic;
  }

  private SubscriptionListener getSearchListener() {
    if (initialSearchListener == null) {
      initialSearchListener = new SubscriptionListener() {
        public void onMessage(Message msg) {
          try {
            if (searchListener != null) {
              if (searchListener.isSearchCancelled()) {
                LOGGER.info("search cancelled");
                searchFinished(true);
              } else {
                try {
                  cern.laser.business.data.Alarm business_alarm = (cern.laser.business.data.Alarm) ((ObjectMessage) msg)
                      .getObject();
                  searchListener.onSearchAlarm(new AlarmImpl(business_alarm));
                } catch (Exception e) {
                  LOGGER.warn("error on message : " + e.getMessage());
                }
                if (msg.propertyExists(LASER_SEARCH_PROPERTY)) {
                  searchFinished(true);
                }
              }
            }
          } catch (Exception e) {
            LOGGER.warn("error on message : " + e.getMessage());
          }
        }
      };
    }

    return initialSearchListener;
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

  private Integer[] getCategoryIds(Selection selection) throws LaserConnectionException, LaserException {
    Category[] categories = selection.getCategorySelection().list();
    Collection category_ids = new ArrayList(categories.length);
    for (int i = 0; i < categories.length; i++) {
      category_ids.add(categories[i].getCategoryId());
    }
    return (Integer[]) category_ids.toArray(new Integer[category_ids.size()]);
  }


}