/*
 * $Id: HeartbeatHelper.java,v 1.8 2011/04/13 15:45:42 acaproni Exp $
 *
 * $Date: 2011/04/13 15:45:42 $ 
 * $Revision: 1.8 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client.impl.services.selection;

import java.util.Timer;
import java.util.TimerTask;

import javax.jms.Message;

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
import cern.laser.client.impl.common.AlarmServiceSingleton;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.LaserHeartbeatException;
import cern.laser.client.services.selection.LaserSelectionException;

/**
 * This class subscribes to the heartbeat from the business layer and verifies that it arrives in time. If it doesn't
 * the AlarmSelectionListener is informed. It is a helper class for the AlarmSelectionHandler.
 * 
 * @version $Revision: 1.8 $ $Date: 2011/04/13 15:45:42 $
 * @author Katarina Sigerud
 */
class HeartbeatHelper implements ExceptionListener {
  private static final Logger LOGGER = Logger.getLogger(HeartbeatHelper.class.getName());
  private static final String HEARTBEAT_PROPERTY = "HEARTBEAT";
  private static final String SQL_FILTER = "HEARTBEAT = TRUE";

  private SubscriptionListener heartbeatListener = null;

  private AlarmSelectionListener selectionListener = null;

  private Timer timer = null;

  private String heartbeatTopic = null;
  private long heartbeatFrequency = -1;
  private long heartbeatCheckFrequency = -1;
  private boolean heartbeatReceived = false;
  private long heartbeatReceptionTime = -1;

  private Subscriber cmwSubscriber;
  private boolean cmwConnected = true;

  // The AlarmService component
  private CERNAlarmService m_laser;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  /**
   * @param sessionEJB
   * @throws LaserException
   * @throws LaserException
   * @throws LaserConnectionException
   *  
   */
  public HeartbeatHelper(AlarmSelectionListener selectionListener, ORB orb, AcsLogger logger) throws LaserException {
  	this.selectionListener = selectionListener;

    try {
        this.m_laser = AlarmServiceSingleton.getInstance(orb,logger);

      heartbeatFrequency = m_laser.getHeartbeatFrequency();
      heartbeatTopic = m_laser.getHeartbeatTopic();

      heartbeatCheckFrequency = heartbeatFrequency / 2;

      initHeartbeatCheck();
    } catch (Exception e) {
      throw new LaserException("unable to setup heartbeat check", e);
    }
  }

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  public void startHeartbeatCheck() throws LaserException {
    if (timer == null) {
      LOGGER.info("Alarm Heartbeat client check frequency : " + heartbeatCheckFrequency + " milliseconds");
      LOGGER.info("starting heartbeat check...");
      timer = new Timer();
      timer.schedule(createHeartbeatTimerTask(), heartbeatCheckFrequency, heartbeatCheckFrequency);
      LOGGER.info("started heartbeat check");
    }
  }

  public void stopHeartbeatCheck() throws LaserException {
    if (timer != null) {
      LOGGER.info("stopping heartbeat check...");
      timer.cancel();
      timer = null;
      heartbeatTopic = null;
      heartbeatFrequency = -1;
      heartbeatCheckFrequency = -1;
      LOGGER.info("stopped heartbeat check");
    }
    if (cmwSubscriber != null) {
      cmwSubscriber.close();
      cmwSubscriber = null;
    }
  }

  //
  // -- implements ExceptionListener ---------------------------------
  //

  /*
   * (non-Javadoc)
   * 
   * @see cern.cmw.mom.pubsub.ExceptionListener#onException(cern.cmw.mom.pubsub.MOMException)
   */
  public void onException(MOMException e) {
    if (e.testException(MOMException.CONNECTION_LOST_EXCEPTION)) {
      cmwConnected = false;
      if (selectionListener != null) {
        selectionListener.onException(new LaserSelectionException(LaserSelectionException.CONNECTION_DROPPED));
      }
    } else {
      if (e.testException(MOMException.CONNECTION_RECOVERED_EXCEPTION)) {
        cmwConnected = true;
        if (selectionListener != null) {
          selectionListener.onException(new LaserSelectionException(LaserSelectionException.CONNECTION_REESTABILISHED));
        }
      }
    }
  }

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //

  /**
   * @throws LaserException
   * @throws LaserConnectionException
   */
  private void initHeartbeatCheck() throws LaserException, LaserConnectionException {
    if (selectionListener != null) {
      try {
        setHeartbeatReceived(true);
        resetHeartbeatReceptionTime();
        getSubscriber().subscribe(heartbeatTopic, getHeartbeatListener(), SQL_FILTER);
      } catch (Exception e1) {
        new LaserException("unable to subscribe to topic " + heartbeatTopic, e1);
      }
    }
  }

  private SubscriptionListener getHeartbeatListener() {
    if (heartbeatListener == null) {
      heartbeatListener = new SubscriptionListener() {
        public void onMessage(Message msg) {
          try {
            resetHeartbeatReceptionTime();
          } catch (Exception e) {
            LOGGER.warn("error on message in heartbeat subscription", e);
          }
        }
      };
    }

    return heartbeatListener;
  }

  private TimerTask createHeartbeatTimerTask() {
    return new TimerTask() {
      public void run() {
        try {
          checkHeartbeat();
        } catch (Exception e) {
          LOGGER.error("heartbeat check failed", e);
        }
      }
    };
  }

  private void checkHeartbeat() {
    if (LOGGER.isDebugEnabled()) LOGGER.debug("checking heartbeat reception...");
    long now = System.currentTimeMillis();
    long heartbeatWaitTime = now - getHeartbeatReceptionTime();
    if (heartbeatWaitTime > heartbeatFrequency) {
      if (isHeartbeatReceived()) {
        if (selectionListener != null) {
          selectionListener.onException(new LaserHeartbeatException(LaserHeartbeatException.HEARTBEAT_LOST));
        }
        setHeartbeatReceived(false);

      }
    } else {
      if (!isHeartbeatReceived()) {
        if (selectionListener != null) {
          selectionListener.onException(new LaserHeartbeatException(LaserHeartbeatException.HEARTBEAT_RECONNECTED));
        }
        setHeartbeatReceived(true);

      }
    }

    if (LOGGER.isDebugEnabled()) LOGGER.debug("heartbeat reception checked (wait time " + heartbeatWaitTime + ")");
  }

  private synchronized void resetHeartbeatReceptionTime() {
    heartbeatReceptionTime = System.currentTimeMillis();
  }

  private synchronized long getHeartbeatReceptionTime() {
    return heartbeatReceptionTime;
  }

  private synchronized void setHeartbeatReceived(boolean value) {
    heartbeatReceived = value;
  }

  private synchronized boolean isHeartbeatReceived() {
    return heartbeatReceived;
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
