package cern.laser.business.pojo;

import java.util.Timer;
import java.util.TimerTask;

import javax.jms.JMSException;
import javax.jms.Session;
import javax.jms.TextMessage;
import javax.jms.Topic;
import javax.jms.TopicConnection;
import javax.jms.TopicConnectionFactory;
import javax.jms.TopicPublisher;
import javax.jms.TopicSession;
import javax.naming.NamingException;

import org.apache.log4j.Logger;

public class HeartbeatImpl {
  private static final Logger LOGGER = Logger.getLogger(HeartbeatImpl.class.getName());

  private static final String HEARTBEAT_PROPERTY = "HEARTBEAT";
  private static final String HEARTBEAT_MESSAGE_TEXT = "HEARTBEAT MESSAGE";

  private Timer timer = null;
  private TopicConnection connection = null;
  private TopicSession session = null;
  private TopicPublisher publisher = null;

  private CoreServiceImpl coreService;
  private TopicConnectionFactory topicConnectionFactory;

  //
  // -- CONSTRUCTORS ------------------------------------------------
  //

  //
  // -- PUBLIC METHODS ----------------------------------------------
  //

  public void setCoreService(CoreServiceImpl coreService) {
    this.coreService = coreService;
  }

  public void setTopicConnectionFactory(TopicConnectionFactory topicConnectionFactory) {
    this.topicConnectionFactory = topicConnectionFactory;
  }
  
  public void start() {
    try {
      if (timer == null) {
        LOGGER.info("AlarmImpl Heartbeat frequency : " + coreService.getHeartbeatFrequency() + " milliseconds");
        LOGGER.info("starting heartbeat...");
        timer = new Timer();
        timer.schedule(createTimerTask(), 0, coreService.getHeartbeatFrequency());
        LOGGER.info("started heartbeat");
      }
    } catch (Exception e) {
      //throw new EJBException(e.getMessage(), e);
    }
  }

  public void stop() {
    try {
      if (timer != null) {
        LOGGER.info("stopping heartbeat...");
        timer.cancel();
        timer = null;
        LOGGER.info("stopped heartbeat");
      }
    } catch (Exception e) {
      //throw new EJBException(e.getMessage(), e);
    }
  }

  //
  // -- PRIVATE METHODS ---------------------------------------------
  //

  private void sendHeartbeat() {
    try {
      LOGGER.debug("sending heartbeat...");
      try {
        Topic topic = getTopicSession().createTopic(coreService.getHeartbeatTopic());
        TextMessage message = getTopicSession().createTextMessage(HEARTBEAT_MESSAGE_TEXT);

        message.setObjectProperty(HEARTBEAT_PROPERTY, Boolean.TRUE);
        getTopicPublisher().publish(topic, message);
      } catch (Exception e) {
        LOGGER.error("unable to send heartbeat", e);
        close();
        //throw new EJBException("unable to send heartbeat", e);
      }
      LOGGER.debug("heartbeat sent");
    } catch (Exception e) {
      //throw new EJBException(e.getMessage(), e);
    }
  }

  private TopicSession getTopicSession() throws JMSException, NamingException {
    if (session == null) {
      session = getTopicConnection().createTopicSession(false, Session.AUTO_ACKNOWLEDGE);
    }

    return session;
  }

  private TopicConnection getTopicConnection() throws JMSException, NamingException {
    if (connection == null) {
      connection = topicConnectionFactory.createTopicConnection();
    }

    return connection;
  }

  private TopicPublisher getTopicPublisher() throws JMSException, NamingException {
    if (publisher == null) {
      publisher = getTopicSession().createPublisher(null);
    }

    return publisher;
  }

  private void close() {
    if (session != null) {
      try {
        session.close();
      } catch (JMSException je) {
        LOGGER.error("unable to close JMS session", je);
      }
    }
    if (connection != null) {
      try {
        connection.close();
      } catch (JMSException je) {
        LOGGER.error("unable to close JMS connection", je);
      }
    }
    publisher = null;
    session = null;
    connection = null;
  }

  private TimerTask createTimerTask() {
    return new TimerTask() {
      public void run() {
        try {
          sendHeartbeat();
        } catch (Exception e) {
          LOGGER.error("heartbeat sending failed", e);
        }
      }
    };
  }

}