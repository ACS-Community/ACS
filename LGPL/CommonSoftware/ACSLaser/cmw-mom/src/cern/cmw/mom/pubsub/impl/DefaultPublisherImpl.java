package cern.cmw.mom.pubsub.impl;

import cern.cmw.mom.pubsub.MOMException;
import cern.cmw.mom.pubsub.Publisher;
import cern.cmw.mom.util.MomConfig;
import cern.cmw.mom.util.TopicAdminHelper;

import org.apache.log4j.Category;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import javax.jms.BytesMessage;
import javax.jms.DeliveryMode;
import javax.jms.ExceptionListener;
import javax.jms.JMSException;
import javax.jms.JMSSecurityException;
import javax.jms.MapMessage;
import javax.jms.Message;
import javax.jms.ObjectMessage;
import javax.jms.StreamMessage;
import javax.jms.TextMessage;
import javax.jms.Topic;
import javax.jms.TopicPublisher;
import javax.jms.TopicSession;

import javax.naming.NamingException;


/**
 * Implementation class.
 * @version 1.0   23 Jan 2001
 * @author   Controls Middleware Project
 * @see Publisher
 */
public class DefaultPublisherImpl implements Publisher, ExceptionListener {
  static Category cat = Category.getInstance(DefaultPublisherImpl.class.getName());
  private Boolean loadBalancing;
  private Boolean selectorAtBroker;
  private Boolean sequential;
  private cern.cmw.mom.pubsub.ExceptionListener listener = null;
  private JMSTopicConnection connection = null;
  private Map topicDirectory = null;
  private Properties momProperties = null;
  private String brokerList;
  private String password;
  private String username;
  private TopicPublisher publisher = null;
  private TopicSession session = null;
  private boolean closed = true;
  private int defaultPersistance;
  private int defaultPriority;
  private long defaultTimeToLive;

  /**
   * Constructor DefaultPublisherImpl
   *
   * @throws MOMException
   *
   */
  public DefaultPublisherImpl() throws MOMException {
    this.username = null;
    this.password = null;
    this.brokerList = null;
    this.loadBalancing = null;
    this.sequential = null;
    this.selectorAtBroker = null;
    open();
  }

  /**
   * Constructor DefaultPublisherImpl
   *
   * @param username the user name
   * @param password the password
   * @param brokerList the coma separated broker URLs list (in the form [protocol://]hostname[:port])
   * @param loadBalancing if true, indicates that the client is willing to have a connect request re-directed to another broker within a cluster
   * @param sequential if true, the broker list will be scanned sequentially
   * @param selectorAtBroker if true, selectors will be evaluated on the broker side
   * @throws MOMException
   *
   */
  public DefaultPublisherImpl(String username, String password, String brokerList, Boolean loadBalancing, Boolean sequential, Boolean selectorAtBroker) throws MOMException {
    this.username = username;
    this.password = password;
    this.brokerList = brokerList;
    this.loadBalancing = loadBalancing;
    this.sequential = sequential;
    this.selectorAtBroker = selectorAtBroker;
    open();
  }

  /**
   * Method isClosed
   *
   * @return Topic
   */
  public boolean isClosed() {
    return closed;
  }

  /**
   * Method setExceptionListener
   *
   * @param listener
   */
  public void setExceptionListener(cern.cmw.mom.pubsub.ExceptionListener listener) {
    this.listener = listener;
  }

  /**
   * Method close
   *
   *
   */
  public void close() {
    cat.debug("close()");

    if (closed) {
      return;
    }

    try {
      if (publisher != null) {
        publisher.close();
        publisher = null;
      }

      if (session != null) {
        session.close();
        session = null;
      }

      if (connection != null) {
        try {
          connection.stop();
        } catch (ConnectionException ce) {
          ce.printStackTrace();
        }

        connection.close();
        connection = null;
      }

      if (topicDirectory != null) {
        topicDirectory.clear();
        topicDirectory = null;
      }

      closed = true;
    } catch (javax.jms.JMSException e) {
      cat.error("JMSException raised closing a Publisher : " + e.getMessage());
    }
  }

  /**
   * Method createBytesMessage
   *
   *
   * @return BytesMessage
   *
   * @throws JMSException
   *
   */
  public BytesMessage createBytesMessage() throws JMSException {
    if (closed) {
      throw (new JMSException("Publisher object has been closed"));
    }

    BytesMessage message = null;
    message = session.createBytesMessage();

    return message;
  }

  /**
   * Method createMapMessage
   *
   *
   * @return MapMessage
   *
   * @throws JMSException
   *
   */
  public MapMessage createMapMessage() throws JMSException {
    if (closed) {
      throw (new JMSException("Publisher object has been closed"));
    }

    MapMessage message = null;
    message = session.createMapMessage();

    return message;
  }

  /**
   * Method createMessage
   *
   *
   * @return Message
   *
   * @throws JMSException
   *
   */
  public Message createMessage() throws JMSException {
    if (closed) {
      throw (new JMSException("Publisher object has been closed"));
    }

    Message message = null;
    message = session.createMessage();

    return message;
  }

  /**
   * Method createObjectMessage
   *
   *
   * @return ObjectMessage
   *
   * @throws JMSException
   *
   */
  public ObjectMessage createObjectMessage() throws JMSException {
    if (closed) {
      throw (new JMSException("Publisher object has been closed"));
    }

    ObjectMessage message = null;
    message = session.createObjectMessage();

    return message;
  }

  /**
   * Method createStreamMessage
   *
   *
   * @return StreamMessage
   *
   * @throws JMSException
   *
   */
  public StreamMessage createStreamMessage() throws JMSException {
    if (closed) {
      throw (new JMSException("Publisher object has been closed"));
    }

    StreamMessage message = null;
    message = session.createStreamMessage();

    return message;
  }

  /**
   * Method createTextMessage
   *
   *
   * @return TextMessage
   *
   * @throws JMSException
   *
   */
  public TextMessage createTextMessage() throws JMSException {
    if (closed) {
      throw (new JMSException("Publisher object has been closed"));
    }

    TextMessage message = null;
    message = session.createTextMessage();

    return message;
  }

  /**
   * Method onException
   *
   *
   * @param ex
   *
   */
  public void onException(JMSException ex) {
    cat.warn("onException()");

    /*if (progress.message.jclient.ErrorCodes.testException(ex, progress.message.jclient.ErrorCodes.ERR_CONNECTION_DROPPED)) {
      cat.error("ERR_CONNECTION_DROPPED detected.");

      if (listener != null) {
        listener.onException(new MOMException("ERR_CONNECTION_DROPPED detected.", MOMException.CONNECTION_LOST_EXCEPTION));
      }

      connection.disconnect();

      try {
        initialize(true);

        if (listener != null) {
          listener.onException(new MOMException("Connection reestabilished", MOMException.CONNECTION_RECOVERED_EXCEPTION));
        }
      } catch (MOMException e) {
        cat.error("unable to recover", e);
      }
    }*/
  }

  /**
   * Method open
   *
   * @throws MOMException
   */
  public void open() throws MOMException {
    cat.debug("open()");
    initialize(false);
  }

  /**
   * Method publish
   *
   *
   * @param topic
   * @param message
   *
   * @throws JMSException
   * @throws NamingException
   *
   */
  public void publish(String topic, Message message) throws JMSException, NamingException {
    publish(topic, message, defaultPersistance, defaultPriority, defaultTimeToLive);
  }

  /**
   * Publish a message to the given topic.
   * @param topic The String representation of the topic
   * @param message The Message object to publish
   * @param deliveryMode The Message persistence
   * @param priority The Message priority
   * @param timeToLive The Message time to live
   * @exception JMSException if JMS fails to publish the message due to some internal JMS error.
   * @exception NamingException  if there is a violation in the namespace policy.
   */
  public void publish(String topic, Message message, int deliveryMode, int priority, long timeToLive) throws JMSException, NamingException {
    cat.debug("publish(" + topic + ", " + message + ")");

    if (closed) {
      throw (new JMSException("Publisher object has been closed"));
    }

    Topic t = createTopic(topic);

    try {
      publisher.publish(t, message, deliveryMode, priority, timeToLive);
    } catch (JMSSecurityException jse) {
      cat.error("JMSSecurityException caught");
      throw (new NamingException(jse.getMessage()));
    }
  }

  /**
   * Method finalize
   *
   * @throws Throwable
   */
  protected void finalize() throws Throwable {
    cat.debug("finalize()");
    close();
    super.finalize();
  }

  /**
   * Method createTopic
   *
   * @param topic
   * @return Topic
   * @throws JMSException
   * @throws NamingException
   */
  private Topic createTopic(String topic) throws JMSException, NamingException {
    cat.debug("createTopic(" + topic + ")");

    Topic the_topic = null;

    if (TopicAdminHelper.validateTopic(topic, connection.getUsername(), TopicAdminHelper.FOR_PUBLISHING)) {
      the_topic = (Topic) topicDirectory.get(topic);

      if (the_topic == null) {
        the_topic = session.createTopic(topic);
        topicDirectory.put(topic, the_topic);
      }
    } else {
      throw (new NamingException("[" + topic + "] is not a valid topic name"));
    }

    return the_topic;
  }

  /**
   * Method initialize.
   *
   * @param retry
   * @throws MOMException
   */
  private void initialize(boolean retry) throws MOMException {
    momProperties = MomConfig.getProperties(this.getClass().getClassLoader());
    defaultPersistance = (momProperties.getProperty(MomConfig.MSG_PERSISTANCE_PROPERTY).equals("true") ? DeliveryMode.PERSISTENT : DeliveryMode.NON_PERSISTENT);
    defaultPriority = Integer.parseInt(momProperties.getProperty(MomConfig.MSG_PRIORITY_PROPERTY));
    defaultTimeToLive = Long.parseLong(momProperties.getProperty(MomConfig.MSG_TIMETOLIVE_PROPERTY));
    topicDirectory = new HashMap();

    try {
      connection = JMSTopicConnectionFactory.createJMSTopicConnection(username, password, brokerList, loadBalancing, sequential, selectorAtBroker);
      connection.connect(retry);
      connection.setExceptionListener(this);
      session = connection.createTopicSession();
      connection.start();
      publisher = session.createPublisher(null);
      closed = false;
    } catch (ConnectionException e) {
    	System.out.println("Exception caught: "+e.getMessage());
    	e.printStackTrace(System.err);
      throw new MOMException("Unable to estabilish a connection, cause is : " + e.getMessage());
    } catch (JMSException e) {
    	System.out.println("Exception caught: "+e.getMessage());
    	e.printStackTrace(System.err);
      throw (new MOMException(e.getMessage()));
    }
  }
}


/*--- Formatted in Sun Java Convention Style on Fri, Aug 3, '01 ---*/
/*------ Formatted by Jindent 3.23 Gold 1.02 --- http://www.jindent.de ------*/
