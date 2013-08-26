package cern.cmw.mom.pubsub.impl;

import cern.cmw.mom.pubsub.MOMException;
import cern.cmw.mom.pubsub.NotificationHelper;
import cern.cmw.mom.pubsub.Subscriber;
import cern.cmw.mom.pubsub.SubscriptionListener;
import cern.cmw.mom.util.MomConfig;
import cern.cmw.mom.util.TopicAdminHelper;

import org.apache.log4j.Category;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;

import javax.jms.DeliveryMode;
import javax.jms.ExceptionListener;
import javax.jms.JMSException;
import javax.jms.JMSSecurityException;
import javax.jms.Message;
import javax.jms.Topic;
import javax.jms.TopicPublisher;
import javax.jms.TopicSession;
import javax.jms.TopicSubscriber;

import javax.naming.NamingException;


/**
 * Implementation class.
 * @version 1.0   23 Jan 2001
 * @author   Controls Middleware Project
 * @see Subscriber
 */
public class DefaultSubscriberImpl implements Subscriber, ExceptionListener {
  static Category cat = Category.getInstance(DefaultSubscriberImpl.class.getName());
  private Boolean loadBalancing;
  private Boolean selectorAtBroker;
  private Boolean sequential;
  private cern.cmw.mom.pubsub.ExceptionListener listener = null;
  private HashMap subscribers = null;
  private JMSTopicConnection connection = null;
  private Map topicDirectory = null;
  private Message notificationMessage = null;
  private Properties momProperties = null;
  private String brokerList;
  private String password;
  private String subscriberId = null;
  private String username;
  private Thread keepAliveThread = null;
  private TopicPublisher notificationPublisher = null;
  private TopicSession serviceSession = null;
  private boolean closed = true;
  private boolean keepAliveEnabled = false;
  private boolean notificationsEnabled = false;
  private int keepAliveInterval = 0;

  /**
   * Constructor DefaultSubscriberImpl
   *
   *
   * @throws MOMException
   *
   */
  public DefaultSubscriberImpl() throws MOMException {
    this.username = null;
    this.password = null;
    this.brokerList = null;
    this.loadBalancing = null;
    this.selectorAtBroker = null;
    this.sequential = null;
    open();
  }

  /**
   * Constructor DefaultSubscriberImpl
   *
   *
   * @throws MOMException
   *
   */
  public DefaultSubscriberImpl(String username, String password, String brokerList, Boolean loadBalancing, Boolean sequential, Boolean selectorAtBroker) throws MOMException {
    this.username = username;
    this.password = password;
    this.brokerList = brokerList;
    this.loadBalancing = loadBalancing;
    this.selectorAtBroker = selectorAtBroker;
    this.sequential = sequential;
    open();
  }

  /**
   * Method isClosed
   *
   * @return boolean true iff the subscriber instance has been closed
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

    synchronized (subscribers) {
      SubscriptionHandle handle = null;
      keepAliveEnabled = false;

      try {
        Iterator iterator = subscribers.values().iterator();

        while (iterator.hasNext()) {
          handle = (SubscriptionHandle) iterator.next();

          if (!NotificationHelper.isNotification(handle.getSubscriptionTopic())) {
            publishNotification(NotificationHelper.CONSUMER_CLOSE_NOTIFICATION, handle);
          }

          handle.getSubscriber().close();
          handle.getSession().close();
        }

        subscribers.clear();

        if (notificationPublisher != null) {
          notificationPublisher.close();
          notificationPublisher = null;
        }

        if (serviceSession != null) {
          serviceSession.close();
          serviceSession = null;
        }

        if (topicDirectory != null) {
          topicDirectory.clear();
          topicDirectory = null;
        }
      } catch (JMSException e) {
        cat.error("Exception raised closing a Subscriber : " + e.getMessage());
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

      closed = true;
    }

    cat.debug("closed.");
  }

  /**
   * Method onException
   *
   *
   * @param ex
   *
   */
  public void onException(JMSException ex) {
    cat.warn("onException(): " + ex);

    /*if (progress.message.jclient.ErrorCodes.testException(ex, progress.message.jclient.ErrorCodes.ERR_CONNECTION_DROPPED)) {
      cat.error("ERR_CONNECTION_DROPPED detected.");

      if (!isClosed()) {
        if (listener != null) {
          listener.onException(new MOMException("ERR_CONNECTION_DROPPED detected.", MOMException.CONNECTION_LOST_EXCEPTION));
        }

        connection.disconnect();

        try {
          initialize(true);
          recoverSubscriptions();

          if (listener != null) {
            listener.onException(new MOMException("Connection reestabilished", MOMException.CONNECTION_RECOVERED_EXCEPTION));
          }
        } catch (MOMException e) {
          cat.error("unable to recover", e);
        }
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

    // initialize the subscriber
    initialize(false);

    // set the subscriber identifier
    setConnectionIdentifier();

    // subscribers vector is initialised
    subscribers = new HashMap();

    // keep alive thread is started iff keepAliveInterval>0
    if (notificationsEnabled && (keepAliveInterval > 0)) {
      keepAliveEnabled = true;
      keepAliveThread = createKeepAliveThread();
      keepAliveThread.setPriority(Thread.MIN_PRIORITY);
      keepAliveThread.setDaemon(true);
      keepAliveThread.start();
    }

    closed = false;
  }

  /**
   * Method subscribe
   *
   *
   * @param topic
   * @param listener
   * @param selector
   *
   * @return long the subscription handle identifier
   *
   * @throws JMSException
   * @throws NamingException
   *
   */
  public long subscribe(String topic, SubscriptionListener listener, String selector) throws JMSException, NamingException {
    cat.info("subscribe(" + topic + ", listener, " + selector + ")");

    if (closed) {
      throw (new JMSException("Subscriber closed."));
    }

    SubscriptionHandle handle = new SubscriptionHandle();
    handle.setSubscriptionTopic(topic);
    handle.setSubscriptionSelector(selector);
    handle.setSubscriptionListener(listener);

    StringBuffer local_selector = new StringBuffer();

    if (NotificationHelper.isNotification(topic)) {
      // this is a subscription to notifications, no further selection is required
      local_selector.append(selector);
    } else {
      // subscription to a generic topic, adding subscriber specific selection
      if (selector != null) {
        local_selector.append(selector);
        local_selector.append(" AND ");
      }

      local_selector.append("( (");
      local_selector.append(NotificationHelper.SUBSCRIPTION_ID_PROPERTY);
      local_selector.append(" IS NULL) OR (");
      local_selector.append(NotificationHelper.SUBSCRIPTION_ID_PROPERTY);
      local_selector.append(" = '");
      local_selector.append(subscriberId);
      local_selector.append("@");
      local_selector.append(handle.getSubscriptionToken());
      local_selector.append("') )");
    }

    TopicSession session = null;
    TopicSubscriber subscriber = null;
    Topic the_topic = createTopic(topic);

    try {
      session = connection.createTopicSession();
      subscriber = session.createSubscriber(the_topic, local_selector.toString(), false);
    } catch (JMSSecurityException jse) {
      cat.error("JMSSecurityException caught");
      throw (new NamingException(jse.getMessage()));
    } catch (JMSException je) {
      cat.error("JMSException caught");
      throw (new NamingException(je.getMessage()));
    } catch (ConnectionException ce) {
      cat.error("ConnectionException caught");
      throw (new JMSException(ce.getMessage()));
    } catch (Exception e) {
      cat.error("Generic exception caught", e);
    }

    subscriber.setMessageListener(listener);
    handle.setSubscriber(subscriber);
    handle.setSession(session);

    synchronized (subscribers) {
      subscribers.put(new Long(handle.getSubscriptionToken()), handle);
    }

    if (!NotificationHelper.isNotification(topic)) {
      publishNotification(NotificationHelper.CONSUMER_OPEN_NOTIFICATION, handle);
    }

    return handle.getSubscriptionToken();
  }

  /**
   * Method unSubscribe
   *
   *
   * @param token
   *
   * @throws JMSException
   *
   */
  public void unSubscribe(long token) throws JMSException {
    cat.info("unSubscribe(" + token + ")");

    if (closed) {
      throw (new JMSException("Subscriber closed."));
    }

    synchronized (subscribers) {
      Long key = new Long(token);
      SubscriptionHandle handle = (SubscriptionHandle) subscribers.get(key);

      if (handle != null) {
        handle.getSubscriber().close();
        handle.getSession().close();

        if (!NotificationHelper.isNotification(handle.getSubscriptionTopic())) {
          publishNotification(NotificationHelper.CONSUMER_CLOSE_NOTIFICATION, handle);
        }

        subscribers.remove(key);
      }
    }
  }

  /**
   * Close all the opened subscriptions.
   * @exception JMSException  if JMS fails to unsubscribe due to some internal JMS error.
   */
  public void unSubscribeAll() throws JMSException {
    cat.info("unSubscribeAll()");
    System.out.println("### DefaultSubscriberImpl::unSubscribeAll");

    if (closed) {
    	throw (new JMSException("Subscriber closed."));
    }

    synchronized (subscribers) {
      SubscriptionHandle handle = null;
      Iterator iterator = subscribers.values().iterator();

      while (iterator.hasNext()) {
        handle = (SubscriptionHandle) iterator.next();
        if (!NotificationHelper.isNotification(handle.getSubscriptionTopic())) {
          publishNotification(NotificationHelper.CONSUMER_CLOSE_NOTIFICATION, handle);
        }

        handle.getSubscriber().close();
        handle.getSession().close();
      }

      subscribers.clear();
    }
  }

  /**
   * Method createTopic
   *
   * @param topic
   * @return Topic the topic
   * @throws JMSException
   * @throws NamingException
   */
  protected Topic createTopic(String topic) throws JMSException, NamingException {
    cat.debug("createTopic(" + topic + ")");
    
    Topic the_topic = null;
    
    if (NotificationHelper.isNotification(topic) || TopicAdminHelper.validateTopic(topic,connection.getUsername(),TopicAdminHelper.FOR_SUBSCRIBING)) {
      the_topic = (Topic) topicDirectory.get(topic);

      if (the_topic == null) {
        the_topic = serviceSession.createTopic(topic);
        topicDirectory.put(topic, the_topic);
      }
    } else {
      throw (new NamingException("[" + topic + "] is not a valid topic name"));
    }

    return the_topic;
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
    cat.debug(subscriberId + " finalized.");
  }

  /**
   * Method recoverSubscriptions
   *
   */
  protected void recoverSubscriptions() {
    cat.info("recoverSubscriptions()");

    try {
      SubscriptionHandle handle = null;
      Iterator iterator = subscribers.values().iterator();

      while (iterator.hasNext()) {
        handle = (SubscriptionHandle) iterator.next();

        if (!NotificationHelper.isNotification(handle.getSubscriptionTopic())) {
          publishNotification(NotificationHelper.CONSUMER_CLOSE_NOTIFICATION, handle);
        }

        cat.debug("Recovering subscription to : " + handle.getSubscriptionTopic());

        TopicSession session = connection.createTopicSession();
        TopicSubscriber subscriber = session.createSubscriber(createTopic(handle.getSubscriptionTopic()), handle.getSubscriptionSelector(), false);
        subscriber.setMessageListener(handle.getSubscriptionListener());
        handle.setSession(session);
        handle.setSubscriber(subscriber);

        if (!NotificationHelper.isNotification(handle.getSubscriptionTopic())) {
          publishNotification(NotificationHelper.CONSUMER_OPEN_NOTIFICATION, handle);
        }
      }

      cat.info("Subscriptions succesfully recovered.");
    } catch (Exception e) {
      cat.error("unable to recover subscriptions", e);
    }
  }

  /**
   * Method setId
   *
   */
  private void setConnectionIdentifier() {
    String hostname = null;

    try {
      hostname = java.net.InetAddress.getLocalHost().getHostName();
    } catch (Exception e) {
      cat.warn("Exception raised attempting to get the hostname.", e);
      hostname = new String("HOSTNAME");
    }

    subscriberId = new String(hostname + "@" + connection.getConnectId());
  }

  /**
   * Method getSubscriptionHandle
   *
   * @param token
   * @return SubscriptionHandle the subscription handle
   */
  private SubscriptionHandle getSubscriptionHandle(long token) {
    synchronized (subscribers) {
      return (SubscriptionHandle) subscribers.get(new Long(token));
    }
  }

  /**
   * Method run
   *
   *
   * @return Thread the keep alive thread
   */
  private Thread createKeepAliveThread() {
    Thread keep_alive_thread = new Thread() {
      public void run() {
        cat.info("KeepAliveThread started.");

        while (keepAliveEnabled) {
          publishKeepAliveNotifications();

          try {
            Thread.sleep(keepAliveInterval * 1000);
          } catch (InterruptedException ie) {
            ie.printStackTrace();
          }
        }

        cat.info("KeepAliveThread exited.");
      }
    };

    return keep_alive_thread;
  }

  /**
   * Method initialize
   *
   * @param retry
   * @throws MOMException
   */
  private void initialize(boolean retry) throws MOMException {
    cat.debug("initialize()");

    momProperties = MomConfig.getProperties(this.getClass().getClassLoader());
    keepAliveInterval = Integer.valueOf(momProperties.getProperty(MomConfig.KEEP_ALIVE_PROPERTY)).intValue();
    notificationsEnabled = Boolean.valueOf(momProperties.getProperty(MomConfig.NOTIFICATION_PROPERTY)).booleanValue();
    topicDirectory = new HashMap();

    try {
      connection = JMSTopicConnectionFactory.createJMSTopicConnection(username, password, brokerList, loadBalancing, sequential, selectorAtBroker);
      connection.connect(retry);
      connection.setExceptionListener(this);
      serviceSession = connection.createTopicSession();
      connection.start();
    } catch (ConnectionException e) {
      throw new MOMException("Unable to estabilish a connection, cause is : " + e.getMessage());
    }

    try {
      notificationPublisher = serviceSession.createPublisher(null);
      notificationPublisher.setDeliveryMode(DeliveryMode.NON_PERSISTENT);
      notificationPublisher.setTimeToLive(60000);
      notificationMessage = serviceSession.createMessage();
    } catch (JMSException e) {
    	System.out.println("Exception caught: "+e.getMessage());
    	e.printStackTrace(System.err);
      throw (new MOMException("Exception raised attempting to create the notifications publisher, cause is : " + e.getMessage()));
    }
  }

  /**
   * Method publishKeepAliveNotifications
   *
   */
  private void publishKeepAliveNotifications() {
    cat.debug("publishKeepAliveNotifications()");

    if (connection.isConnected()) {
      SubscriptionHandle handle = null;
      Collection local_subscribers = null;

      synchronized (subscribers) {
        local_subscribers = ((Map) subscribers.clone()).values();
      }

      try {
        Iterator iterator = local_subscribers.iterator();

        while (iterator.hasNext()) {
          handle = (SubscriptionHandle) iterator.next();

          if (!NotificationHelper.isNotification(handle.getSubscriptionTopic())) {
            publishNotification(NotificationHelper.CONSUMER_ALIVE_NOTIFICATION, handle);
          }
        }
      } catch (JMSException je) {
        cat.error("unable to publish keep alive notifications", je);
      }
    }
  }

  /**
   * Method publishNotification
   *
   * @param type
   * @param handle
   * @throws JMSException
   */
  private void publishNotification(int type, SubscriptionHandle handle) throws JMSException {
    if (notificationsEnabled) {
      cat.info("publishNotification(" + type + ", " + handle.getSubscriptionTopic() + ", " + handle.getSubscriptionSelector() + ")");
      notificationMessage.setIntProperty(NotificationHelper.NOTIFICATION_TYPE_PROPERTY, type);
      notificationMessage.setStringProperty(NotificationHelper.SELECTOR_PROPERTY, handle.getSubscriptionSelector());
      notificationMessage.setStringProperty(NotificationHelper.TOPIC_PROPERTY, handle.getSubscriptionTopic());
      notificationMessage.setStringProperty(NotificationHelper.SUBSCRIPTION_ID_PROPERTY, subscriberId + "@" + handle.getSubscriptionToken());

      Topic t = null;

      try {
        t = createTopic(NotificationHelper.CarrierTopics[type]);
      } catch (NamingException ne) {
        ne.printStackTrace();
      }

      notificationPublisher.publish(t, notificationMessage);
    }
  }
}


/*--- Formatted in Sun Java Convention Style on Mon, Aug 6, '01 ---*/
/*------ Formatted by Jindent 3.23 Gold 1.02 --- http://www.jindent.de ------*/
