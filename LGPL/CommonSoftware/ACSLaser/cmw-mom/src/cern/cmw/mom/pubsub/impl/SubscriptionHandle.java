package cern.cmw.mom.pubsub.impl;

import javax.jms.MessageListener;
import javax.jms.TopicSession;
import javax.jms.TopicSubscriber;


/**
 * Internal class. Incapsulate all the information related to a subscription.
 * @version 1.0   23 Jan 2001
 * @author   Controls Middleware Project
 */
public class SubscriptionHandle {
  /**
   * The unique identifier generator for the subscriptions.
   */
  private static long subscriptionTokenGenerator = 1;
  private Boolean subscriptionTokenGeneratorLock = Boolean.TRUE;

  /**
   * The message listener associated to the subscription.
   */
  private MessageListener listener = null;

  /**
   * The selector associated to the subscription.
   */
  private String selector = null;

  /**
   * The topic associated to the subscription.
   */
  private String topic = null;

  /**
   * The TopicSession object associated to the subscription.
   */
  private TopicSession session = null;

  /**
   * The TopicSubscriber object associated to the subscription.
   */
  private TopicSubscriber subscriber = null;

  /**
   * The unique key that identifies a specific subscription.
   */
  private long subscriptionToken = 0;

  /**
   * Constructor for the SubscriptionHandle class.
   * @param subscriber the TopicSubscriber instance.
   * @param topic
   * @param selector
   * @param listener
   */
  public SubscriptionHandle(TopicSubscriber subscriber, String topic, String selector, MessageListener listener) {
    synchronized (subscriptionTokenGeneratorLock) {
      subscriptionToken = (subscriptionTokenGenerator++);
    }

    this.subscriber = subscriber;
    this.topic = topic;
    this.selector = selector;
    this.listener = listener;
  }

  /**
   * Default constructor for the SubscriptionHandle class.
   */
  public SubscriptionHandle() {
    synchronized (subscriptionTokenGeneratorLock) {
      subscriptionToken = (subscriptionTokenGenerator++);
    }

    subscriber = null;
    selector = null;
    listener = null;
    topic = null;
  }

  /**
   * Set the TopicSession Object
   * @param session the TopicSession instance
   */
  public void setSession(TopicSession session) {
    this.session = session;
  }

  /**
   * Return the TopicSession object.
   * @return TopicSession the topic session.
   */
  public TopicSession getSession() {
    return session;
  }

  /**
   * Set the TopicSubscriber Object
   * @param subscriber the TopicSubscriber instance
   */
  public void setSubscriber(TopicSubscriber subscriber) {
    this.subscriber = subscriber;
  }

  /**
   * Return the TopicSubscriber object.
   * @return TopicSubscriber
   */
  public TopicSubscriber getSubscriber() {
    return subscriber;
  }

  /**
   * Set the subscription listener
   * @param listener the subscription listener
   */
  public void setSubscriptionListener(MessageListener listener) {
    this.listener = listener;
  }

  /**
   * Return MessageListener associated with the subscription.
   * @return MessageListener the listener.
   */
  public MessageListener getSubscriptionListener() {
    return listener;
  }

  /**
   * Set the subscription selector
   * @param selector the subscription selector
   */
  public void setSubscriptionSelector(String selector) {
    this.selector = selector;
  }

  /**
   * Return the String defining the filter associated with the subscription.
   * @return String the defined filter.
   */
  public String getSubscriptionSelector() {
    return selector;
  }

  /**
   * Return the key identifying the subscription.
   * @return long the subscription token.
   */
  public long getSubscriptionToken() {
    return subscriptionToken;
  }

  /**
   * Set the topic name
   * @param topic the topic name
   */
  public void setSubscriptionTopic(String topic) {
    this.topic = topic;
  }

  /**
   * Return the String that identifies the subscribed topic.
   * @return String the topic name.
   */
  public String getSubscriptionTopic() {
    return topic;
  }

  /**
   * Method equals
   *
   * @param object
   * @return boolean
   */
  public boolean equals(Object object) {
    if (object instanceof SubscriptionHandle) {
      SubscriptionHandle handle = (SubscriptionHandle) object;

      return (subscriptionToken == handle.getSubscriptionToken());
    } else {
      return false;
    }
  }
}


/*--- Formatted in Sun Java Convention Style on Fri, Aug 3, '01 ---*/
/*------ Formatted by Jindent 3.23 Gold 1.02 --- http://www.jindent.de ------*/
