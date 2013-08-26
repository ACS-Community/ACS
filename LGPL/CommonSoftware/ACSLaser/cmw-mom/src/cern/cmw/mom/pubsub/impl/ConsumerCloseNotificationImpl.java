package cern.cmw.mom.pubsub.impl;

import cern.cmw.mom.pubsub.ConsumerCloseNotification;
import cern.cmw.mom.pubsub.NotificationHelper;

import javax.jms.JMSException;
import javax.jms.Message;


/**
 * Implementation class.
 * @version 1.0   23 Jan 2001
 * @author   Controls Middleware Project
 * @see ConsumerCloseNotification
 */
public class ConsumerCloseNotificationImpl extends NotificationImpl implements ConsumerCloseNotification {
  /**
   * Constructor ConsumerCloseNotificationImpl
   *
   *
   * @param m
   *
   */
  public ConsumerCloseNotificationImpl(Message m) {
    super(m);
  }

  /**
   * Method getSelector
   *
   *
   * @return String
   *
   * @throws JMSException
   *
   */
  public String getSelector() throws JMSException {
    if (message != null) {
      if (message.propertyExists(NotificationHelper.SELECTOR_PROPERTY)) {
        return message.getStringProperty(NotificationHelper.SELECTOR_PROPERTY);
      }
    }

    return null;
  }

  /**
   * Method getSubscriptionId
   *
   *
   * @return String
   *
   * @throws JMSException
   *
   */
  public String getSubscriptionId() throws JMSException {
    if (message != null) {
      if (message.propertyExists(NotificationHelper.SUBSCRIPTION_ID_PROPERTY)) {
        return message.getStringProperty(NotificationHelper.SUBSCRIPTION_ID_PROPERTY);
      }
    }

    return null;
  }

  /**
   * Method getTopicName
   *
   *
   * @return String
   *
   * @throws JMSException
   *
   */
  public String getTopicName() throws JMSException {
    if (message != null) {
      if (message.propertyExists(NotificationHelper.TOPIC_PROPERTY)) {
        return message.getStringProperty(NotificationHelper.TOPIC_PROPERTY);
      }
    }

    return null;
  }
}


/*--- Formatted in Sun Java Convention Style on Mon, Feb 12, '01 ---*/
/*------ Formatted by Jindent 3.23 Gold 1.02 --- http://www.jindent.de ------*/
