package cern.cmw.mom.pubsub.impl;

import cern.cmw.mom.pubsub.ConsumerOpenNotification;
import cern.cmw.mom.pubsub.NotificationHelper;

import javax.jms.JMSException;
import javax.jms.Message;


/**
 * Implementation class.
 * @version 1.0   23 Jan 2001
 * @author   Controls Middleware Project
 * @see ConsumerOpenNotification
 */
public class ConsumerOpenNotificationImpl extends NotificationImpl implements ConsumerOpenNotification {
  /**
   * Constructor ConsumerOpenNotificationImpl
   *
   *
   * @param m
   *
   */
  public ConsumerOpenNotificationImpl(Message m) {
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
