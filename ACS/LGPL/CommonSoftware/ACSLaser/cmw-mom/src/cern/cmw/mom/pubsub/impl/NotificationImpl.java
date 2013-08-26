package cern.cmw.mom.pubsub.impl;

import cern.cmw.mom.pubsub.Notification;
import cern.cmw.mom.pubsub.NotificationHelper;

import javax.jms.JMSException;
import javax.jms.Message;


/**
 * Implementation class.
 * @version 1.0   23 Jan 2001
 * @author   Controls Middleware Project
 * @see Notification
 */
public class NotificationImpl implements Notification {
  protected Message message = null;

  /**
   * Constructor NotificationImpl
   *
   *
   * @param m
   *
   */
  public NotificationImpl(Message m) {
    message = m;
  }

  /**
   * Method getType
   *
   *
   * @return int
   *
   * @throws JMSException
   *
   */
  public int getType() throws JMSException {
    if (message != null) {
      if (message.propertyExists(NotificationHelper.NOTIFICATION_TYPE_PROPERTY)) {
        return message.getIntProperty(NotificationHelper.NOTIFICATION_TYPE_PROPERTY);
      }
    }

    return -1;
  }
}
