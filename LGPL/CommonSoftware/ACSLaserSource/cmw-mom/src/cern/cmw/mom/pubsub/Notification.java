package cern.cmw.mom.pubsub;


/**
 * Public interface. Several mission-critical applications require a capability
 * to be notified about special events such as the creation/destruction of a
 * subscription. Some client might also be interested in knowing if a specific
 * topic is beeing subscribed. A notification class hierarchy corrensponds to
 * the different type of notifications published by the system on specific
 * administrative topics. Notification is the base class for such a hierarchy.
 * @version 1.0   23 Jan 2001
 * @author   Controls Middleware Project
 * @see NotificationHelper
 */
public interface Notification {
  /**
   * Return the type of Notification.
   * @return int The code corresponding to the notification type
   *
   * @throws javax.jms.JMSException
   */
  public int getType() throws javax.jms.JMSException;
}

