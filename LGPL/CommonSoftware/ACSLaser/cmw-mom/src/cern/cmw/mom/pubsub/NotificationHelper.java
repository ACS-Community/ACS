package cern.cmw.mom.pubsub;

import cern.cmw.mom.pubsub.impl.ConsumerAliveNotificationImpl;
import cern.cmw.mom.pubsub.impl.ConsumerCloseNotificationImpl;
import cern.cmw.mom.pubsub.impl.ConsumerOpenNotificationImpl;
import cern.cmw.mom.pubsub.impl.NotificationImpl;
import cern.cmw.mom.util.TopicAdminHelper;

import org.apache.log4j.Category;

import javax.jms.JMSException;
import javax.jms.Message;


/**
 * Public class. Helper class for notification handling.
 * Several mission-critical applications require a capability
 * to be notified about special events such as the creation/destruction of a
 * subscription. Some client might also be interested in knowing if a specific
 * topic is beeing subscribed. A notification class hierarchy corrensponds to
 * the different type of notifications published by the system on specific
 * administrative topics. The helper class offers fields and static methods to
 * deal with Notifications.
 * @version 1.0   23 Jan 2001
 * @author   Controls Middleware Project
 * @see Notification
 */
public final class NotificationHelper {
  static Category cat = Category.getInstance(NotificationHelper.class.getName());
  private final static String T_NOTIFICATIONS_ROOT = "CMW.ADMIN.NOTIFICATIONS";
  private final static String T_NOTIFICATIONS = "CMW.ADMIN.NOTIFICATIONS.#";
  private final static String T_CONSUMER_NOTIFICATIONS = T_NOTIFICATIONS_ROOT + ".CONSUMER.#";
  private final static String T_OPEN_SUB_NOTIFICATIONS = T_NOTIFICATIONS_ROOT + ".CONSUMER.OPEN_SUBSCRIPTION";
  private final static String T_CLOSE_SUB_NOTIFICATIONS = T_NOTIFICATIONS_ROOT + ".CONSUMER.CLOSE_SUBSCRIPTION";
  private final static String T_KEEP_ALIVE_NOTIFICATIONS = T_NOTIFICATIONS_ROOT + ".CONSUMER.KEEP_ALIVE_SUBSCRIPTION";

  // ...

  /**
   * Array containing the topics used for distributing notifications.
   */
  public final static String[] CarrierTopics = {
    T_NOTIFICATIONS, T_CONSUMER_NOTIFICATIONS, T_OPEN_SUB_NOTIFICATIONS,
    T_CLOSE_SUB_NOTIFICATIONS, T_KEEP_ALIVE_NOTIFICATIONS
  };

  /**
   * Generic notification.
   */
  public final static int NOTIFICATION = 0;

  /**
   * Subscription related notification.
   */
  public final static int CONSUMER_NOTIFICATION = 1;

  /**
   * Subscriber open notification.
   */
  public final static int CONSUMER_OPEN_NOTIFICATION = 2;

  /**
   * Subscriber close notification.
   */
  public final static int CONSUMER_CLOSE_NOTIFICATION = 3;

  /**
   * Subscription keep-alive notification.
   */
  public final static int CONSUMER_ALIVE_NOTIFICATION = 4;

  /**
   * Message property name identifying the notification type.
   */
  public final static String NOTIFICATION_TYPE_PROPERTY = "NOTIFICATION_TYPE";

  /**
   * Message property name identifying the topic the notification is about.
   */
  public final static String TOPIC_PROPERTY = "TOPIC";

  /**
   * Message property name identifying the selector the notification is about.
   */
  public final static String SELECTOR_PROPERTY = "SELECTOR";

  /**
   * Message property name identifying the subscription the notification is about.
   */
  public final static String SUBSCRIPTION_ID_PROPERTY = "SUBSCRIPTION_ID";

  /**
   * Return true if the topic is a notification topic.
   * @param topic The topic to check.
   * @return boolean
   */
  public static final boolean isNotification(String topic) {
    if (topic == null) {
      return false;
    }

    return topic.startsWith(T_NOTIFICATIONS_ROOT);
  }

  /**
   * Convert a Message into a Notification.
   * @param m The Message to convert
   * @return Notification The Notification instance
   */
  public static Notification messageToNotification(Message m) {
    if (m != null) {
      try {
        if (m.propertyExists(NOTIFICATION_TYPE_PROPERTY)) {
          switch (m.getIntProperty(NOTIFICATION_TYPE_PROPERTY)) {
          case NOTIFICATION:
            return new NotificationImpl(m);

          case CONSUMER_OPEN_NOTIFICATION:
            return new ConsumerOpenNotificationImpl(m);

          case CONSUMER_CLOSE_NOTIFICATION:
            return new ConsumerCloseNotificationImpl(m);

          case CONSUMER_ALIVE_NOTIFICATION:
            return new ConsumerAliveNotificationImpl(m);

          default:
            cat.error("Unknown NOTIFICATION_TYPE");

            return null;
          }
        }
      } catch (JMSException e) {
        cat.error("Unable to unwrap the Notification");
        e.printStackTrace();
      }
    }

    return null;
  }
}


/*--- Formatted in Sun Java Convention Style on Mon, Sep 24, '01 ---*/
/*------ Formatted by Jindent 3.23 Gold 1.02 --- http://www.jindent.de ------*/
