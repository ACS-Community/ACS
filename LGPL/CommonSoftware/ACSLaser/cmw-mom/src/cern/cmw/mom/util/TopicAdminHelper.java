package cern.cmw.mom.util;

import cern.cmw.mom.pubsub.NotificationHelper;


/**
 * Util class. Helper class for topic namespace handling.
 * @version 1.0   23 Jan 2001
 * @author   Controls Middleware Project
 */
public final class TopicAdminHelper {
  /** The root cmw topic namespace */
  public final static String ROOT = "CMW";

  /**
   * Validate topic creation for publication.
   */
  public final static int FOR_PUBLISHING = 0;

  /**
   * Validate topic creation for subscription.
   */
  public final static int FOR_SUBSCRIBING = 1;

  /**
   * Check the topic validity with respect to the username.
   * @return boolean true if the topic is valid.
   * @param topic The topic name.
   * @param username The username.
   * @param reason The reason why the topic is about to be created.
   */
  public static boolean validateTopic(String topic, String username, int reason) {
    /* USELESS
    if (!topic.startsWith(ROOT+TOPIC_SEPARATOR))
      return false;
    */
    if ((reason == FOR_PUBLISHING) && (topic.startsWith(NotificationHelper.CarrierTopics[NotificationHelper.NOTIFICATION]))) {
      return false;
    }

    return true;
  }
}


/*--- Formatted in Sun Java Convention Style on Fri, Sep 21, '01 ---*/
/*------ Formatted by Jindent 3.23 Gold 1.02 --- http://www.jindent.de ------*/
