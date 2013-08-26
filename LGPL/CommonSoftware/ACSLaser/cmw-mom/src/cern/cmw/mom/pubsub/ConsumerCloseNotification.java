package cern.cmw.mom.pubsub;


/**
 * Public interface. Notification generated when a subscription is closed.
 * @version 1.0   23 Jan 2001
 * @author   Controls Middleware Project
 */
public interface ConsumerCloseNotification extends Notification {
  /**
   * Returns the topic related to the subscription that has been closed.
   * @return String The topic name
   *
   * @throws javax.jms.JMSException
   */
  public String getTopicName() throws javax.jms.JMSException;

  /**
   * Returns the selector related to the subscription that has been closed.
   * @return String The selector
   *
   * @throws javax.jms.JMSException
   */
  public String getSelector() throws javax.jms.JMSException;

  /**
   * Return the unique subscription identifier.
   * @return String The subscription id
   *
   * @throws javax.jms.JMSException
   */
  public String getSubscriptionId() throws javax.jms.JMSException;
}

