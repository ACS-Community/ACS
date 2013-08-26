package cern.cmw.mom.pubsub;

import javax.jms.JMSException;

import javax.naming.NamingException;


/**
 * Public interface. Provide topic subscription facilities.
 * <P>Two kinds of exception are thrown :
 * <UL>
 * <LI><A HREF="../../../../api/javax/jms/JMSException.html">javax.jms.JMSException</A> are thrown for exceptions caused by a problem in
 * the underlying implementation.</LI>
 * <LI><A HREF="../../../../jndi/javax/naming/NamingException.html">javax.naming.NamingException</A> are thrown if the topic usage is not correct. Each
 * time a topic is used, either in publication or in subscription, the topic name
 * is checked against a topic namespace, to avoid namespace pollution and performance
 * degradation.</LI>
 * </UL>
 *
 * <P>This example shows a simple subscription to a topic:
 * <P><blockquote><pre>
 * try {
 *   s = PubSubFactory.subscriber();
 * } catch (MOMException momEx) { ... }
 * try {
 *   subToken = s.subscribe("CMW.DEVICES.PowerConverter.PC1.Current", listener, selector);
 *   // ...
 *   s.unSubscribe(subToken);
 *   s.close();
 * }
 * catch (JMSException jmsEx) { ... }
 * catch (NamingException namingEx) { ... }
 * </blockquote></pre>
 * where listener is an instance of a class
 * implementing the SubscriptionListener interface method <b>void onMessage(Message m)</b> and
 * selector is an optional String defining
 * a message selector.
 *
 * @see Publisher
 * @see SubscriptionListener
 * @see PubSubFactory
 * @see NotificationHelper
 * @see cern.cmw.mom.mapping.MappingService
 * @version 1.0   23 Jan 2001
 * @author   Controls Middleware Project
 */
public interface Subscriber {
  /**
   * Set the listener for exceptions handling
   *
   * @param listener The exceptions listener
   */
  public void setExceptionListener(ExceptionListener listener);

  /**
   * Close the publisher singleton instance and dealloacate any resource.
   */
  public void close();

  /**
   * Subscribe to the given topic.
   * @param topic The String representation of the topic
   * @param listener An instance of a class implementing the SubscriptionListener interface
   * @param selector The String representation of the filter. May be null.
   * @exception JMSException  if JMS fails to subscribe due to some internal JMS error.
   * @exception NamingException if there is a violation in the namespace policy.
   * @return long the unique subscription token.
   */
  public long subscribe(String topic, SubscriptionListener listener, String selector) throws JMSException, NamingException;

  /**
   * Close the subscription identified by the subscriptionToken.
   * @param subscriptionToken The subscription identifier
   * @exception JMSException  if JMS fails to unsubscribe due to some internal JMS error.
   */
  public void unSubscribe(long subscriptionToken) throws JMSException;

  /**
   * Close all the opened subscriptions.
   * @exception JMSException  if JMS fails to unsubscribe due to some internal JMS error.
   */
  public void unSubscribeAll() throws JMSException;
}


/*--- Formatted in Sun Java Convention Style on Fri, Aug 3, '01 ---*/
/*------ Formatted by Jindent 3.23 Gold 1.02 --- http://www.jindent.de ------*/
