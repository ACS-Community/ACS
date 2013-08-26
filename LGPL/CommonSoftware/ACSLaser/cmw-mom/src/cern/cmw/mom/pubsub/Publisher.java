package cern.cmw.mom.pubsub;

import javax.jms.BytesMessage;
import javax.jms.JMSException;
import javax.jms.MapMessage;
import javax.jms.Message;
import javax.jms.ObjectMessage;
import javax.jms.StreamMessage;
import javax.jms.TextMessage;

import javax.naming.NamingException;


/**
 * Public interface. Provide methods to create and publish messages on topics.
 * The standard JMS specified message types are supported :
 * <UL>
 * <LI><A HREF="../../../../../api/javax/jms/Message.html">Message</A></LI>
 * <LI><A HREF="../../../../../api/javax/jms/TextMessage.html">TextMessage</A></LI>
 * <LI><A HREF="../../../../../api/javax/jms/StreamMessage.html">StreamMessage</A></LI>
 * <LI><A HREF="../../../../../api/javax/jms/BytesMessage.html">BytesMessage</A></LI>
 * <LI><A HREF="../../../../../api/javax/jms/MapMessage.html">MapMessage</A></LI>
 * <LI><A HREF="../../../../../api/javax/jms/ObjectMessage.html">ObjectMessage</A></LI>
 * </UL>
 * <P>Two kinds of exception are thrown :
 * <UL>
 * <LI><A HREF="../../../../../api/javax/jms/JMSException.html">javax.jms.JMSException</A> are thrown for exceptions caused by a problem in
 * the underlying implementation.</LI>
 * <LI><A HREF="../../../../../jndi/javax/naming/NamingException.html">javax.naming.NamingException</A> are thrown if the topic usage is not correct. Each
 * time a topic is used, either in publication or in subscription, the topic name
 * is checked against the topic namespace, to avoid namespace pollution and performance
 * degradation</LI>
 * </UL>
 *
 * <P>This example shows a simple publication of a TextMessage on a topic:
 * <P><blockquote><pre>
 * try {
 *   Publisher p = PubSubFactory.publisher();
 * } catch (MOMException momEx) { ... }
 * // ...
 * try {
 *   TextMessage msg = p.createTextMessage();
 *   msg.setText("This is the text");
 *   p.publish("CMW.DEVICES.PowerConverter.PC1.Current", msg);
 *   // ...
 *   p.close();
 * }
 * catch (JMSException jmsEx) { ... }
 * catch (NamingException namingEx) { ... }
 * </blockquote></pre>
 *
 * @version 1.0   23 Jan 2001
 * @author   Controls Middleware Project
 * @see PubSubFactory
 * @see Subscriber
 * @see cern.cmw.mom.mapping.MappingService
 */
public interface Publisher {
  /**
   * Set the listener for exceptions handling
   *
   * @param listener The exceptions listener
   */
  public void setExceptionListener(ExceptionListener listener);

  /**
   * Close the publisher singleton instance and dealloacate any resource
   */
  public void close();

  /**
   * Create an instance of BytesMessage.
   * @exception JMSException if JMS fails to create the message due to some internal JMS error.
   * @return BytesMessage the BytesMessage instance
   */
  public BytesMessage createBytesMessage() throws JMSException;

  /**
   * Create an instance of MapMessage.
   * @exception JMSException if JMS fails to create the message due to some internal JMS error.
   * @return MapMessage the MapMessage instance
   */
  public MapMessage createMapMessage() throws JMSException;

  /**
   * Create an instance of Message.
   * @exception JMSException if JMS fails to create the message due to some internal JMS error.
   * @return Message the Message instance
   */
  public Message createMessage() throws JMSException;

  /**
   * Create an instance of ObjectMessage.
   * @exception JMSException if JMS fails to create the message due to some internal JMS error.
   * @return ObjectMessage the ObjectMessage instance
   */
  public ObjectMessage createObjectMessage() throws JMSException;

  /**
   * Create an instance of StreamMessage.
   * @exception JMSException if JMS fails to create the message due to some internal JMS error.
   * @return StreamMessage the StreamMessage instance
   */
  public StreamMessage createStreamMessage() throws JMSException;

  /**
   * Create an instance of TextMessage.
   * @exception JMSException if JMS fails to create the message due to some internal JMS error.
   * @return TextMessage the TextMessage instance
   */
  public TextMessage createTextMessage() throws JMSException;

  /**
   * Publish a message to the given topic.
   * @param topic The String representation of the topic
   * @param message The Message object to publish
   * @exception JMSException if JMS fails to publish the message due to some internal JMS error.
   * @exception NamingException  if there is a violation in the namespace policy.
   */
  public void publish(String topic, Message message) throws JMSException, NamingException;

  /**
   * Publish a message to the given topic.
   * @param topic The String representation of the topic
   * @param message The Message object to publish
   * @param deliveryMode The Message persistence (true, false)
   * @param priority The Message priority (0..9)
   * @param timeToLive The Message time to live (msec)
   * @exception JMSException if JMS fails to publish the message due to some internal JMS error.
   * @exception NamingException  if there is a violation in the namespace policy.
   */
  public void publish(String topic, Message message, int deliveryMode, int priority, long timeToLive) throws JMSException, NamingException;
}


/*--- Formatted in Sun Java Convention Style on Fri, Aug 3, '01 ---*/
/*------ Formatted by Jindent 3.23 Gold 1.02 --- http://www.jindent.de ------*/
