/*
 * ASISubscriptionListener.java
 *
 * Created on February 28, 2003, 11:34 AM
 */
package cern.laser.source.alarmsysteminterface.impl;

import java.sql.Timestamp;
import java.util.Collection;

import javax.jms.Message;
import javax.jms.TextMessage;

import org.apache.log4j.Category;

import cern.cmw.mom.pubsub.SubscriptionListener;
import cern.laser.source.alarmsysteminterface.impl.message.ASIMessage;
import cern.laser.source.alarmsysteminterface.listener.ASIListener;


/**
 * Listener class for alarm source subscription.
 * @author  fracalde
 */
public class ASISubscriptionListener implements SubscriptionListener {
  /** logging category
   */
  private static Category cat = Category.getInstance(ASISubscriptionListener.class.getName());

  /** the ASI listener instance
   */
  private ASIListener listener = null;

  /** Creates a new instance of ASISubscriptionListener */
  public ASISubscriptionListener(ASIListener l) {
    if (l == null) {
      throw (new IllegalArgumentException("listener is null"));
    }

    listener = l;
  }

  /**
   * Callback method implementation.
   * @param message the JMS message received.
   */
  public void onMessage(Message message) {
    cat.debug("got a message");

    if (message instanceof TextMessage) {
      try {
        TextMessage text_message = (TextMessage) message;
        String xml_message = text_message.getText();
        ASIMessage asi_message = XMLMessageHelper.unmarshal(xml_message);
        String source_name = asi_message.getSourceName();
        String source_hostname = asi_message.getSourceHostname();
        boolean backup = asi_message.getBackup();
        String source_timestamp = asi_message.getSourceTimestamp(); // ISO format
        Collection states = ASIMessageHelper.unmarshal(asi_message);
        listener.onMessage(source_name, source_hostname, source_timestamp, backup, states);
      } catch (Exception e) {
        cat.error("exception caught : " + e.getMessage(), e);
      }
    }
  }
}
