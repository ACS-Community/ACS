/*
 * ASISubscriberProxy.java
 *
 * Created on February 27, 2003, 12:06 PM
 */
package cern.laser.source.alarmsysteminterface.impl;

import org.apache.log4j.Category;

import cern.cmw.mom.pubsub.PubSubFactory;
import cern.cmw.mom.pubsub.Subscriber;
import cern.laser.source.alarmsysteminterface.ASIException;
import cern.laser.source.alarmsysteminterface.impl.configuration.ASIConfiguration;
import cern.laser.source.alarmsysteminterface.listener.ASIListener;
import cern.laser.source.alarmsysteminterface.listener.ASISubscriber;


/**
 * Implement the ASISubscriber interface.
 * @author  fracalde
 */
public class ASISubscriberProxy implements ASISubscriber {
  private static Subscriber subscriber = null;

  /** logging category
   */
  private static Category cat = Category.getInstance(ASISubscriberProxy.class.getName());
  private ASIConfiguration configuration = null;

  /** Creates a new instance of ASISubscriberProxy */
  public ASISubscriberProxy() throws ASIException {
    try {
      Configurator configurator = new Configurator();
      configuration = configurator.getConfiguration();

      if (subscriber == null) {
        subscriber = PubSubFactory.subscriber();
      }
    } catch (Exception e) {
      ASIException asi_ex = new ASIException("unable to create a new ASISubscriber instance : " + e.getMessage());
      asi_ex.setRootCause(e);
      throw (asi_ex);
    }
  }

  /** Close the resources.
   *
   */
  public void close() {
    cat.info("closing...");
    subscriber.close();
  }

  /** Subscribe to ASIMessage from a specific alarm source.
   *
   */
  public long subscribe(String source, ASIListener listener) throws ASIException {
    cat.info("subscribing to " + source + "...");

    try {
      return subscriber.subscribe(configuration.getAlarmsTopic() + "." + ((source == null) ? "#" : source), new ASISubscriptionListener(listener), null);
    } catch (Exception e) {
      ASIException asi_ex = new ASIException("unable to subscribe : " + e.getMessage());
      asi_ex.setRootCause(e);
      throw (asi_ex);
    }
  }

  /** Subscribe to ASIMessage.
   *
   */
  public long subscribeAll(ASIListener listener) throws ASIException {
    cat.info("subscribing to all the alarm sources...");

    return subscribe(null, listener);
  }

  /** Unsubscribe.
   *
   */
  public void unsubscribe(long token) throws ASIException {
    cat.info("unsubscribing...");

    try {
      subscriber.unSubscribe(token);
    } catch (Exception e) {
      ASIException asi_ex = new ASIException("unable to unsubscribe : " + e.getMessage());
      asi_ex.setRootCause(e);
      throw (asi_ex);
    }
  }
}
