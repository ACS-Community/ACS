package cern.cmw.mom.test;



// Import the CMW pubsub topic API
import cern.cmw.mom.pubsub.MOMException;
import cern.cmw.mom.pubsub.PubSubFactory;
import cern.cmw.mom.pubsub.Publisher;
import cern.cmw.mom.pubsub.Subscriber;
import cern.cmw.mom.pubsub.SubscriptionListener;
import cern.cmw.mom.util.TopicAdminHelper;

import javax.jms.JMSException;
import javax.jms.MapMessage;

import javax.naming.NamingException;

import org.apache.log4j.Category;


/**
 * The HelloWorld example. A publisher publishes the message 'Hello World!' to
 * the topic CMW.TEMP.Hello and a subscriber subscribes to it.
 */
public class HelloWorld implements SubscriptionListener {

  /**
   * The Publisher instance.
   */
  private Publisher pub = null;

  /**
   * The Subscriber instance.
   */
  private Subscriber sub = null;

  /**
   * The subscription token.
   */
  private long subscriptionToken = 0;

  /**
   * The Topic name.
   */
  private static final String HELLO_TOPIC = "CMW.TEMP.Hello";

  /**
   * The message text.
   */
  private static final String MSG_TXT = "Hello World !!";

  /**
   * The MapMessage message fields.
   */
  private static final String TEXT_MAP_FIELD      = "TXT";
  private static final String TIMESTAMP_MAP_FIELD = "TS";

  private static final Category cat = Category.getInstance(HelloWorld.class.getName());
  /**
   * Instantiates Publisher & Subscriber, sets up the subscription
   * and publishes the message
   */
  public HelloWorld() {

    cat.info("starting...");

    try {
      pub = PubSubFactory.publisher();
      sub = PubSubFactory.subscriber();
      subscribeToHelloWorld();
      publishToHelloWorld();
    } catch (MOMException me) {
      me.printStackTrace();
    }
  }

  /**
   * Sets up the subscription.
   */
  public void subscribeToHelloWorld() {

    try {
      subscriptionToken = sub.subscribe(HELLO_TOPIC, this, null);
    } catch (Exception e) {
      cat.info("Exception caught", e);
    }
  }

  /**
   * Preapares the message and publishes it.
   */
  public void publishToHelloWorld() {

    try {
      MapMessage message = pub.createMapMessage();

      message.setString(TEXT_MAP_FIELD, MSG_TXT);
      message.setLong(TIMESTAMP_MAP_FIELD, System.currentTimeMillis());
      pub.publish(HELLO_TOPIC, message);
    } catch (Exception e) {
      cat.info("Exception caught", e);
    }
  }

  /**
   * Unsubscribes and deallocates both Subscriber & Publisher.
   */
  public void close() {

    cat.info("closing...");

    try {
      sub.unSubscribe(subscriptionToken);
    } catch (Exception e) {
      cat.info("Exception caught", e);
    }

    sub.close();
    pub.close();
  }

  /**
   * Method implementing the message handler defined in the
   * SubscriptionListener interface.
   *
   * @param m
   */
  public void onMessage(javax.jms.Message m) {

    try {
      MapMessage     message = (MapMessage) m;
      String         txt     = message.getString(TEXT_MAP_FIELD);
      java.util.Date date    =
        new java.util.Date(message.getLong(TIMESTAMP_MAP_FIELD));

      cat.info("got message : [" + date.toString() + "] " + txt);
    } catch (Exception e) {
      cat.info("Exception caught", e);
    }

    close();
  }

  /**
   * Method main
   *
   *
   * @param args
   *
   */
  public static void main(String[] args) {

    HelloWorld helloWorld1 = new HelloWorld();
  }
}

