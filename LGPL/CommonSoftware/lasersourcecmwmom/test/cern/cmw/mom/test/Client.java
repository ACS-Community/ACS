package cern.cmw.mom.test;



import cern.cmw.mom.pubsub.*;
import cern.cmw.mom.util.TopicAdminHelper;


/**
 * This example shows how to setup a simple client subscribing to a topic. This
 * code shows how to get a Subscriber instance :
 * <P><blockquote><pre>
 *   try {
 *     s = PubSubFactory.subscriber();
 *   } catch (MOMException me) {
 *     System.out.println("MOMException raised while instantiating a Subscriber");
 *     me.printStackTrace();
 *   }
 * <P></blockquote></pre>
 * <P>And this shows how to implement the onMessage() method for a SubscriptionListener:
 * <P><blockquote><pre>
 * public void onMessage(javax.jms.Message message) {
 * // Subscriber implementation of the listener
 *   try {
 *     javax.jms.TextMessage msg = (javax.jms.TextMessage)message;
 *     System.out.println("Got message  : " + msg.getText());
 *     if (msg.getText().equals("bye")) {
 *       System.out.println("Time to close!");
 *       s.unSubscribe(subscriptionToken);
 *       s.close();
 *     }
 *   } catch(javax.jms.JMSException je) {
 *     System.out.println("JMSException raised while processing message: "+message);
 *     je.printStackTrace();
 *   }
 * }
 * <P></blockquote></pre>
 */
public class Client implements SubscriptionListener, ExceptionListener {

    private final static String TOPIC             = "CMW.ALARM_SYSTEM.ALARM_CATEGORY_TREE.CERN.INSTANT";
    private Subscriber          s                 = null;
    private long                subscriptionToken = 0;

    /**
     * Constructor Client
     *
     *
     */
    public Client() {

        try {
            s = PubSubFactory.subscriber();

            s.setExceptionListener(this);
        } catch (MOMException me) {
            System.out.println("MOMException raised while instantiating a Subscriber");
            me.printStackTrace();
        }

        start();
    }

    /**
     * Method start
     *
     *
     */
    public void start() {

        try {

            // Open the subscription to TOPIC
            subscriptionToken = s.subscribe(TOPIC, this, null);
        } catch (javax.jms.JMSException je) {
            System.out.println("JMSException raised while subscribing to " + TOPIC);
            je.printStackTrace();
        } catch (javax.naming.NamingException je) {
            System.out.println("JMSException raised while subscribing to " + TOPIC);
            je.printStackTrace();
        }
    }

    /**
     * Method onMessage
     *
     *
     * @param message
     *
     */
    public void onMessage(javax.jms.Message message) {

        // Subscriber implementation of the listener
        try {
            javax.jms.ObjectMessage msg = (javax.jms.ObjectMessage) message;
            Object                  obj = msg.getObject();
            String                  str = (String) obj;

            System.out.println("Got message  : " + str);

            if (str.equals("bye")) {
                System.out.println("Time to close!");
                s.unSubscribe(subscriptionToken);
                s.close();
            }
        } catch (javax.jms.JMSException je) {
            System.out.println("JMSException raised while processing message: " + message);
            je.printStackTrace();
        }
    }

    /**
     * Method onException
     *
     * @param e
     */
    public void onException(MOMException e) {

        System.out.println("onException called!!!");

        if (e.testException(MOMException.CONNECTION_LOST_EXCEPTION)) {
            System.out.println("CONNECTION_LOST_EXCEPTION");
        } else if (e.testException(MOMException.CONNECTION_RECOVERED_EXCEPTION)) {
            System.out.println("CONNECTION_RECOVERED_EXCEPTION");
        }
    }

    /**
     * Method main
     *
     *
     * @param args
     *
     */
    public static void main(String[] args) {

        //System.setProperty("cmw.mom.keepalive","10");
        Client client = new Client();
    }
}


/*--- Formatted in Sun Java Convention Style on Mon, Sep 24, '01 ---*/


/*------ Formatted by Jindent 3.23 Gold 1.02 --- http://www.jindent.de ------*/
