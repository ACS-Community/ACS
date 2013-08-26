package cern.cmw.mom.test;



import cern.cmw.mom.pubsub.*;
import cern.cmw.mom.util.TopicAdminHelper;


//import org.apache.log4j.Category;
//import org.apache.log4j.Priority;

/**
 * This example shows how to setup a server publishing on a topic and
 * listening to CONSUMER_NOTIFICATION events. It enables the actual msg publishing
 * as it gets a CONSUMER_OPEN_NOTIFICATION msg and stops publishing when receives
 * a CONSUMER_CLOSE_NOTIFICATION msg. It also sends an initialization msg to
 * the new subscriber. This is the code that initializes the Subscriber :
 * <P><blockquote><pre>
 * // Build the selector on the notification messages, only interested in notifications related to the topic TOPIC
 * String selector = new String( NotificationHelper.TOPIC_PROPERTY+" = '"+TOPIC+"'");
 * try {
 *   // Subscribe to CONSUMER_OPEN_NOTIFICATION and CONSUMER_CLOSE_NOTIFICATION events with the defined selector
 *   openSubscriptionToken = s.subscribe(NotificationHelper.CarrierTopics[NotificationHelper.CONSUMER_OPEN_NOTIFICATION],
 *                                       this,
 *                                       selector);
 *   closeSubscriptionToken = s.subscribe(NotificationHelper.CarrierTopics[NotificationHelper.CONSUMER_CLOSE_NOTIFICATION],
 *                                        this,
 *                                        selector);
 * } catch (javax.jms.JMSException je) {
 *   System.out.println("JMSException raised while subscribing to notifications");
 *   je.printStackTrace();
 * } catch (javax.naming.NamingException ne) {
 *   System.out.println("NamingException raised while subscribing to notifications");
 *   ne.printStackTrace();
 * }
 *
 * <P></blockquote></pre>
 * <P>And this is how notifications are handled :
 * <P><blockquote><pre>
 * public void onMessage(javax.jms.Message message) {
 *   System.out.println("Got a notification");
 *   try {
 *     // Convert the Message into a Notification
 *     Notification n = NotificationHelper.messageToNotification(message);
 *     int type = n.getType();
 *     switch (type) {
 *       case NotificationHelper.CONSUMER_OPEN_NOTIFICATION:
 *       {
 *         // A subscription has been opened
 *         System.out.println("CONSUMER_OPEN_NOTIFICATION");
 *         ConsumerOpenNotification on = (ConsumerOpenNotification)n;
 *         // Get the subscription topic name
 *         String topicName = on.getTopicName();
 *         // Get the subscription identification
 *         String subscriptionId = on.getSubscriptionId();
 *         // Prepare a message to initialize the new subscription with the current 'satus'
 *         javax.jms.TextMessage m = p.createTextMessage();
 *         m.setText(status);
 *         m.setStringProperty(NotificationHelper.SUBSCRIPTION_ID_PROPERTY, subscriptionId);
 *         try {
 *           p.publish(topicName,m);
 *         } catch (javax.naming.NamingException ne) {
 *           System.out.println("NamingException raised while publishing an initialization msg");
 *           ne.printStackTrace();
 *         }
 *         // Enable msg publication, someone is listening
 *         publicationEnabled = true;
 *         break;
 *       }
 *       case NotificationHelper.CONSUMER_CLOSE_NOTIFICATION:
 *       {
 *         System.out.println("CONSUMER_CLOSE_NOTIFICATION");
 *         // Disable msg publication, subscription has been closed
 *         publicationEnabled = false;
 *         break;
 *       }
 *     }
 *   } catch(javax.jms.JMSException e) {
 *     System.out.println("JMSException raised while handling a notification");
 *     e.printStackTrace();
 *   }
 * }
 * <P></blockquote></pre>
 *
 */
public class Server implements SubscriptionListener {

    //    static Category                                         cat              =
    //        Category.getInstance(Server.class.getName());
    private final static String TOPIC                  = "CMW.ALARM_SYSTEM.ALARM_CATEGORY_TREE.CERN.INSTANT";
    private Publisher           p                      = null;
    private Subscriber          s                      = null;
    private long                openSubscriptionToken  = 0;
    private long                closeSubscriptionToken = 0;
    private boolean             publicationEnabled     = false;
    private String              status                 = null;

    /**
     * Constructor Server
     *
     *
     */
    public Server() {

        try {
            p = PubSubFactory.publisher();
            s = PubSubFactory.subscriber();
        } catch (MOMException me) {
            System.out.println("MOMException raised while instantiating pub&sub objects");
            me.printStackTrace();
        }

        status = new String("INITIAL STATUS");

        start();
    }

    /**
     * Method start
     *
     */
    private void start() {

        // Build the selector on the notification messages:
        // 1. Not interested to CONSUMER_ALIVE_NOTIFICATION notifications.
        // 2. Only interested in notifications related to the topic TOPIC
        String selector = new String(NotificationHelper.TOPIC_PROPERTY + " = '" + TOPIC + "'");

        try {

            // Subscribe to CONSUMER_OPEN_NOTIFICATION and CONSUMER_CLOSE_NOTIFICATION events with the defined selector
            openSubscriptionToken  =
                s.subscribe(NotificationHelper
                    .CarrierTopics[NotificationHelper.CONSUMER_OPEN_NOTIFICATION], this, selector);
            closeSubscriptionToken =
                s.subscribe(NotificationHelper
                    .CarrierTopics[NotificationHelper.CONSUMER_CLOSE_NOTIFICATION], this, selector);
        } catch (javax.jms.JMSException je) {
            System.out.println("JMSException raised while subscribing to notifications");
            je.printStackTrace();
        } catch (javax.naming.NamingException ne) {
            System.out.println("NamingException raised while subscribing to notifications");
            ne.printStackTrace();
        }

        // Read msgs from the stdin and send them if someone has subscribed
        try {
            java.io.BufferedReader in  =
                new java.io.BufferedReader(new java.io.InputStreamReader(System.in));
            String                 str = null;

            do {
                System.out.println("Messaggio : ");

                str    = in.readLine();
                status = str;

                if (publicationEnabled) {
                    try {
                        javax.jms.ObjectMessage text = p.createObjectMessage();

                        text.setObject(str);
                        p.publish(TOPIC, text);
                    } catch (javax.jms.JMSException je) {
                        je.printStackTrace();
                    } catch (javax.naming.NamingException ne) {
                        ne.printStackTrace();
                    }
                }
            } while (!str.equals("bye"));
        } catch (java.io.IOException ioe) {
            System.out.println("IOException raised while reading from stdin");
        }

        try {
            s.unSubscribe(openSubscriptionToken);
            s.unSubscribe(closeSubscriptionToken);
        } catch (javax.jms.JMSException je) {
            System.out.println("JMSException raised while unsubscribing from notifications");
            je.printStackTrace();
        }

        s.close();
        p.close();
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
        System.out.println("Got a notification");

        try {

            // Convert the Message into a Notification
            Notification n    = NotificationHelper.messageToNotification(message);
            int          type = n.getType();

            switch (type) {

            case NotificationHelper.CONSUMER_OPEN_NOTIFICATION : {

                // A subscription has been opened
                System.out.println("CONSUMER_OPEN_NOTIFICATION");

                ConsumerOpenNotification on             = (ConsumerOpenNotification) n;
                String                   topicName      = on.getTopicName();
                String                   subscriptionId = on.getSubscriptionId();
                String                   selector       = on.getSelector();

                System.out.println("TOPIC : " + topicName + ", SELECTOR : " + selector);

                javax.jms.ObjectMessage m = p.createObjectMessage();

                m.setObject(status);
                m.setStringProperty(NotificationHelper.SUBSCRIPTION_ID_PROPERTY, subscriptionId);

                try {
                    p.publish(topicName, m);
                } catch (javax.naming.NamingException ne) {
                    System.out
                        .println("NamingException raised while publishing an initialization msg");
                    ne.printStackTrace();
                }

                System.out.println("Enabling publication...");

                publicationEnabled = true;

                break;
            }
            case NotificationHelper.CONSUMER_CLOSE_NOTIFICATION : {
                System.out.println("CONSUMER_CLOSE_NOTIFICATION");
                System.out.println("Disabling publication...");

                publicationEnabled = false;

                break;
            }
            }
        } catch (javax.jms.JMSException e) {
            System.out.println("JMSException raised while handling a notification");
            e.printStackTrace();
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

        //Category.getInstance("cern.cmw.mom.pubsub").setPriority(Priority.INFO);
        //cat.setPriority(Priority.DEBUG);
        //Category.getRoot().setPriority(Priority.DEBUG);
        //Category.getInstance("cern.cmw.mom.pubsub").setPriority(Priority.INFO);
        Server serevr = new Server();
    }
}


/*--- Formatted in Sun Java Convention Style on Mon, Sep 24, '01 ---*/


/*------ Formatted by Jindent 3.23 Gold 1.02 --- http://www.jindent.de ------*/
