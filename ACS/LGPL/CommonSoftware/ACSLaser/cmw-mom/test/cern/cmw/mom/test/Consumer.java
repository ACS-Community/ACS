package cern.cmw.mom.test;



import cern.cmw.mom.pubsub.*;
import cern.cmw.mom.util.TopicAdminHelper;


/**
 *  Message consumer test example. Usage : java Consumer msg_nr.
 * Subscribes to a predefined topic CMW.TEMP and prints statistics on throughput
 * every msg_nr messages received.
 * @see Producer
 */
public class Consumer implements SubscriptionListener {

    private final static String TOPIC         = "CMW.TEMP.THROUGHPUT";
    private Subscriber          sub           = null;
    private int                 number        = 1;
    private int                 msgCounter    = 1;
    private long                prevTimestamp = 0;
    private long                currTimestamp = 0;
    private long                interval      = 0;
    private float               throughput    = 0;

    /**
     * Constructor Consumer
     *
     *
     * @param n
     *
     */
    public Consumer(int n) {

        try {
            number = n;
            sub    = PubSubFactory.subscriber();

            consume();
        } catch (MOMException me) {
            me.printStackTrace();
        }
    }

    /**
     * Method consume
     *
     */
    private void consume() {

        try {
            sub.subscribe(TOPIC, this, null);
            System.out.println("Subscribed.");
        } catch (javax.jms.JMSException jmse) {
            jmse.printStackTrace();
        } catch (javax.naming.NamingException ne) {
            ne.printStackTrace();
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

        if ((msgCounter % number) == 0) {
            currTimestamp = System.currentTimeMillis();
            interval      = currTimestamp - prevTimestamp;

            if (interval != 0) {
                throughput = (((float) number) / ((float) interval)) * 1000;
            } else {
                throughput = 0;
            }

            System.out.println("Got message #" + msgCounter + " at " + currTimestamp
                               + " msec (interval=" + interval + "msec throughput=" + throughput
                               + "msg/sec)");

            prevTimestamp = currTimestamp;
        }

        msgCounter++;
    }

    /**
     * Method printUsage
     *
     */
    private static void printUsage() {
        System.err.println("usage: java Consumer <message nr>");
    }

    /**
     * Method main
     *
     *
     * @param args
     *
     */
    public static void main(String[] args) {

        // Is there anything to do?
        if (args.length != 1) {
            printUsage();
            System.exit(1);
        }

        int number = Integer.parseInt(args[0]);

        //System.setProperty("cmw.mom.mbrokername","pcslux9");
        //System.setProperty("cmw.mom.mbrokerport","2506");
        //cmw.mom.pubsub.impl.Logger.LOG_ENABLED = true;
        Consumer consumer1 = new Consumer(number);
    }
}


/*--- Formatted in Sun Java Convention Style on Mon, Sep 24, '01 ---*/


/*------ Formatted by Jindent 3.23 Gold 1.02 --- http://www.jindent.de ------*/
