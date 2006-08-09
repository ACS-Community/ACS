package cern.cmw.mom.test;



import cern.cmw.mom.pubsub.*;
import cern.cmw.mom.util.TopicAdminHelper;


/**
 *  Multi threaded message consumer test example. Usage : java MultithreadedConsumer thread_nr message_nr.
 *
 */
public class MultithreadedConsumer {

    class ConsumerThread extends Thread implements SubscriptionListener {

        private long subscriptionToken = 0;
        private int  identifier        = 0;
        private int  messageCounter    = 0;

        /**
         * Constructor ConsumerThread
         *
         * @param id
         */
        public ConsumerThread(int id) {

            super();

            identifier = id;
        }

        /**
         * Method run
         *
         */
        public void run() {

            try {
                subscriptionToken = subscriber.subscribe(TOPIC, this, null);

                System.out.println("Thread " + identifier + " subscribed.");
            } catch (javax.jms.JMSException jmse) {
                jmse.printStackTrace();
            } catch (javax.naming.NamingException ne) {
                ne.printStackTrace();
            }
        }

        /**
         * Method onMessage
         *
         * @param message
         */
        public void onMessage(javax.jms.Message message) {

            try {
                System.out.println("Thread " + identifier + " got message " + (messageCounter++));

                if ((messageCounter) == messageNumber) {
                    System.out.println("Thread " + identifier + " time to close!");
                    subscriber.unSubscribe(subscriptionToken);
                }
            } catch (javax.jms.JMSException je) {
                System.out.println("JMSException raised while processing message: " + message);
                je.printStackTrace();
            }
        }
    }

    private final static String TOPIC         = "CMW.TEMP.THROUGHPUT";
    private Subscriber          subscriber    = null;
    private int                 threadNumber  = 1;
    private int                 messageNumber = 1;

    /**
     * Constructor Consumer
     *
     *
     * @param threads
     * @param number
     *
     */
    public MultithreadedConsumer(int threads, int number) {

        threadNumber  = threads;
        messageNumber = number;

        try {
            subscriber = PubSubFactory.subscriber();

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

        for (int i = 0; i < threadNumber; i++) {
            System.out.println("starting thread nr " + i);

            Thread consumer_thread = new ConsumerThread(i);

            consumer_thread.start();
        }
    }

    /**
     * Method printUsage
     *
     */
    private static void printUsage() {
        System.err.println("usage: java MultithreadedConsumer <threads nr> <message nr>");
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
        if (args.length != 2) {
            printUsage();
            System.exit(1);
        }

        int                   thread_number  = Integer.parseInt(args[0]);
        int                   message_number = Integer.parseInt(args[1]);
        MultithreadedConsumer consumer       = new MultithreadedConsumer(thread_number,
                                                   message_number);
    }
}


/*--- Formatted in Sun Java Convention Style on Mon, Sep 24, '01 ---*/


/*------ Formatted by Jindent 3.23 Gold 1.02 --- http://www.jindent.de ------*/
