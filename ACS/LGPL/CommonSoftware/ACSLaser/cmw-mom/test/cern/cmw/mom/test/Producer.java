package cern.cmw.mom.test;



import cern.cmw.mom.pubsub.*;
import cern.cmw.mom.util.TopicAdminHelper;


/**
 *  Message producer test example. Usage : java Producer msg_size msg_nr delay.
 * Publishes msg_nr messages of msg _size bytes on a predefined topic
 * CMW.TEMP.
 * @see Consumer
 */
public class Producer {

    private Publisher           pub    = null;
    private int                 size   = 0;
    private int                 number = 0;
    private int                 delay  = 0;
    private final static String TOPIC  = "CMW.TEMP.THROUGHPUT";

    /**
     * Constructor Producer
     *
     *
     * @param s
     * @param n
     * @param d
     *
     */
    public Producer(int s, int n, int d) {

        try {
            size   = s;
            number = n;
            delay  = d;
            pub    = PubSubFactory.publisher();

            publish();
            pub.close();
        } catch (MOMException me) {
            me.printStackTrace();
        }
    }

    /**
     * Method publish
     *
     *
     */
    public void publish() {

        SerializableByteArray   body    = new SerializableByteArray(size);
        javax.jms.ObjectMessage message = null;

        try {
            message = pub.createObjectMessage();

            message.setObject(body);
        } catch (javax.jms.JMSException jmse) {
            jmse.printStackTrace();
        }

        System.out.println("Time : " + System.currentTimeMillis());

        try {
            for (int i = 0; i < number; i++) {
                pub.publish(TOPIC, message);

                if (delay > 0) {
                    try {
                        Thread.sleep(delay * 1000);
                    } catch (Exception e) {}
                }
            }
        } catch (javax.jms.JMSException jmse) {
            jmse.printStackTrace();
        } catch (javax.naming.NamingException ne) {
            ne.printStackTrace();
        }

        System.out.println("Time : " + System.currentTimeMillis());
    }

    /**
     * Method printUsage
     *
     */
    private static void printUsage() {
        System.err.println("usage: java Producer <message size> <message nr> <delay>");
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
        if (args.length != 3) {
            printUsage();
            System.exit(1);
        }

        int size   = Integer.parseInt(args[0]);
        int number = Integer.parseInt(args[1]);
        int delay  = Integer.parseInt(args[2]);

        //cmw.mom.pubsub.impl.Logger.LOG_ENABLED = true;
        Producer producer1 = new Producer(size, number, delay);
    }
}


/*--- Formatted in Sun Java Convention Style on Fri, Aug 3, '01 ---*/


/*------ Formatted by Jindent 3.23 Gold 1.02 --- http://www.jindent.de ------*/
