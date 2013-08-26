package cern.cmw.mom.test;

import cern.cmw.mom.pubsub.*;
import cern.cmw.mom.mapping.*;

/**
 * This example shows how to subscribe to a device property through the pubsub API.
 * <P><blockquote><pre>
 * public void subscribeToProperty() {
 *   String topic = MappingService.mapPropertyToTopic(DEV_CLASS, DEV_INSTANCE, DEV_PROPERTY);
 *   String cycle_selector = MappingService.mapCycleSelectorToSelector(CYCLE_SELECTOR);
 *   try {
 *     subscriptionToken = sub.subscribe( topic, this, cycle_selector);
 *   } catch(javax.jms.JMSException je) {
 *     je.printStackTrace();
 *   } catch (javax.naming.NamingException ne) {
 *     ne.printStackTrace();
 *   }
 * }
 *
 * <P></blockquote></pre>
 */
public class PropertySubscription implements SubscriptionListener {
    
    private Subscriber          sub               = null;
    private long                subscriptionToken = 0;
    private static final String DEV_CLASS         = "PowerConverter";
    private static final String DEV_INSTANCE      = "PC1";
    private static final String DEV_PROPERTY      = "Current";
    private static final String CYCLE_SELECTOR    = "Cycle_X";
    
    /**
     * Constructor PropertySubscription
     *
     *
     */
    public PropertySubscription() {
        try {
            sub = PubSubFactory.subscriber();
        } catch (MOMException me) {
            System.out.println("MOMException raised while instantiating a Subscriber");
            me.printStackTrace();
        }
        subscribeToProperty();
    }
    
    /**
     * Method subscribeToProperty
     *
     *
     */
    public void subscribeToProperty() {
        try {
            String topic = MappingService.mapPropertyToTopic(DEV_CLASS, DEV_INSTANCE, DEV_PROPERTY);
            String cycle_selector = MappingService.mapCycleSelectorToSelector(CYCLE_SELECTOR);
            subscriptionToken = sub.subscribe(topic, this, cycle_selector);
        } catch (javax.jms.JMSException je) {
            je.printStackTrace();
        } catch (javax.naming.NamingException ne) {
            ne.printStackTrace();
        } catch (MOMException me) {
            me.printStackTrace();
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
        try {
            System.out.println("Got a message");
            sub.unSubscribe(subscriptionToken);
            sub.close();
        } catch (javax.jms.JMSException e) {
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
        PropertySubscription propertySubscription1 = new PropertySubscription();
    }
}


/*--- Formatted in Sun Java Convention Style on Mon, Feb 12, '01 ---*/


/*------ Formatted by Jindent 3.23 Gold 1.02 --- http://www.jindent.de ------*/
