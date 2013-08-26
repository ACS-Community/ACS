package cern.cmw.mom.mapping;

import cern.cmw.mom.pubsub.MOMException;
import cern.cmw.mom.pubsub.SubscriptionListener;
import cern.cmw.mom.util.TopicAdminHelper;

import javax.jms.JMSException;
import javax.jms.Message;


/**
 * Service class. Provides mapping functionalities to :
 * <UL>
 * <LI>associate a class/device/property triplet to the corresponding topic</LI>
 * <LI>associate a cycle selector to the corresponding subscription selector</LI>
 * </UL>
 * The device topic namespace starts at CMW.DEVICES.
 * Static field CYCLE_SELECTOR_PROPERTY determines the message string property
 * that handles a cycle selector string.
 *
 * @version 1.0   23 Jan 2001
 * @author   Controls Middleware Project
 * @see cern.cmw.mom.pubsub.Publisher
 * @see cern.cmw.mom.pubsub.Subscriber
 * @see cern.cmw.mom.util.TopicAdminHelper
 */
public final class MappingService {
  /**
   * Static field that holds the property name to be used for filtering on cycle
   * selector.
   */
  public final static String CYCLE_SELECTOR_PROPERTY = "CYCLE_SELECTOR";

  /**
   * Static method to automatically get the topic name associated with the triplet
   * deviceClass, deviceName, propertyName
   * @param deviceClass The device class
   * @param deviceName The device name
   * @param propertyName The property name
   * @return String The topic associated to the triplet
   * @throws MOMException A MOMException
   */
  public static String mapPropertyToTopic(String deviceClass,
    String deviceName, String propertyName) throws MOMException {
    if ((deviceClass == null) || (deviceName == null) ||
        (propertyName == null)) {
      throw new MOMException("Unable to create Topic : null parameter");
    }

    String topic = new String("CMW.DEVICES." + deviceClass + "." + deviceName +
        "." + propertyName);

    return topic;
  }

  /**
   * Static method to automatically create the message selector associated with the given cycle selector string
   * @param cycle The cycle selector string representation
   * @return String The corresponding message selector. If cycle is null, the message selector filters on null CYCLE_SELECTOR_PROPERTY.
   * @throws MOMException A MOMException
   */
  public static String mapCycleSelectorToSelector(String cycle)
    throws MOMException {
    String selector = null;

    if (cycle == null) {
      selector = new String(CYCLE_SELECTOR_PROPERTY + " IS NULL");
    } else {
      selector = new String(CYCLE_SELECTOR_PROPERTY + " = '" + cycle + "'");
    }

    return selector;
  }
}

