/*
 * ASISubscriberFactory.java
 *
 * Created on February 27, 2003, 11:52 AM
 */
package cern.laser.source.alarmsysteminterface.listener;

import cern.laser.source.alarmsysteminterface.ASIException;
import cern.laser.source.alarmsysteminterface.impl.ASISubscriberProxy;


/**
 * Factory class for creating ASISubscriber instances.
 * @author  fracalde
 */
public class ASISubscriberFactory {
  /** Creates a new instance of ASISubscriberFactory */
  private ASISubscriberFactory() {
  }

  /** Factory method. Create a new instance of ASISubscriber.
   * @return the ASISubscriber instance.
   * @throws ASIException if the ASISubscriber can not be instantiated.
   */
  public static ASISubscriber create() throws ASIException {
    return new ASISubscriberProxy();
  }
}
