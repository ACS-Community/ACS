/*
 * AlarmSystemInterfaceFactory.java
 *
 * Created on March 5, 2002, 5:37 PM
 */
package cern.laser.source.alarmsysteminterface;

import cern.laser.source.alarmsysteminterface.impl.AlarmSystemInterfaceProxy;
import cern.laser.source.alarmsysteminterface.impl.FaultStateImpl;


/**
 * Factory class for creating new instances of alarm system interface.
 * @author  fracalde
 */
public class AlarmSystemInterfaceFactory {
  /** Default constructor.
   */
  protected AlarmSystemInterfaceFactory() {
  }

  /** Factory method for creating FaultState instances.
   * @return a new FaultState instance.
   *
   */
  public static FaultState createFaultState() {
    return new FaultStateImpl();
  }

  /** Factory method for creating FaultState instances.
   * @return a new FaultState instance.
   * @param family the fault family.
   * @param member the fault member.
   * @param code the fault code.
   */
  public static FaultState createFaultState(String family, String member, int code) {
    return new FaultStateImpl(family, member, code);
  }

  /**
   * Create a new instance of an alarm system interface.
   * @param sourceName the source name.
   * @return the interface instance.
  * @throws ASIException if the AlarmSystemInterface instance can not be created.
   */
  public static AlarmSystemInterface createSource(String sourceName) throws ASIException {
    return new AlarmSystemInterfaceProxy(sourceName);
  }

  /**
   * Create a new instance of an alarm system interface without binding it to any source.
   * @return the interface instance.
  * @throws ASIException if the AlarmSystemInterface instance can not be created.
   */
  public static AlarmSystemInterface createSource() throws ASIException {
    return new AlarmSystemInterfaceProxy("UNDEFINED");
  }
}
