/*
 * ASIMessageHelper.java
 *
 * Created on February 27, 2003, 11:11 AM
 */
package cern.laser.source.alarmsysteminterface.impl;

import java.util.Collection;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Vector;

import cern.laser.source.alarmsysteminterface.FaultState;
import cern.laser.source.alarmsysteminterface.impl.message.ASIMessage;
import cern.laser.source.alarmsysteminterface.impl.message.FaultStates;


/**
 * Helper class for marshaling/unmarshaling to/from ASIMessage instances and FaultState instances collection.
 *
 * @author  fracalde
 */
public class ASIMessageHelper {
  /** Creates a new instance of ASIMessageHelper */
  private ASIMessageHelper() {
  }

  /**
   * DOCUMENT ME!
   *
   * @param states DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public static ASIMessage marshal(Collection states) {
    if (states == null) {
      throw (new IllegalArgumentException("states collection is null"));
    }

    ASIMessage asi_message = new ASIMessage();
    Iterator iterator = states.iterator();
    FaultStates fault_states = new FaultStates();

    while (iterator.hasNext()) {
      Object ref = iterator.next();

      if (ref instanceof FaultState) {
        fault_states.addFaultState(FaultStateHelper.marshal((FaultState) ref));
      } else {
        throw new IllegalArgumentException("collection does not contain FaultState instances");
      }
    }

    asi_message.setFaultStates(fault_states);

    return asi_message;
  }

  /**
   * DOCUMENT ME!
   *
   * @param asiMessage DOCUMENT ME!
   *
   * @return DOCUMENT ME!
   */
  public static Collection unmarshal(ASIMessage asiMessage) {
    if (asiMessage == null) {
      throw (new IllegalArgumentException("ASI message is null"));
    }

    Collection states = new Vector();
    FaultStates generated_states = asiMessage.getFaultStates();

    if (generated_states != null) {
      Enumeration enumer = generated_states.enumerateFaultState();

      while (enumer.hasMoreElements()) {
        cern.laser.source.alarmsysteminterface.impl.message.FaultState generated_state = (cern.laser.source.alarmsysteminterface.impl.message.FaultState) enumer.nextElement();
        states.add(FaultStateHelper.unmarshal(generated_state));
      }
    }

    return states;
  }
}
