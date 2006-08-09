/*
 * AlarmSystemInterface.java
 *
 * Created on November 5, 2002, 10:17 AM
 */
package cern.laser.source.alarmsysteminterface;

import java.util.Collection;


/**
Alarm System interface for alarm generators.
@author fracalde
@version 1.0
 */
public interface AlarmSystemInterface {
  /**
   * Set the source name.
   * @param newSourceName the source name.
   */
  public void setSourceName(String newSourceName);

  /**
   * Get the source name.
   * @return the source name.
   */
  public String getSourceName();

  /**
   * Close and deallocate resources.
   */
  public void close();

  /**
   * Push a fault state.
   * @param state the fault state change to push.
   * @throws ASIException if the fault state can not be pushed.
   */
  public void push(FaultState state) throws ASIException;

  /**
   * Push a collection of fault states.
   * @param states
   * @throws ASIException if the fault state collection can not be pushed.
   */
  public void push(Collection states) throws ASIException;

  /**
   * Push the set of active fault states.
   * @param active the active fault states.
   * @throws ASIException if the fault state active list can not be pushed.
   */
  public void pushActiveList(Collection active) throws ASIException;
}
