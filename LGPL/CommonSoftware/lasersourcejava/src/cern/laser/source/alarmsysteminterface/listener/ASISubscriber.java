/*
 * ASISubscriber.java
 *
 * Created on February 27, 2003, 11:45 AM
 */
package cern.laser.source.alarmsysteminterface.listener;

import cern.laser.source.alarmsysteminterface.ASIException;


/**
 * ASI subscriber interface.
 * @author  fracalde
 */
public interface ASISubscriber {
  /**
   * Close the resources.
   */
  public void close();

  /**
   * Subscribe to ASIMessage from a specific alarm source.
   * @param source the alarm source.
   * @param listener the subscription listener.
   * @throws ASIException if subscription fails.
   * @return the subscription token.
   */
  public long subscribe(String source, ASIListener listener) throws ASIException;

  /**
   * Subscribe to ASIMessage from all the alarm sources.
   * @param listener the subscription listener.
   * @throws ASIException if subscription fails.
   * @return the subscription token.
   */
  public long subscribeAll(ASIListener listener) throws ASIException;

  /**
   * Unsubscribe.
   * @param token the subscription token.
   * @throws ASIException if unsubscription fails.
   */
  public void unsubscribe(long token) throws ASIException;
}
