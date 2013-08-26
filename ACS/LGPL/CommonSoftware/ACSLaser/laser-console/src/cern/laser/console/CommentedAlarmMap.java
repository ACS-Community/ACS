/*
 * $Id: CommentedAlarmMap.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.console;

import java.io.Serializable;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * A generic commented alarm container. It defines a mapping between alarm identifiers and commented alarms.
 * 
 * @author F.Calderini
 * @see cern.laser.console.CommentedAlarm
 * @see cern.laser.console.Comment
 */
public class CommentedAlarmMap implements Serializable {
  private Map adaptee = null;

  /**
   * Constructor.
   */
  public CommentedAlarmMap() {
    adaptee = Collections.synchronizedMap(new HashMap());
  }

  /**
   * Maps the specified commented alarm in this hashtable.
   * 
   * @param alarm the commented alarm.
   * @return the previous commented alarm in this hashtable, or null if it did not have one.
   */
  public CommentedAlarm put(CommentedAlarm alarm) {
    return (CommentedAlarm) adaptee.put(alarm.getAlarm().getAlarmId(), alarm);
  }

  /**
   * Copies all the mappings from the specified CommentedAlarmMap to this one.
   * 
   * @param alarms the commented alarm map.
   */
  public void putAll(CommentedAlarmMap alarms) {
    Map map = new HashMap();
    Iterator iterator = alarms.values().iterator();
    while (iterator.hasNext()) {
      CommentedAlarm alarm = (CommentedAlarm) iterator.next();
      map.put(alarm.getAlarm().getAlarmId(), alarm);
    }
    adaptee.putAll(map);
  }

  /**
   * Returns the commented alarm to which the specified alarm identifier is mapped in this hashtable.
   * 
   * @param alarmId the alarm identifier.
   * @return the commented alarm to which the alarm identifier is mapped in this hashtable; null if the alarm identifier
   *         is not mapped to any commented alarm in this hashtable.
   */
  public CommentedAlarm get(String alarmId) {
    return (CommentedAlarm) adaptee.get(alarmId);
  }

  /**
   * Removes the commented alarm to which the specified alarm identifeir is mapped in this hashtable.
   * 
   * @param alarmId the alarm to remove.
   * @return the commented alarm to which the alarm identifier had been mapped in this hashtable; null if the alarm
   *         identifier was not mapped to any commented alarm in this hashtable.
   */
  public CommentedAlarm remove(String alarmId) {
    return (CommentedAlarm) adaptee.remove(alarmId);
  }

  /**
   * Tests if the specified alarm is a key in this hashtable.
   * 
   * @param alarmId the alarm to check.
   * @return true if and only if the specified alarm is a key in this hashtable, false otherwise
   */
  public boolean containsKey(String alarmId) {
    return adaptee.containsKey(alarmId);
  }

  /**
   * Clears this hashtable so that it contains no alarms.
   */
  public void clear() {
    adaptee.clear();
  }

  /**
   * Tests if this hashtable maps no alarms to comments.
   * 
   * @return true if this hashtable maps no alarms to comments; false otherwise.
   */
  public boolean isEmpty() {
    return adaptee.isEmpty();
  }

  /**
   * Returns a collection of the commented alarms in this hashtable.
   * 
   * @return a collection of the commented alarms in this hashtable.
   */
  public Collection values() {
    return adaptee.values();
  }

  /**
   * Returns the number of commented alarms in this hashtable.
   * 
   * @return the number of commented alarms in this hashtable.
   */
  public int size() {
    return adaptee.size();
  }

  /**
   * Returns a string representation.
   * 
   * @return a string representation.
   */
  public String toString() {
    return values().toString();
  }
}