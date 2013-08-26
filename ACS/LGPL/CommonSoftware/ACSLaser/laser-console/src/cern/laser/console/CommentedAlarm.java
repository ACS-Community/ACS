/*
 * $Id: CommentedAlarm.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.console;

import java.io.Serializable;

/**
 * A generic commented alarm.
 * 
 * @author F.Calderini
 * @see cern.laser.console.CommentedAlarmMap
 * @see cern.laser.console.Comment
 */
public class CommentedAlarm implements Serializable, Cloneable {
  private cern.laser.client.data.Alarm alarm;
  private Comment comment;

  /**
   * Default constructor.
   */
  public CommentedAlarm() {
    this.alarm = null;
    this.comment = null;
  }

  /**
   * Constructor.
   * 
   * @param alarm the alarm
   * @param comment the alarm comment
   */
  public CommentedAlarm(cern.laser.client.data.Alarm alarm, Comment comment) {
    this.alarm = alarm;
    this.comment = comment;
  }

  /**
   * Accessor method.
   * 
   * @return the alarm
   */
  public cern.laser.client.data.Alarm getAlarm() {
    return alarm;
  }

  /**
   * Accessor method.
   * 
   * @param newAlarm the alarm
   */
  public void setAlarm(cern.laser.client.data.Alarm newAlarm) {
    alarm = newAlarm;
  }

  /**
   * Accessor method.
   * 
   * @return the alarm comment
   */
  public Comment getComment() {
    return comment;
  }

  /**
   * Accessor method.
   * 
   * @param newComment the alarm comment
   */
  public void setComment(Comment newComment) {
    comment = newComment;
  }

  /**
   * Returns a string representation.
   * 
   * @return a string representation.
   */
  public String toString() {
    return new String("[" + (alarm == null ? null : alarm.getAlarmId()) + ","
        + (comment == null ? null : comment.toString()) + "]");
  }

  public Object clone() {
    try {
      CommentedAlarm commented_alarm = (CommentedAlarm) super.clone();
      commented_alarm.setAlarm(alarm == null ? null : (cern.laser.client.data.Alarm) alarm.clone());
      commented_alarm.setComment(comment == null ? null : (Comment) comment.clone());

      return commented_alarm;
    } catch (Exception e) {
      throw new InternalError("clone failed : " + e.getMessage());
    }
  }

}