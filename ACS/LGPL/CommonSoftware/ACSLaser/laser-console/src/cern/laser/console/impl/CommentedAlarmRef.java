/*
 * $Id: CommentedAlarmRef.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.console.impl;

import java.io.Serializable;
import java.sql.Timestamp;

import cern.laser.console.Comment;

public class CommentedAlarmRef implements Serializable {
  private String alarmId;
  private Timestamp sourceTimestamp;
  private Comment comment;

  public CommentedAlarmRef(String alarmId, Timestamp sourceTimestamp, Comment comment) {
    this.alarmId = alarmId;
    this.sourceTimestamp = sourceTimestamp;
    this.comment = comment;
  }

  public String getAlarmId() {
    return alarmId;
  }

  public void setAlarmId(String newAlarmId) {
    alarmId = newAlarmId;
  }

  public Timestamp getSourceTimestamp() {
    return sourceTimestamp;
  }

  public void setSourceTimestamp(Timestamp newSourceTimestamp) {
    sourceTimestamp = newSourceTimestamp;
  }

  public Comment getComment() {
    return comment;
  }

  public void setComment(Comment newComment) {
    comment = newComment;
  }

  public int hashCode() {
    return alarmId.hashCode();
  }

  public boolean equals(Object obj) {
    if ((obj == null) || (!(obj instanceof CommentedAlarmRef))) { return false; }
    CommentedAlarmRef ref = (CommentedAlarmRef) obj;

    return (alarmId.equals(ref.getAlarmId()) && sourceTimestamp.equals(ref.getSourceTimestamp()));
  }
}