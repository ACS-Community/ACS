/*
 * $Id: Comment.java,v 1.1 2005/06/07 03:17:25 kzagar Exp $
 *
 * $Date: 2005/06/07 03:17:25 $ 
 * $Revision: 1.1 $ 
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.console;

import java.io.Serializable;

/**
 * An alarm comment.
 * 
 * @author F.Calderini
 * @see cern.laser.console.CommentedAlarmMap
 */
public class Comment implements Serializable, Cloneable {
  private String consoleUser = null;
  private String comment = null;

  /**
   * Constructor.
   * 
   * @param newConsoleUser the author of the comment.
   * @param newComment the comment text.
   */
  public Comment(String newConsoleUser, String newComment) {
    consoleUser = newConsoleUser;
    comment = newComment;
  }

  /**
   * Accessor method.
   * 
   * @return the author of the comment.
   */
  public String getConsoleUser() {
    return consoleUser;
  }

  /**
   * Accessor method.
   * 
   * @param newConsoleUser the author of the comment.
   */
  public void setConsoleUser(String newConsoleUser) {
    consoleUser = newConsoleUser;
  }

  /**
   * Accessor method.
   * 
   * @return the comment text.
   */
  public String getComment() {
    return comment;
  }

  /**
   * Accessor method.
   * 
   * @param newComment the comment text.
   */
  public void setComment(String newComment) {
    comment = newComment;
  }

  public String toString() {
    return new String("[" + consoleUser + "," + comment + "]");
  }

  public Object clone() {
    try {
      Comment commentClone = (Comment) super.clone();

      return commentClone;
    } catch (Exception e) {
      throw new InternalError("clone failed : " + e.getMessage());
    }
  }
}