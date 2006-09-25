/*
 * $Id: TestCommentedAlarm.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.console;
import cern.laser.console.Comment;
import cern.laser.console.CommentedAlarm;

public class TestCommentedAlarm 
{
  public TestCommentedAlarm() throws Exception
  {
    CommentedAlarm ca = new CommentedAlarm(null, new Comment("pippo", "pluto"));
    System.out.println("clone : " + ca.clone());
  }

  public static void main(String[] args) throws Exception
  {
    TestCommentedAlarm testCommentedAlarm = new TestCommentedAlarm();
  }
}