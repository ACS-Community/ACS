/*
 * $Id: TestCommentedAlarm.java,v 1.1 2005/06/07 03:17:25 kzagar Exp $
 *
 * $Date: 2005/06/07 03:17:25 $ 
 * $Revision: 1.1 $ 
 * $Author: kzagar $
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