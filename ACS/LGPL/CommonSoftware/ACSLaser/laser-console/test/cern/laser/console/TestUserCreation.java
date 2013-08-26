/*
 * $Id: TestUserCreation.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.console;
import cern.laser.client.LaserException;
import cern.laser.console.User;
import cern.laser.console.UserHandler;

public class TestUserCreation 
{
  public TestUserCreation()
  {
    try {
      UserHandler handler = UserHandler.get();
      User user = handler.createUser("niall", "password");
      System.out.println("done");
    } catch (LaserException e) 
    {
      System.err.println("stack trace : ");
      e.printStackTrace();
      System.err.println("root cause : ");
      e.getCause().printStackTrace();
    }
  }

  public static void main(String[] args)
  {
    TestUserCreation testUserCreation = new TestUserCreation();
  }
}