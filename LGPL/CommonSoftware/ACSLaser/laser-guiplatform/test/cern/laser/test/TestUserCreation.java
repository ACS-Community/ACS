package cern.laser.test;


import cern.laser.client.LaserException;
import cern.laser.console.User;
import cern.laser.console.UserHandler;

public class TestUserCreation 
{
  public TestUserCreation()
  {
    try {
      UserHandler handler = UserHandler.get();
      User user = handler.createUser("GUEST", "guest");
      System.out.println("done");
    } catch (LaserException e) 
    {
      System.err.println("stack trace : ");
      e.printStackTrace();
      System.err.println("root cause : ");
      e.getRootCause().printStackTrace();
    }
  }

  public static void main(String[] args)
  {
    TestUserCreation testUserCreation = new TestUserCreation();
  }
}


