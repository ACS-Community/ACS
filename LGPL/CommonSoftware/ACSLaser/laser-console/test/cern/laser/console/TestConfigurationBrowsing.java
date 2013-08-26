/*
 * $Id: TestConfigurationBrowsing.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.console;

import java.util.Collection;
import java.util.Iterator;

import cern.laser.console.Configuration;
import cern.laser.console.User;
import cern.laser.console.UserHandler;

public class TestConfigurationBrowsing {
  public TestConfigurationBrowsing() {
    try {
      UserHandler handler = UserHandler.get();
      Collection users = handler.getUsers();
      Iterator iterator = users.iterator();
      while (iterator.hasNext()) {
        User user = (User) iterator.next();
        System.out.println(user);
        Collection configurations = user.getConfigurations();
        Iterator conf_iterator = configurations.iterator();
        while (conf_iterator.hasNext()) {
          Configuration configuration = (Configuration) conf_iterator.next();
          System.out.println(configuration);
        }
      }
    } catch (Exception e) {
      System.err.println("stack trace : ");
      e.printStackTrace();
      System.err.println("root cause : ");
      e.getCause().printStackTrace();
    }
  }

  public static void main(String[] args) {
    TestConfigurationBrowsing testConfigurationBrowsing = new TestConfigurationBrowsing();
  }
}