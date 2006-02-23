/*
 * $Id: TestAlarmBrowsing.java,v 1.1 2005/06/06 18:18:35 kzagar Exp $
 *
 * $Date: 2005/06/06 18:18:35 $ 
 * $Revision: 1.1 $ 
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client;

import cern.laser.client.data.Alarm;
import cern.laser.client.services.browsing.AlarmBrowsingHandler;

/**
 * DOCUMENT ME!
 * 
 * @author $author$
 * @version $Revision: 1.1 $
 */
public class TestAlarmBrowsing {
  /**
   * Creates a new TestCategoryBrowsing object.
   */
  public TestAlarmBrowsing() {
    try {
      System.out.println("Browsing alarms");
      AlarmBrowsingHandler handler = AlarmBrowsingHandler.get();
        Alarm alarm = handler.getAlarmByTriplet("test-mult-ff", "test-mult-parent", new Integer(1));
        System.out.println("Alarm :\n" + alarm);
    } catch (Exception e) {
      System.err.println("stack trace : ");
      e.printStackTrace();
      System.err.println("root cause : ");
      e.getCause().printStackTrace();
    }
  }

  /**
   * DOCUMENT ME!
   * 
   * @param args DOCUMENT ME!
   */
  public static void main(String[] args) {
    TestAlarmBrowsing testCategoryBrowsing = new TestAlarmBrowsing();
  }
}