/*
 * $Id: TestClone.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client;
import cern.laser.client.data.Alarm;
import cern.laser.client.services.browsing.AlarmBrowsingHandler;
import cern.laser.client.services.reduction.AlarmReductionHandler;

public class TestClone 
{
  public TestClone()
  {
    try {
      AlarmBrowsingHandler browsing_handler = AlarmBrowsingHandler.get();
      AlarmReductionHandler reduction_handler = AlarmReductionHandler.get();
      Alarm alarm = browsing_handler.getAlarmByTriplet("ff1", "fm1", new Integer(1));
      System.out.println(alarm);
      System.out.println(alarm.clone());
    } catch (Exception e) 
    {
      System.err.println("stack trace : ");
      e.printStackTrace();
    }
  }

  public static void main(String[] args)
  {
    TestClone testClone = new TestClone();
  }
}