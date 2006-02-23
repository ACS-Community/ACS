/*
 * $Id: TestReduction.java,v 1.1 2005/06/06 18:18:35 kzagar Exp $
 *
 * $Date: 2005/06/06 18:18:35 $ 
 * $Revision: 1.1 $ 
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client;
import cern.laser.client.LaserException;
import cern.laser.client.data.Alarm;
import cern.laser.client.services.browsing.AlarmBrowsingHandler;
import cern.laser.client.services.reduction.AlarmReductionHandler;

public class TestReduction 
{
  public TestReduction()
  {
    try {
      AlarmBrowsingHandler browsing_handler = AlarmBrowsingHandler.get();
      AlarmReductionHandler reduction_handler = AlarmReductionHandler.get();
      Alarm alarm = browsing_handler.getAlarmByTriplet("ff1", "fm1", new Integer(10));
      System.out.println(alarm.getAlarmId() + " => node children:\n" + reduction_handler.getNodeChildren(alarm));
    } catch (Exception e) 
    {
      System.err.println("stack trace : ");
      e.printStackTrace();
      System.err.println("root cause : ");
      e.getCause().printStackTrace();
    }
  }

  public static void main(String[] args)
  {
    TestReduction testReduction = new TestReduction();
  }
}