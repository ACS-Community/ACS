package cern.laser.definition.impl;

import cern.laser.business.definition.data.AlarmDefinition;
import cern.laser.business.definition.data.CategoryDefinition;
import cern.laser.business.definition.data.CategoryLink;
import cern.laser.definition.AdminUser;
import cern.laser.definition.AdminUserHandler;
import cern.laser.definition.LaserDefinitionException;

public class TestRemoveAlarm {
  public TestRemoveAlarm() {
    try {
      AdminUserHandler handler = AdminUserHandler.get();
      System.out.println("logging...");
      AdminUser admin = handler.loginUser("test-admin-1", "test-admin-1");
      System.out.println("removing...");
      admin.getCategoryLinkDefinitionHandler().removeCategoryLink(
          new CategoryLink(new CategoryDefinition("CERN.test-1.test-cat-1"), new AlarmDefinition("test-ff-1",
              "test-fm-1", new Integer(11))));
//      admin.getAlarmDefinitionHandler().removeAlarm(new AlarmDefinition("ff1", "fm1", new Integer(1001)));
      System.out.println("removed");

    } catch (LaserDefinitionException le) {
      le.printStackTrace();
      le.getCause().printStackTrace();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  public static void main(String[] args) {
    TestRemoveAlarm testRemoveAlarm = new TestRemoveAlarm();
  }
}