package cern.laser.definition.impl;

import java.util.ArrayList;
import java.util.Collection;

import cern.laser.business.definition.data.AlarmDefinition;
import cern.laser.definition.AdminUser;
import cern.laser.definition.AdminUserHandler;
import cern.laser.definition.LaserDefinitionException;


/**
 * DOCUMENT ME!
 *
 * @author $author$
 * @version $Revision: 1.2 $
 */
public class TestAdminUser {
  /**
   * Creates a new TestAdminUser object.
   */
  public TestAdminUser() {
    try {
      AdminUserHandler handler = AdminUserHandler.get();

      System.out.println("logging...");

      AdminUser admin = handler.loginUser("francesco", "password");

      /*
      System.out.println("creating user...");
      AdminUser admin = handler.createUser("francesco","password");


      System.out.println("creating categories...");
      admin.getCategoryDefinitionHandler().createCategory(new CategoryDefinition("CERN.TEST CATEGORY ROOT", "This is a test category"));
      Collection categories = new ArrayList();
      for (int i=1; i<=100; i++)
      {
        categories.add(new CategoryDefinition("CERN.TEST CATEGORY ROOT.TEST CATEGORY " + i, "This is a test category"));
      }
      admin.getCategoryDefinitionHandler().createCategories(categories);;

      System.out.println("creating source...");
      admin.getSourceDefinitionHandler().createSource(new SourceDefinition("TEST", "Test source", new Integer(30000), new Integer(49821)));
      //admin.removeSource(new Integer(100));

      System.out.println("creating sources...");
      Collection sources = new ArrayList();
      for (int i=1; i<=100; i++)
      {
        sources.add(new SourceDefinition("TEST" + i, "Test source", new Integer(300000), new Integer(49821)));
      }
      admin.getSourceDefinitionHandler().createSources(sources);

      System.out.println("removing sources...");
      for (int i=101; i<=200; i++)
      {
        admin.getSourceDefinitionHandler().removeSource(new Integer(i));;
      }
      */
      /*
      System.out.println("creating alarms...");
      Collection alarms = null;
      for (int i=1; i<=10; i++) {
        alarms = new ArrayList();
        for (int j=1; j<=100; j++) {
          alarms.add(new AlarmDefinition("ff1", "fm1", new Integer(1000 + ((i-1)*100) + j), "System name", "Identifier", "Problem description", new Integer(1), "cause", "action", "consequence", Boolean.FALSE, "http://www.cern.ch", "TEST", "936", "R", "006", null, null, new Integer(49821), "4384", Collections.singleton(new String("CERN.TEST CATEGORY ROOT.TEST CATEGORY " + i))));
        }
        admin.getAlarmDefinitionHandler().upload(alarms, null, null);
        System.out.println("bunch nr " + i + " created");
      }
      */
      System.out.println("removing alarms...");

      Collection alarms = new ArrayList();

      for (int i = 1; i <= 10; i++) {
        for (int j = 1; j <= 100; j++) {
          AlarmDefinition alarm = new AlarmDefinition("ff1", "fm1", new Integer(1000 + ((i - 1) * 100) + j));
          alarms.add(alarm);

          //admin.getAlarmDefinitionHandler().removeAlarm(new Triplet("ff1", "fm1", new Integer(1000 + ((i-1)*100) + j)));
        }

        //System.out.println("bunch " + i + " removed");
      }

      admin.getAlarmDefinitionHandler().upload(null, null, alarms);
    } catch (LaserDefinitionException lde) {
      lde.printStackTrace();
      lde.getCause().printStackTrace();
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  /**
   * DOCUMENT ME!
   *
   * @param args DOCUMENT ME!
   */
  public static void main(String[] args) {
    TestAdminUser testAdminUser = new TestAdminUser();
  }
}
