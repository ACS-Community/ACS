/*
 * TestConfiguration2.java
 *
 * Created on November 18, 2004, 9:55 AM
 */

package cern.laser.test;

import cern.laser.client.services.browsing.AlarmBrowsingHandler;
import cern.laser.client.services.browsing.CategoryBrowsingHandler;
import cern.laser.client.services.selection.AlarmSelectionHandler;
import cern.laser.client.services.selection.CategorySelection;
import cern.laser.client.services.selection.FilterSelection;
import cern.laser.client.services.selection.Selection;
import cern.laser.console.Behaviour;
import cern.laser.console.Comment;
import cern.laser.console.CommentedAlarm;
import cern.laser.console.CommentedAlarmMap;
import cern.laser.console.Configuration;
import cern.laser.console.LaserConfigurationNotFoundException;
import cern.laser.console.User;
import cern.laser.console.UserHandler;

public class TstConfiguration2
{
    public TstConfiguration2()
  {
    try {
      UserHandler handler = UserHandler.get();
      User user = handler.getUser("laser");
      Configuration configuration = null;
      try {
        configuration = user.getConfiguration("TEST_CONF_1");
      } catch (LaserConfigurationNotFoundException nfe) {
        System.out.println("TEST_CONF_1 not found, creating...");
        configuration = user.createConfiguration("TEST_CONF_1");
      }

      Behaviour behaviour = configuration.createBehaviour();
      behaviour.setAlarmAutoKlaxon(true);
      behaviour.setAlarmAutoTerminated(true);
      behaviour.setAlarmDistinguished(true);
      behaviour.setDailyPrinter("936-LJ");
      behaviour.setDailyPrinting(true);
      behaviour.setKlaxonVolume(Behaviour.HIGH_BELL_VOLUME);
      String[] columns = {"alarmId", "problemDescription"};
      behaviour.setColumnsToDisplay(columns);
      configuration.setBehaviour(behaviour);

      AlarmSelectionHandler selection_handler = AlarmSelectionHandler.get();
      Selection selection = selection_handler.createSelection();
      CategorySelection category_selection = selection.createCategorySelection();
      CategoryBrowsingHandler category_handler = CategoryBrowsingHandler.get();
      category_selection.addAll(category_handler.getCategories());
      selection.setCategorySelection(category_selection);
      FilterSelection filter_selection = selection.createFilterSelection();
      selection.setFilterSelection(filter_selection);
      selection.setReducedMaskedSelection(true);
      configuration.setSelection(selection);

      CommentedAlarmMap alarms = new CommentedAlarmMap();
      alarms.put(new CommentedAlarm(AlarmBrowsingHandler.get().getAlarmByTriplet("TEST_FAULT_FAMILY", "HAIFA", new Integer(1)), new Comment("pippo", "pluto")));
      configuration.setInhibited(alarms);
      configuration.setMasked(alarms);
      configuration.setHighlighted(alarms);

      System.out.println("inhibited :\n" + configuration.getInhibited());
      System.out.println("masked :\n" + configuration.getMasked());
      System.out.println("highlighted :\n" + configuration.getHighlighted());

      System.out.println("configurations : " + user.getConfigurations());
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
    TstConfiguration2 testConfiguration = new TstConfiguration2();
  }
}
