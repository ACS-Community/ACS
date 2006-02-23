package cern.laser.test;



import cern.laser.client.LaserException;
import cern.laser.client.services.browsing.CategoryBrowsingHandler;
import cern.laser.client.services.selection.AlarmSelectionHandler;
import cern.laser.client.services.selection.CategorySelection;
import cern.laser.client.services.selection.FilterSelection;
import cern.laser.client.services.selection.Selection;
import cern.laser.console.Behaviour;
import cern.laser.console.Configuration;
import cern.laser.console.User;
import cern.laser.console.UserHandler;

public class TestConfiguration 
{
  public TestConfiguration()
  {
    try {
        
      UserHandler handler = UserHandler.get();
      User user = handler.getUser("laser");
      Configuration configuration = user.createConfiguration("conf 1");
      
      
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
      //filter_selection.add(Filter.REDUCED_MASKED_SET_FILTER);
      selection.setFilterSelection(filter_selection);
      configuration.setSelection(selection);
      
      System.out.println("configurations : " + user.getConfigurations());
      /*
      // list all users
      java.util.Collection users = handler.getUsers();
      for (java.util.Iterator iter = users.iterator(); iter.hasNext(); ) {
          User user1 = (User) iter.next();
          System.out.println(user1.getName());
          
      }
      */
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
    TestConfiguration testConfiguration = new TestConfiguration();
  }
}

