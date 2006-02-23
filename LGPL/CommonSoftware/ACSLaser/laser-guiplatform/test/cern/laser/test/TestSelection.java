package cern.laser.test;



import java.util.Collection;
import java.util.Date;
import java.util.Map;

import cern.laser.client.LaserException;
import cern.laser.client.data.Alarm;
import cern.laser.client.services.browsing.CategoryBrowsingHandler;
import cern.laser.client.services.selection.AlarmSelectionHandler;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.CategorySelection;
import cern.laser.client.services.selection.FilterSelection;
import cern.laser.client.services.selection.LaserSelectionException;
import cern.laser.client.services.selection.Selection;

public class TestSelection implements AlarmSelectionListener
{

  // this is temporary solution, I do not know yet where I should put
  // all this *.properties files
  // maybe, when everthing will be fine, when this application will work as 
  // NetBeans module    
  static {
    // cmw-mom-config.properties
    System.setProperty("cmw.mom.config", "http://slwww/~pca/cmw/cmw.cfg");


    // cmw-mom.properties")
    System.setProperty("cmw.mom.retry", "40");
    System.setProperty("cmw.mom.maxretry", "10");
    System.setProperty("cmw.mom.ping", "10");
    System.setProperty("cmw.mom.username", "laser_usr");
    System.setProperty("cmw.mom.password", "laser_pwd");
    System.setProperty("cmw.mom.brokerlist", "abjas1:2507,abjas2:2507");
    System.setProperty("cmw.mom.loadbalancing", "false");
    System.setProperty("cmw.mom.persistance", "false");
    System.setProperty("cmw.mom.timetolive", "0");
    System.setProperty("cmw.mom.priority", "4");
    System.setProperty("cmw.mom.keepalive", "0");
     

    // jndi.properties
    System.setProperty("java.naming.factory.initial", "com.evermind.server.ApplicationClientInitialContextFactory");
    //System.setProperty("java.naming.factory.initial", "com.evermind.server.rmi.RMIInitialContextFactory");
    System.setProperty("java.naming.provider.url", "ormi://abjas1/laser-core");
    //System.setProperty("java.naming.provider.url", "ormi://hpjvm/laser-core");
    System.setProperty("java.naming.security.principal", "admin");
    System.setProperty("java.naming.security.credentials", "password");
    
  }
    
  public TestSelection()
  {
      
    
      
      
      
    try {
      System.out.println("test11");
      AlarmSelectionHandler selection_handler = AlarmSelectionHandler.get();
      CategoryBrowsingHandler category_handler = CategoryBrowsingHandler.get();
      Selection selection = selection_handler.createSelection();

      CategorySelection category_selection = selection.createCategorySelection();
      Collection categories = category_handler.getCategories();
      //System.out.println(categories);
      category_selection.addAll(categories);
      selection.setCategorySelection(category_selection);

      FilterSelection filter_selection = selection.createFilterSelection();
      //filter_selection.add(Filter.REDUCED_MASKED_SET_FILTER);
      selection.setFilterSelection(filter_selection);

      System.out.println(new Date(System.currentTimeMillis()) + " : selecting...");
      Map result = selection_handler.select(selection, this);
      System.out.println(new Date(System.currentTimeMillis()) + " : selected " + result.size() + " alarms.");
      System.out.println(result.toString());
    } catch (LaserException e) 
    {
      System.err.println("stack trace : ");
      e.printStackTrace();
      System.err.println("root cause : ");
      e.getRootCause().printStackTrace();
    }
     
     
      //System.out.println(System.getProperties().toString());
  }

  public void onAlarm(Alarm alarm) 
  {
    System.out.println("received :\n" + alarm.getTriplet() + alarm.getStatus());
  }

  public void onException(LaserSelectionException e) 
  {
    System.out.println(e.getCode());
  }

  public static void main(String[] args)
  {
    TestSelection testSelection = new TestSelection();
  }
}


