/*
 * $Id: TestSelection.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client;

import java.util.Date;
import java.util.Map;

import org.apache.log4j.Logger;

import cern.laser.client.data.Alarm;
import cern.laser.client.data.Category;
import cern.laser.client.services.browsing.CategoryBrowsingHandler;
import cern.laser.client.services.selection.AlarmSelectionHandler;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.CategorySelection;
import cern.laser.client.services.selection.LaserSelectionException;
import cern.laser.client.services.selection.Selection;

public class TestSelection implements AlarmSelectionListener {

  private static final Logger LOGGER = Logger.getLogger(TestSelection.class.getName());

  public TestSelection() {
    AlarmSelectionHandler selection_handler = null;
    try {
      selection_handler = AlarmSelectionHandler.get();
      CategoryBrowsingHandler category_handler = CategoryBrowsingHandler.get();
      Selection selection = selection_handler.createSelection();

      CategorySelection category_selection = selection.createCategorySelection();
//      Category category = category_handler.getCategoryByPath("CERN.SOURCES.test-src-1");
//      Category category = category_handler.getCategoryByPath("CERN.REDUCTION.MULTIPLICITY");
      Category category = category_handler.getCategoryByPath("CERN.OLD_TREE.UNUSED1");
      category_selection.add(category);
      selection.setCategorySelection(category_selection);
      System.out.println(new Date(System.currentTimeMillis()) + " : selecting...");
      Map result = selection_handler.select(selection, this);
      System.out.println(new Date(System.currentTimeMillis()) + " : selected " + result.size() + " alarms.");
      while(true);
    } catch (LaserException e) {
      System.err.println("stack trace : ");
      e.printStackTrace();
      System.err.println("root cause : ");
      e.getCause().printStackTrace();

    } catch (Exception e) {
      System.err.println("got exception : " + e.getMessage());
      e.printStackTrace();

    }
  }

  public void onAlarm(Alarm alarm) {
    LOGGER.debug("=> " + new Date(System.currentTimeMillis()) + alarm.getTriplet());
  }

  public void onException(LaserSelectionException e) {
    LOGGER.error("*********onException************\n" + e.getCode());
  }

  public static void main(String[] args) {
    TestSelection testSelection = new TestSelection();
  }
}