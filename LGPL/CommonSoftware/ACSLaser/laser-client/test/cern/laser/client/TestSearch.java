/*
 * $Id: TestSearch.java,v 1.2 2006/09/25 08:52:36 acaproni Exp $
 *
 * $Date: 2006/09/25 08:52:36 $ 
 * $Revision: 1.2 $ 
 * $Author: acaproni $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client;

import java.util.Date;

import org.apache.log4j.Logger;

import cern.laser.client.data.Alarm;
import cern.laser.client.data.Category;
import cern.laser.client.impl.services.selection.FilterImpl;
import cern.laser.client.services.browsing.CategoryBrowsingHandler;
import cern.laser.client.services.selection.AlarmSearchListener;
import cern.laser.client.services.selection.AlarmSelectionHandler;
import cern.laser.client.services.selection.CategorySelection;
import cern.laser.client.services.selection.Filter;
import cern.laser.client.services.selection.FilterSelection;
import cern.laser.client.services.selection.LaserSearchException;
import cern.laser.client.services.selection.Selection;

public class TestSearch implements AlarmSearchListener {

  private static final Logger LOGGER = Logger.getLogger(TestSearch.class.getName());

  public TestSearch() {
    AlarmSelectionHandler selection_handler = null;
    try {
      selection_handler = AlarmSelectionHandler.get();
      CategoryBrowsingHandler category_handler = CategoryBrowsingHandler.get();
      Selection selection = selection_handler.createSelection();

      CategorySelection category_selection = selection.createCategorySelection();
      Category category = category_handler.getCategoryByPath("CERN.SOURCES.AITB");
      category_selection.add(category);
      selection.setCategorySelection(category_selection);
      
      FilterSelection filter_selection = selection.createFilterSelection();
      Filter filter = new FilterImpl();
      filter.setProperty(FilterImpl.properties()[1]);
      filter.setOperator(FilterImpl.operators()[6]);
      filter.setValue("POWER%");      
      filter_selection.add(filter);
      selection.setFilterSelection(filter_selection);
      
      System.out.println(new Date(System.currentTimeMillis()) + " : searching...");
      selection_handler.search(selection, 10, this);
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

  public void onSearchAlarm(Alarm alarm) {
    LOGGER.debug("=> " + new Date(System.currentTimeMillis()) + alarm);
  }

  public static void main(String[] args) {
    TestSearch testSelection = new TestSearch();
  }

  /* (non-Javadoc)
   * @see cern.laser.client.services.selection.AlarmSearchListener#onSearchException(cern.laser.client.services.selection.LaserSearchException)
   */
  public void onSearchException(LaserSearchException e) {
    LOGGER.debug("*********onException************\n" + e.getCode());
  }

  /* (non-Javadoc)
   * @see cern.laser.client.services.selection.AlarmSearchListener#isSearchCancelled()
   */
  public boolean isSearchCancelled() {
    return false;
  }

  /* (non-Javadoc)
   * @see cern.laser.client.services.selection.AlarmSearchListener#searchFinished()
   */
  public void searchFinished() {
    LOGGER.debug("search finished");
  }
}