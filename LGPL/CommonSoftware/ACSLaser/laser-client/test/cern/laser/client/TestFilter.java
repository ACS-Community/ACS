/*
 * $Id: TestFilter.java,v 1.1 2005/06/06 18:18:35 kzagar Exp $
 *
 * $Date: 2005/06/06 18:18:35 $ 
 * $Revision: 1.1 $ 
 * $Author: kzagar $
 *
 * Copyright CERN, All Rights Reserved.
 */
package cern.laser.client;
import cern.laser.client.services.selection.AlarmSelectionHandler;
import cern.laser.client.services.selection.Filter;
import cern.laser.client.services.selection.FilterSelection;

public class TestFilter 
{
  public TestFilter() throws Exception
  {
    FilterSelection filter_selection = AlarmSelectionHandler.get().createSelection().createFilterSelection();
    Filter filter_1 = Filter.create("ALARM_ID", ">", "10");
    filter_1.validate();
    filter_selection.add(filter_1);
    Filter filter_2 = Filter.create("PROBLEM_DESCRIPTION", "=", "100");
    filter_2.validate();
    filter_selection.add(filter_2);
    System.out.println(filter_selection.toSQLString());
  }

  public static void main(String[] args) throws Exception
  {
    TestFilter testFilter = new TestFilter();
  }
}