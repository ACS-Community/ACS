/*
 * TestListExplorer.java
 *
 * Created on September 20, 2002, 5:02 PM
 */

package cern.gp.explorer.test;

import cern.gp.explorer.ListTableExplorer;
import cern.gp.explorer.test.helpers.ColoredBean;
import cern.gp.explorer.test.helpers.RecursiveChildrenListManager;
import cern.gp.nodes.NodeFactory;
import cern.gp.windows.WindowUtils;
import cern.gp.nodes.GPNode;

/**
 * A simple test driver for the ListTableExplorer
 * @todo test all functionality of the ListableExplorer
 * @author Vito Baggiolini
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public class TestColoredListTableExplorer {
  /**
   * method that creates and returns a ListTableExplorer
   */
  public static ListTableExplorer createListTableExplorer() {
    ListTableExplorer expl = null;
    try {
      expl = new ListTableExplorer();
      GPNode root = NodeFactory.createNode(new Object(), new RecursiveChildrenListManager(ColoredBean.class, 1, 10));
      expl.setRootNode(root);
      expl.setTableColumns(new ColoredBean(), null);
    } catch (Exception ex) { ex.printStackTrace(); }
    return expl;
  }
  
  public static void main(String[] args) {
    WindowUtils.openInMode(createListTableExplorer(), "TestListTableExplorer");    
  }
}