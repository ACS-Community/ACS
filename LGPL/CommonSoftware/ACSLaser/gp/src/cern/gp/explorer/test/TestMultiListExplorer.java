/*
 * TestListExplorer.java
 *
 * Created on September 20, 2002, 5:02 PM
 */

package cern.gp.explorer.test;

import cern.gp.explorer.MultiListExplorer;
import cern.gp.explorer.test.helpers.SimpleDemoBean;
import cern.gp.explorer.test.helpers.RecursiveChildrenListManager;
import cern.gp.nodes.NodeFactory;
import cern.gp.windows.WindowUtils;
import cern.gp.nodes.GPNode;

/**
 * A simple test driver for the MultiListExplorer
 * @author Vito Baggiolini
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public class TestMultiListExplorer {
  
  /** Creates a new instance of TestListExplorer */
  public TestMultiListExplorer() {
  }

  public static void main(String[] args) throws Exception {
    GPNode root = NodeFactory.createNode(new SimpleDemoBean("parent"), new RecursiveChildrenListManager());
    MultiListExplorer expl = new MultiListExplorer();
    expl.setRootNode(root);
    expl.setListCount(3);
    WindowUtils.openInMode(expl, "TestMultiListExplorer");
  }
  
  
}