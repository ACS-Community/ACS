/*
 * TestTreeExplorer.java
 *
 * Created on September 20, 2002, 5:02 PM
 */

package cern.gp.explorer.test;

import cern.gp.explorer.TreeTableExplorer;
import cern.gp.explorer.test.helpers.SimpleDemoBean;
import cern.gp.explorer.test.helpers.RecursiveChildrenListManager;
import cern.gp.nodes.NodeFactory;
import cern.gp.windows.WindowUtils;
import cern.gp.nodes.GPNode;

/**
 * A simple test driver for the TreeTableExplorer
 * @todo test all functionality of the TreeableExplorer
 * @author  Vito Baggiolini
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public class TestTreeTableExplorer {
  
  public static void main(String[] args) throws Exception {
    final SimpleDemoBean bean = new SimpleDemoBean("parent");
    GPNode root = NodeFactory.createNode(bean, new RecursiveChildrenListManager());
    TreeTableExplorer expl = new TreeTableExplorer();
    expl.setTableColumns(bean);
    expl.setRootNode(root);
    WindowUtils.openInMode(expl, "TestTreeTableExplorer");
    if (expl.getRootNode() != root) {
      System.err.println("bug:rootNode !+ getRootNode");
    }
  }
}