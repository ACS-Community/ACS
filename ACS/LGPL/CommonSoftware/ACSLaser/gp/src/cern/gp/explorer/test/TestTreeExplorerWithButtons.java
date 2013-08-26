/*
 * TestTreeExplorer.java
 *
 * Created on September 20, 2002, 5:02 PM
 */

package cern.gp.explorer.test;

import cern.gp.actions.OpenLocalExplorerAction;
import cern.gp.actions.PropertiesAction;
import cern.gp.actions.support.ActionUtils;
import cern.gp.explorer.TreeExplorer;
import cern.gp.explorer.test.helpers.SimpleDemoBean;
import cern.gp.explorer.test.helpers.RecursiveChildrenListManager;
import cern.gp.nodes.NodeFactory;
import cern.gp.windows.WindowUtils;
import java.awt.BorderLayout;
import java.beans.IntrospectionException;
import javax.swing.JPanel;
import cern.gp.nodes.GPNode;

/**
 * A simple test driver for the TreeExplorer
 * @author Vito Baggiolini
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public class TestTreeExplorerWithButtons {
  
  /** Creates a new instance of TestTreeExplorer */
  public TestTreeExplorerWithButtons() {
  }
  
  public static TreeExplorer createTreeExplorer() {
    TreeExplorer expl = null;
    try {
      GPNode root = NodeFactory.createNode(new SimpleDemoBean("parent"), new RecursiveChildrenListManager());
      expl = new TreeExplorer();
      expl.setRootNode(root);
    } catch (IntrospectionException ex) { ex.printStackTrace(); }
    return expl;
  }
  
  public static void main(String[] args) {
    TreeExplorer expl = createTreeExplorer();
    JPanel panel = ActionUtils.createJButtonPanel(new Class[] { PropertiesAction.class, OpenLocalExplorerAction.class });
    expl.add(panel, BorderLayout.SOUTH);
    WindowUtils.openInMode(expl, "TestTreeExplorer");
  }
}