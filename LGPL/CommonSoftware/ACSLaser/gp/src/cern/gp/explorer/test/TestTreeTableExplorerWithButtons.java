/*
 * TestTreeExplorer.java
 *
 * Created on September 20, 2002, 5:02 PM
 */

package cern.gp.explorer.test;

import cern.gp.actions.OpenLocalExplorerAction;
import cern.gp.actions.PropertiesAction;
import cern.gp.actions.support.ActionUtils;
import cern.gp.explorer.TreeTableExplorer;
import cern.gp.explorer.test.helpers.SimpleDemoBean;
import cern.gp.explorer.test.helpers.RecursiveChildrenListManager;
import cern.gp.nodes.NodeFactory;
import cern.gp.windows.WindowUtils;
import java.awt.BorderLayout;
import java.beans.IntrospectionException;
import javax.swing.JPanel;
import cern.gp.nodes.GPNode;

/**
 * A simple test driver for the TreeTableExplorer that shows how to use an 
 * Action-sensitive Button 
 * 
 * @author  Vito Baggiolini
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public class TestTreeTableExplorerWithButtons {

  public static TreeTableExplorer createExplorer() {
    TreeTableExplorer expl = null;
    try {
      final SimpleDemoBean bean = new SimpleDemoBean("parent");
      GPNode root = NodeFactory.createNode(bean, new RecursiveChildrenListManager());
      expl = new TreeTableExplorer();
      expl.setTableColumns(bean);
      expl.setRootNode(root);
    } catch (IntrospectionException ex) { ex.printStackTrace(); }
    return expl;
  }
  
  
  public static void main(String[] args) throws Exception {
    TreeTableExplorer expl = createExplorer();
    JPanel panel = ActionUtils.createJButtonPanel(new Class[] { PropertiesAction.class, OpenLocalExplorerAction.class });
    expl.add(panel, BorderLayout.SOUTH);
    WindowUtils.openInMode(expl, "TestTreeTableExplorer");
    expl.requestFocus();
  }
  
  
}