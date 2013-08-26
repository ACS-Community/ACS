/*
 * TestActionButtons.java
 *
 * Created on September 29, 2002, 7:08 PM
 */

package cern.gp.actions.test;

import cern.gp.actions.support.ActionUtils;
import cern.gp.windows.WindowUtils;
import java.awt.BorderLayout;
import java.awt.Dimension;
import javax.swing.JLabel;
import javax.swing.JPanel;
import org.openide.actions.OpenLocalExplorerAction;
import org.openide.actions.PropertiesAction;
import org.openide.windows.TopComponent;

/**
 * @author Vito Baggiolini
 * @version $Revision: 1.2 $ $Date: 2006/09/25 08:52:36 $
 */
public class TestActionButtons {
  private static int DEFAULT_WIDTH = 600;
  private static int DEFAULT_HEIGHT = 200;
  
  /** Creates a new instance of TestActionButtons */
  public TestActionButtons() {
  }
  
  public static void main(String[] args) {
    TopComponent tc = new TopComponent();
    tc.setName("TestActionButtions");
    tc.setLayout(new BorderLayout());
    tc.setPreferredSize(new Dimension(DEFAULT_WIDTH, DEFAULT_HEIGHT));

    JLabel lab = new JLabel("Actions are enabled if you select nodes in the IDE", JLabel.CENTER);
    tc.add(lab, BorderLayout.CENTER);

    JPanel panel = ActionUtils.createJButtonPanel(new Class[] { OpenLocalExplorerAction.class, PropertiesAction.class } );
    tc.add(panel, BorderLayout.SOUTH);

    WindowUtils.openInMode(tc, "TestActionButtons");
  }
}
