package cern.laser.test;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.beans.IntrospectionException;
import java.util.Arrays;

import javax.swing.Action;
import javax.swing.JButton;
import javax.swing.JLabel;

import org.openide.explorer.ExplorerPanel;
import org.openide.explorer.view.ListView;
import org.openide.nodes.BeanNode;
import org.openide.nodes.Children;
import org.openide.nodes.Node;
import org.openide.util.ContextAwareAction;
import org.openide.util.HelpCtx;
import org.openide.util.actions.NodeAction;
import org.openide.util.actions.SystemAction;
import org.openide.windows.TopComponent;

/** 
 * An example that shows a possible bug: A JButton connected to an action 
 * (via a ButtonBridge) is not correctly activated.
 * 
 * To see this behavior, execute the main() method. It creates 
 * two Explorers, one of which is directly opened, and the other 
 * is first inserted into a TopComponent and then opened.
 * Click on the node: 
 * The JButton works in the first explorer, but not in the 
 * second one.
 */
public class ExplorerIssue extends ExplorerPanel {

  public ExplorerIssue(Node rootNode) {
    super();
    add(new ListView());
    //add(new BeanTreeView());
    getExplorerManager().setRootContext(rootNode);
  }

  public static void main(String[] args) throws IntrospectionException {
    // create two identical explorerpanels
    ExplorerPanel expl1 = new ExplorerIssue(createNodeStructure("bean1"));
    ExplorerPanel expl2 = new ExplorerIssue(createNodeStructure("bean2"));
    expl1.setPreferredSize(new Dimension(300, 200));
    expl2.setPreferredSize(new Dimension(300, 200));

    //  open expl1 individually
    expl1.add(new JLabel("OK: JButton activated"), BorderLayout.NORTH);
    expl1.add(new JButton(actionForComp(expl1)), BorderLayout.SOUTH);
    expl1.open();

    // open expl2 inside an additional TopComponent
    TopComponent tc = new TopComponent();
    tc.setLayout(new BorderLayout());
    tc.add(expl2, BorderLayout.CENTER);
    tc.add(new JLabel("OK #2: JButton now activated"), BorderLayout.NORTH);
    tc.add(new JButton(actionForComp(expl2)), BorderLayout.SOUTH);
    tc.open();
  }
  
  private static Action actionForComp(TopComponent c) {
      Action a = SystemAction.get(MyNodeAction.class);
      return ((ContextAwareAction)a).createContextAwareInstance(c.getLookup());
  }

  /** helper method, creates a BeanNode with children */
  private static Node createNodeStructure(String name)
    throws IntrospectionException {
    Children ch = new Children.Array();
    ch.add(new Node[] { new MyBeanNode(new MyBean("child"), Children.LEAF)});
    return new MyBeanNode(new MyBean(name), ch);
  }

  /** a bean object to be used in MyBeanNode */
  private static class MyBean {
    private final String name;
    public MyBean(String name) {
      this.name = name;
    }
    public String getDisplayName() {
      return name;
    }
  }

  /** a bean node that provides the MyNodeAction in the Popup menu */
  private static class MyBeanNode extends BeanNode {
    public MyBeanNode(Object bean) throws IntrospectionException {
      super(bean);
    }
    public MyBeanNode(MyBean bean, Children children)
      throws IntrospectionException {
      super(bean, children);
    }
    public SystemAction[] createActions() {
      return new SystemAction[] { SystemAction.get(MyNodeAction.class)};
    }
    public String getName() {
      return "Click on me to activate the JButton";
    }
  }

  /** a NodeAction that is enabled only when at least one node is selected */
  private static class MyNodeAction extends NodeAction {
    protected void performAction(Node[] activatedNodes) {
      System.out.println("MyNodeAction called: " + Arrays.asList(activatedNodes));
    
      /*
      try {
        activatedNodes[0].destroy();
      } catch (java.io.IOException ie) {
          ie.printStackTrace();
      }
       */
      //if ( activatedNodes[0].getParentNode() != null )
      //    activatedNodes[0].getParentNode().getChildren().remove(activatedNodes);
    }
    protected boolean enable(Node[] activatedNodes) {
      System.out.println("enabled: " + Arrays.asList(activatedNodes));
      
      return (activatedNodes.length > 0);
    }
    public String getName() {
      return "MyNodeAction";
    }
    public HelpCtx getHelpCtx() {
      return null;
    }
    public boolean surviveFocusChange() {
      return false;
    }
  }
  
  //private static class MyAddNodeAction
}
