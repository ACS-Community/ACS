package cern.laser.test;



import cern.gp.actions.support.NodeAction;
import cern.gp.nodes.GPNode;


/**
 * An action that displays the selected nodes in a separate ListTableExplorer 
 * 
 * @author Vito Baggiolini
 * @version $Revision: 1.1 $ $Date: 2005/06/07 04:07:03 $ $Author: kzagar $
 */
public class ShowDetailsAction extends NodeAction {
    
    private static int counter = 0;
  /**
   * the method that performs the action on the selected nodes
   * @param selectedNodes the nodes currently selected
   * @see cern.gp.actions.support.NodeAction#performAction(cern.gp.nodes.GPNode[])
   */
  protected void performAction(GPNode[] selectedNodes) {
    // the nodes need to be cloned because they need a new parent in the new ListTableExplorer.
    // A node can only have one parent...
      /*
    final GPNode[] clones = NodeUtils.cloneNodes(selectedNodes);
    ListExplorer expl = new ListExplorer(clones);
    WindowUtils.openInMode(expl, "Selected Devices");
       */
      
      System.out.println("Show Details action " + counter++);
  }
  public String getName() {
    return "&Display details"; // the '&' in front of 'd' specifies the shortcut (ALT-D)
  }
}
