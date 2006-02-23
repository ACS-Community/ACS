package cern.gp.nodes.children;

import cern.gp.nodes.GPNode;
import cern.gp.nodes.children.ChildrenMapManager;
import cern.gp.nodes.children.NodeMap;
import java.util.Comparator;

/**
 * A ChildrenMapManager class takes an array of nodes that it adds to the Map
 * when initChildrenMap is called
 * 
 * @author Lionel Mestre
 * @version $Revision: 1.1 $ $Date: 2005/06/07 03:26:13 $
 */
public class DefaultChildrenMapManager implements ChildrenMapManager {
  final private GPNode[] nodeArr;
  public DefaultChildrenMapManager(GPNode[] nodeArr) {
    this.nodeArr = nodeArr;
  }
  public void initChildrenMap(NodeMap nodeMap) {
    nodeMap.addNodes(nodeArr);
  }
  public Comparator getComparator() {
    return null;
  }
}
