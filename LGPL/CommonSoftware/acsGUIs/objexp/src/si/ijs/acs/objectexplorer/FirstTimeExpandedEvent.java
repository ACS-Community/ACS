package si.ijs.acs.objectexplorer;

/**
 * Event is fired by OETree when an OETreeNode is expanded for the first time. 
 *
 * Creation date: (9/26/98 11:22:38 AM)
 * @author: Miha Kadunc
 */
public class FirstTimeExpandedEvent extends java.util.EventObject {
	OETreeNode treeNode = null;
/**
 * Insert the method's description here.
 * Creation date: (9/26/98 11:24:04 AM)
 */
public FirstTimeExpandedEvent(Object source) {
  super(source);
}
/**
 * Insert the method's description here.
 * Creation date: (9/30/98 5:54:22 PM)
 * @return TreePath
 */
public OETreeNode getTreeNode() {
	return treeNode;
}
/**
 * Insert the method's description here.
 * Creation date: (9/30/98 5:54:22 PM)
 * @param newTreePath TreePath
 */
public void setTreeNode(OETreeNode newTreeNode) {
	treeNode = newTreeNode;
}
}
