package si.ijs.acs.objectexplorer;

import javax.swing.tree.TreeNode;
/**
 * Insert the type's description here.
 * Creation date: (3.2.2002 14:25:42)
 * @author: 
 */
public interface NodeRequestListener {
	public void addNode(TreeNode node, TreeNode parentNode);
	public void addNodes(TreeNode[] nodes, TreeNode parentNode);
	public void removeNode(TreeNode node);
	public void removeNodes(TreeNode[] nodes);
}
