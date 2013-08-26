package si.ijs.acs.objectexplorer;

import javax.swing.*;
import java.util.ArrayList;
import javax.swing.tree.*;
/**
 * Insert the type's description here.
 * Creation date: (9/26/98 1:32:06 PM)
 * @author: Miha Kadunc
 *
 * 09.05.2001  added setChildrenDefined(boolean areChildrenDefined)
 */
public class OETreeNode extends DefaultMutableTreeNode implements Comparable {
  private boolean areChildrenDefined = true;
  private int nodeType = 0;
  private OETree parentTree = null;
  private String name=null;
  public ArrayList childrenHolder = null;
  private Icon icon=null;
  private boolean introspectable=false;
/**
 * Insert the method's description here.
 * Creation date: (9/26/98 9:17:18 PM)
 * @param newLevel int
 * @param newObject java.lang.Object
 */
public OETreeNode(int newType, String newName, Object newObject, OETree newParentTree) {
  super(newObject);
  introspectable= (this instanceof si.ijs.acs.objectexplorer.engine.Introspectable);
  nodeType = newType;
  parentTree=newParentTree;
  name=newName;
  areChildrenDefined=false;
}
/**
 * Insert the method's description here.
 * Creation date: (9/26/98 9:17:18 PM)
 * @param newLevel int
 * @param newObject java.lang.Object
 */
public OETreeNode(int newType, String newName, Object newObject, OETree newParentTree, Icon icon) {
  super(newObject);
  this.icon=icon;
  introspectable= (this instanceof si.ijs.acs.objectexplorer.engine.Introspectable);
  nodeType = newType;
  parentTree=newParentTree;
  name=newName;
  areChildrenDefined=false;
}
/**
 * Insert the method's description here.
 * Creation date: (11/7/00 5:58:20 PM)
 */
public OETreeNode(short nodeType, String name, Object data, OETree parentTree, ArrayList children, Icon icon) {
	this(nodeType, name, data, parentTree);
	this.icon=icon;
	childrenHolder = children;
}
/**
 * Insert the method's description here.
 * Creation date: (9.5.2001 17:19:10)
 */
public boolean areChildrenDefined() {
  return areChildrenDefined;	
}
/**
 * compareTo method comment.
 */
public int compareTo(java.lang.Object o) {
	return(toString().compareToIgnoreCase(o.toString())); //2010.02.05 panta@naoj
}
/**
 * Insert the method's description here.
 * Creation date: (9/28/98 12:44:36 PM)
 */
public int getChildCount() {
	if (!areChildrenDefined) {
		areChildrenDefined=true;
		FirstTimeExpandedEvent event = new FirstTimeExpandedEvent(parentTree);
		event.setTreeNode(this);
		parentTree.fireFirstTimeExpanded(event);
  	}
	return super.getChildCount();
}
/**
 * Insert the method's description here.
 * Creation date: (3/27/2001 7:20:16 PM)
 * @return javax.swing.Icon
 */
public Icon getIcon() {
	return icon;
}
/**
 * Insert the method's description here.
 * Creation date: (11/7/00 5:57:34 PM)
 */
public String getName() {
	return name;
}
/**
 * Insert the method's description here.
 * Creation date: (9/28/98 5:53:54 PM)
 * @return int
 */
public int getNodeType() {
	return nodeType;
}
/**
 * Insert the method's description here.
 * Creation date: (11/10/00 3:58:25 PM)
 * @return si.ijs.acs.objectexplorer.OETree
 */
public OETree getParentTree() {
	return parentTree;
}
/**
 * Insert the method's description here.
 * Creation date: (2.2.2002 17:12:58)
 */
public boolean isIntrospectable() {
	return introspectable;	
}
/**
 * Insert the method's description here.
 * Creation date: (9/26/98 1:45:06 PM)
 */

  public boolean isLeaf() {
   if (this instanceof si.ijs.acs.objectexplorer.engine.Invocation) return true;
   else return false;
  }
/**
 * Insert the method's description here.
 * Creation date: (9.5.2001 17:19:10)
 */
public synchronized void setChildrenDefined(boolean c) {
  areChildrenDefined=c;	
}
/**
 * Insert the method's description here.
 * Creation date: (11/10/00 3:58:25 PM)
 * @param newParentTree si.ijs.acs.objectexplorer.OETree
 */
public void setParentTree(OETree newParentTree) {
	parentTree = newParentTree;
}
/**
 * Insert the method's description here.
 * Creation date: (11/8/00 3:00:49 PM)
 */
public String toString() {
  return(name);
}
}
