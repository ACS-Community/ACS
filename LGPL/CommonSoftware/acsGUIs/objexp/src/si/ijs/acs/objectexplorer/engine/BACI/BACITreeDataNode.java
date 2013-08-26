package si.ijs.acs.objectexplorer.engine.BACI;

import java.util.ArrayList;
import si.ijs.acs.objectexplorer.OETree;



/**
 * Insert the type's description here.
 * Creation date: (1.11.2000 12:35:07)
 * @author: 
 */
public class BACITreeDataNode extends si.ijs.acs.objectexplorer.OETreeNode {
	ArrayList childrenHolder = null;
	String domainRemainder = null;
/**
 * OETreeDataNode constructor comment.
 * @param newType int
 * @param newObjectType java.lang.String
 * @param newObject java.lang.Object
 * @param newParentTree si.ijs.acs.objectexplorer.OETree
 */
public BACITreeDataNode(int newType, String name, Object data, OETree newParentTree, javax.swing.Icon icon) {
	super(newType, name, data, newParentTree, icon);
}
}
