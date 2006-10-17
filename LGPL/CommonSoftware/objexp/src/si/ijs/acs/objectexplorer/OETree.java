package si.ijs.acs.objectexplorer;


import javax.swing.*;
import javax.swing.tree.*;
/**
 * Insert the type's description here.
 * Creation date: (9/26/98 1:24:30 PM)
 * @author: Miha Kadunc
 */
public class OETree extends JTree {
	// ----- integer constants, prepresenting hierarchy types -----
	public static final int HIERARCHY_BY_TYPE = 1;
	public static final int	HIERARCHY_BY_NAME = 2;
	// ----- * by rbertoncelj
	
	protected transient java.util.Vector aOETreeEventListener = null;
	private NotificationBean notifier=null;
/**
 * ComponentTree constructor comment.
 */
public OETree() {
	super();
	setShowsRootHandles(true);
	setRootVisible(false);
	setCellRenderer(new OETreeCellRenderer());
	setModel(new DefaultTreeModel(new OETreeNode(-1,"Objects","root",this)));
	getSelectionModel().setSelectionMode(DefaultTreeSelectionModel.SINGLE_TREE_SELECTION);

}
/**
 * Add a si.ijs.anka.utilities.serviceLocator.ServiceLocatorEventListener.
 */
public void addOETreeEventListener(OETreeEventListener newListener) {
	if (aOETreeEventListener == null) {
		aOETreeEventListener = new java.util.Vector();
	};
	aOETreeEventListener.addElement(newListener);
}
/**
 * Insert the method's description here.
 * Creation date: (9/27/98 6:38:56 PM)
 */
public void clearTree() {
  OETreeNode rootnode=new OETreeNode(0,"Objects","root",this);
  setModel(new DefaultTreeModel(rootnode));
  setSelectionRow(0);
}
/**
 * Method to support listener events.
 */
protected void fireFirstTimeExpanded(FirstTimeExpandedEvent event) {
	if (aOETreeEventListener == null) {
		return;
	};
	int currentSize = aOETreeEventListener.size();
	OETreeEventListener tempListener = null;
	for (int index = 0; index < currentSize; index++){
		tempListener = (OETreeEventListener)aOETreeEventListener.elementAt(index);
		if (tempListener != null) {
			tempListener.firstTimeExpanded(event);
		};
	};
}
/**
 * Insert the method's description here.
 * Creation date: (11/10/00 3:55:37 PM)
 * @return si.ijs.acs.objectexplorer.NotificationBean
 */
public NotificationBean getNotifier() {
	return notifier;
}
/**
 * Remove a si.ijs.anka.utilities.serviceLocator.ServiceLocatorEventListener.
 */
public void removeOETreeEventListener(OETreeEventListener newListener) {
	if (aOETreeEventListener != null) {
		aOETreeEventListener.removeElement(newListener);
	};
}
/**
 * Insert the method's description here.
 * Creation date: (11/10/00 3:55:37 PM)
 * @param newNotifier si.ijs.acs.objectexplorer.NotificationBean
 */
public void setNotifier(NotificationBean newNotifier) {
	notifier = newNotifier;
}
}
