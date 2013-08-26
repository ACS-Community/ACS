package si.ijs.acs.objectexplorer;

import javax.swing.* ;
import si.ijs.acs.objectexplorer.engine.*;
/**
 * Handles Attributes and Operations of the selected Object
 *
 * @author: Miha Kadunc
 *
 * 09.05.2001  Fixed exception handling, removed unnecessary members
 */

public class ListsHandlerBean implements OperationInvocator, ObjectDetailsHandler, RemoteResponseCallbackListener{
	NotificationBean notifier = null;
  	NodeRequestListener nodeRequestListener=null;
	ReporterBean reporter =null;
	
	Attribute[] cacheAttributes=null;
	Operation[] cacheOperations=null;

	SimpleIntrospectable currentNode=null;
	
	boolean showSpecial=true;
	boolean isSearching=false;

	private ListsSimpleIntrospectableDetails panel=null;

/**
 * This type is a thread that handles the execution of an Operation on
 * an object and returning the RemoteCall, which includes the return value.
 *
 * Creation date: (9/26/98 10:40:53 AM)
 * @author: Miha Kadunc
 */
 
private class OperationAcquiry extends Thread{
  private ListsHandlerBean parent=null;
  private ReporterBean reporter=null;
  private NotificationBean notifier=null;
  private Object[] params=null;
  private Object ob=null;
/**
 * ComponentNamesSearching constructor comment.
 */
private OperationAcquiry(ListsHandlerBean in_parent, ReporterBean reporter, Object ob, Object[] params) {
	super();
	this.parent=in_parent;
	this.reporter=reporter;
	this.notifier=parent.notifier;
	this.params=params;
	this.ob=ob;
}
	/**
 * Insert the method's description here.
 * Creation date: (9/26/98 11:11:15 AM)
 */
public void run() {
  try{
	 if (ob instanceof Operation) {
		Operation op=(Operation)ob;
		if (op.isInvocation()){
	 	  notifier.reportDebug("OperationAcquiry::run","Invocation " +op);
	 	  OERemoteResponseCallback cb = new OERemoteResponseCallback(parent, ReporterBean.raID/*parent.reporter.raID*/);
		  Invocation node=op.invokeAsync(params, cb);
		  if (node!=null) {
		    cb.setInvocation(node);
		    reporter.reportRemoteCall(node.getInvocationRequest());
		    if (op.getIntrospectable() instanceof OETreeNode && node.isControllable()) {
			   OETreeNode parentNode=(OETreeNode)op.getIntrospectable();
			   parent.nodeRequestListener.addNode(node,parentNode);
			   //TODO update for secondary tree!!!
		    }
		  }
		}
		else {
 		   notifier.reportDebug("OperationAcquiry::run","Operation " + op + " invoked");
		   reporter.reportRemoteCall(op.invoke(params));
		}
	 }
	 else if (ob instanceof Attribute) {
		Attribute at=(Attribute)ob;
		notifier.reportDebug("OperationAcquiry::run","Attribute " +at+" invoked");
 	    reporter.reportRemoteCall(at.invokeAccessor());
	 }
  }	
 
  catch (Exception e) {
	notifier.reportError("Operation Aquiry::run",e);
  }
}
}
/**
*
* This is a thread that queries the RemoteAccess for the members of the currently
* selected object (i.e. gets object's operations and attributes)
*
* Creation date: (9/26/98 10:40:53 AM)
* @author: Miha Kadunc
*/

private class MembersSearching extends Thread {
	private ListsHandlerBean parent = null;
	private SimpleIntrospectable node = null;
	/**
	 * MembersSearching constructor comment.
	 */
	private MembersSearching(
		ListsHandlerBean in_parent,
		SimpleIntrospectable node) {
		super();
		this.parent = in_parent;
		this.node = node;
	}
	/**
	* Insert the method's description here.
	* Creation date: (9/26/98 11:11:15 AM)
	*/
	public void run() {
		try {
			if (node instanceof Introspectable)
				 ((Introspectable) node).connect();
			Attribute[] attr = node.getAttributes();
			Operation[] oper = node.getOperations();
			parent.updateLists(attr, oper);
		} catch (Exception e) {
			parent.isSearching = false;
			notifier.reportError("Members Searching::run", e);
		}
	}
}
/**
 * ServiceLocatorBean constructor comment.
 */
public ListsHandlerBean() {
}
/**
 * Insert the method's description here.
 * Creation date: (11/14/00 6:51:28 PM)
 */
synchronized void clickedItem(Object item) {
	if (item==null) return;
	try {
		 if (item instanceof Operation){
	        Operation op = (Operation) item;
			notifier.reportDebug(
				"ListsHandlerBean::clickItem",
				"Operation '" + op + "' clicked.");
			boolean[] mask = op.getMask();
			boolean isUserInput = false;
			for (int i = 0; i < mask.length; i++)
				if (mask[i])
					isUserInput = true;
			if (isUserInput || !isConfirmed()) {
				CallMethodDialog cmd =
					new CallMethodDialog(
						op,
						(JFrame) panel.getTopLevelAncestor(),
						true,
						notifier,
						this);
				cmd.show();
			} else {
				invokeOperation(op, new Object[mask.length]);
			}
		}
		else if (item instanceof Attribute){
			Attribute attr = (Attribute) item;
			notifier.reportDebug(
				"ListsHandlerBean::clickItem",
				"Attribute '" + attr + "' accessor clicked.");
			if (attr.isReadOnly()) {
				invokeOperation(attr, null);
			} else {
				notifier.reportMessage("Read only attributes supported only");
				invokeOperation(attr, null);
			}
		}
	} catch (Exception e) {
		handleException("ListsHandler clickItem", e);
	}
}

public void setConfirmed(boolean b) {
	notifier.setConfirmationDialog(!b);
}
private boolean isConfirmed() {
	return !notifier.isConfirmationDialog();
}
/**
 * getEditorPanel method comment.
 */
public javax.swing.JPanel getEditorPanel() {
	if (panel==null) panel=new ListsSimpleIntrospectableDetails(this);
	return panel;
}
/**
 * Called whenever the part throws an exception.
 * @param exception java.lang.Throwable
 */
private void handleException(String message, Throwable exception) {
	/* Uncomment the following lines to print uncaught exceptions to stdout */
	notifier.reportError(message,exception);
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 6:54:50 PM)
 */
public void invocationDestroyed(Invocation inv) {
  if (inv == null) return;
  if (inv.isDestroyed()) return;
  notifier.reportDebug("ListsHandlerBean::invocationDestroyed","I:" + inv);
  nodeRequestListener.removeNode(inv);
  reporter.invocationDestroyed(inv);
  inv.destroy();
}
/**
 * Insert the method's description here.
 * Creation date: (11/2/00 5:06:24 PM)
 */
private void invokeOperation(Object ob, Object[] params) {
  try{
	 new OperationAcquiry(this,reporter,ob,params).start();
  }
  catch(Exception e){
	handleException("ListsHandler invokeOperation",e);
  }

}
/**
 * invokeOperation method comment.
 */
public void invokeOperation(si.ijs.acs.objectexplorer.engine.Operation op, java.lang.Object[] params) {
   invokeOperation((Object)op,params);	
}
/**
 * Insert the method's description here.
 * Creation date: (11/27/00 6:55:38 PM)
 */
public void responseReceived(RemoteResponse response) {
  reporter.reportRemoteResponse(response);
}
/**
 * Insert the method's description here.
 * Creation date: (10/19/00 5:02:20 PM)
 */
private void selectionChange(SimpleIntrospectable node) {
	try {
		if ((currentNode != node) && (!this.isSearching)) {
			currentNode = node;
			panel.clear();
			panel.setDeviceText(node.getName());
			this.isSearching = true;
			new MembersSearching(this, node).start();
			}
		return;
	} catch (Exception e) {
		handleException("ListHandlerBean::selectionChange " + node + " ", e);
		this.isSearching = false;
	}

}
/**
 * setNodeRequestListener method comment.
 */
public void setNodeRequestListener(NodeRequestListener listener) {
  nodeRequestListener = listener;	
}
/**
 * setNotifier method comment.
 */
public void setNotifier(NotificationBean notifier) {
  this.notifier=notifier;	
}
/**
 * setObject method comment.
 */
public void setObject(si.ijs.acs.objectexplorer.engine.SimpleIntrospectable object) {
  if (object==null) {
	  currentNode=null;
	  panel.clear();
  }
  else selectionChange(object);
}
/**
 * Insert the method's description here.
 * Creation date: (11/10/00 4:31:21 PM)
 * @param newReporter si.ijs.acs.objectexplorer.ReporterBean
 */
public void setReporter(ReporterBean newReporter) {
	reporter = newReporter;
}
/**
 * Insert the method's description here.
 * Creation date: (10/31/00 1:33:18 PM)
 */
void setSpecial(boolean special) {
	if (showSpecial == !(special)) {
		showSpecial = special;
		updateLists();
	}
}
/**
 * Insert the method's description here.
 * Creation date: (9/29/98 12:29:42 PM)
 */
private void updateLists() {
	this.isSearching = false;
	if (cacheAttributes != null) {
		DefaultListModel sampleModel = new DefaultListModel();
		for (int i = 0; i < cacheAttributes.length; i++)
			sampleModel.addElement(cacheAttributes[i]);
		panel.setAttributesModel(sampleModel);
	}
	if (cacheOperations != null) {
		DefaultListModel sampleModel1 = new DefaultListModel();
		for (int i = 0; i < cacheOperations.length; i++) {
			if ((!cacheOperations[i].isSpecial()) || (this.showSpecial)) sampleModel1.addElement(cacheOperations[i]);
		}
		panel.setOperationsModel(sampleModel1);
	}
}
/**
 * Insert the method's description here.
 * Creation date: (9/29/98 12:29:42 PM)
 */
private void updateLists(Attribute[] properties, Operation[] methods) {
	this.isSearching = false;
	this.cacheAttributes=properties;
	this.cacheOperations=methods;
	updateLists();
}
}
