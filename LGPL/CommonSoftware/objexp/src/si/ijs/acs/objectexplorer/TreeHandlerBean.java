package si.ijs.acs.objectexplorer;

/**
 * Used for handling OETree events - RemoteAccess initialization, destruction
 * node searching, connecting and disconnecting...
 *
 * @author Miha Kadunc
 *
 * 07.05.2001  Fixed destroying of RemoteAccess
 */
import javax.swing.tree.* ;
import si.ijs.acs.objectexplorer.engine.*;

public class TreeHandlerBean implements NodeRequestListener{
/**
 * Thread used to aquire subnodes of a first time expanded OETreeNode
 *
 *
 * Creation date: (9/26/98 10:40:53 AM)
 * @author: Miha Kadunc
 */

public class SubNodesSearching extends Thread{
  private OETreeNode rootNode = null;
  private TreeHandlerBean parent =null;
/**
 * ComponentNamesSearching constructor comment.
 */
public SubNodesSearching(TreeHandlerBean in_parent,OETreeNode node) {
	super();
	parent=in_parent;
	rootNode =node;
}
/**
 * Insert the method's description here.
 * Creation date: (9/26/98 11:11:15 AM)
 */

public void run() {
	try{
	  OETreeNode node=rootNode;
	  if ( (node.getLevel()==0) && (("Objects").equals(node.getName())) && (("root").equals(node.getUserObject().toString())) ) node=null;
	  OETreeNode[] out_Nodes =  parent.remoteAccess.explodeTreeNode(node);
	  parent.setNodes(out_Nodes,rootNode);
	}
	catch (Throwable t){
	  parent.getParent().setEnabled(true);
	  parent.getNotifier().reportError("Error while searching for subnodes of "+rootNode,t);	
	}
}
}

/* A thread used to set and initialize RemoteAccess
 *
 * Constructor parameters:
 *
 * @accessType (String) name of the RemoteAccessClass e.g. Abeans, BACI
 *                      class has to be in the package 
 *                        si.ijs.acs.objectexplorer.engine."accessType"
 *                      and has to be named "accessType"RemoteAccess
 *                      e.g. si.ijs.acs.objectexplorer.engine.Abeans.AbeansRemoteAccess
 *
 * @params (Object[])   reference to TreeHandlerBean and NotifierBean (in that order),
 *                      i.e. Constructor parameters of the RemoteAccess class
 *
 */
private class accessSetter extends Thread {
	private String type = null;
	private TreeHandlerBean treehandler = null;
	private NotificationBean notifier = null;
	private OETreeNode node = null;

	public accessSetter(
		String accessType, 
		TreeHandlerBean treehandler, 
		NotificationBean notifier, 
		OETreeNode node) {
			
		this.type = accessType;
		this.treehandler = treehandler;
		this.notifier = notifier;
		this.node = node;
	}

public void run() {
  notifier.reportDebug("accessSetter.run","starting set");
  try {
	ReporterBean.raID++;
	treehandler.destroyRA();
	treehandler.getParent().removeEngineMenu();
  } catch (Throwable e) {
	notifier.reportError(
	  "Remote Access " + remoteAccess + " could not be properly destroyed",
	  e);
  }
  treehandler.remoteAccess= null;
  try {

	RemoteAccess ra=constructRemoteAccess(type);
	ra.initialize();
	treehandler.remoteAccess= ra;
	javax.swing.JMenu engineMenu= ra.getEngineMenu();
	if (engineMenu != null)
	  treehandler.getParent().addEngineMenu(engineMenu);
	else
	  notifier.reportMessage(type + " engine menu is missing");
	(new SubNodesSearching(treehandler, node)).start();
	treehandler.parent.getReporter().killResponseWindows();
  } catch (Throwable e) {
	notifier.reportError("Remote Access " + type + " could not be found (or initialized)", e);
	treehandler.getParent().setEnabled(true);
	return;
  }
  initializeDetailsHandler();
  notifier.reportDebug("accessSetter.run","finishing set");
}

private RemoteAccess constructRemoteAccess(String type) throws ClassNotFoundException, java.lang.reflect.InvocationTargetException, InstantiationException, IllegalAccessException{
 	String raName=type;
	Object[] parameters= { treehandler, notifier };
	if (type.indexOf(".") == -1)
	  raName= "si.ijs.acs.objectexplorer.engine." + type + "." + type + "RemoteAccess";
	return (RemoteAccess) Class.forName(raName).getConstructors()[0].newInstance(parameters);
}

}

/* A thread used to destroy RemoteAccess
 *
 * Constructor parameters:
 * @remoteAccess (RemoteAccess)
 *
 *
 */
private class accessDestroyer extends Thread{
	private RemoteAccess ra = null;
	private AccessDestroyWindow adw=null;

	public accessDestroyer(RemoteAccess ra, AccessDestroyWindow adw)
	{  super();
	   this.ra=ra;
	   this.adw=adw;
	}

	public void run() {
		  notifier.reportDebug("accessDestroyer.run","starting destroy");
	      try {
		     notifier.setShowError(false);
		     notifier.setAccessDestroyWindow(adw);
		     ra.destroy();		     
	      }
	      catch (Exception e) {
		     notifier.reportError("Error while destroying RemoteAccess",e,false);
	      }
	      notifier.setShowError(true);
	      notifier.setAccessDestroyWindow(null);
		  adw.terminate(true);
		  notifier.reportDebug("accessDestroyer.run","end of destroy");
		  
	}
}
	private String accessType=null;

	private transient Introspectable clicked=null;
	private transient OETree tree = null;
	private transient RemoteAccess remoteAccess = null;
	private transient NotificationBean notifier = null;
	private transient ObjectExplorer parent=null;
	private transient TreeNode selectedNode=null;

	private ObjectDetailsHandler handler=null;
/**
 * TreeHandlerBean constructor comment.
 */
public TreeHandlerBean() {
}
/**
 * addNode method comment.
 */
public void addNode(javax.swing.tree.TreeNode node, javax.swing.tree.TreeNode parentNode) {
  addNodes(new TreeNode[]{node},parentNode);	
}
/**
 * addNodes method comment.
 */
public void addNodes(javax.swing.tree.TreeNode[] nodes, javax.swing.tree.TreeNode parentNode) {
  setNodes(nodes,parentNode);	
}
/**
 * Insert the method's description here.
 * Creation date: (3/26/2001 10:33:18 PM)
 */
public void connect() {
	clicked.connect();
	((DefaultTreeModel)tree.getModel()).reload((TreeNode)clicked);
	if ((tree.getSelectionPath()!=null) && (tree.getSelectionPath().getLastPathComponent()==clicked)) {
		this.selectedNode=null;
	    handler.setObject(clicked);
	}
	clicked=null;
	tree.repaint();
}
/**
 * Insert the method's description here.
 * Creation date: (30.11.2000 21:48:15)
 */
public void destroy() {
  destroyRA();
}
/**
 * Insert the method's description here.
 * Creation date: (30.11.2000 21:48:15)
 */
private void destroyRA() {
	if (remoteAccess != null) {
		AccessDestroyWindow adw= new AccessDestroyWindow(parent);
		(new accessDestroyer(remoteAccess,adw)).start();
		adw.show();
	}
}
/**
 * Insert the method's description here.
 * Creation date: (3/26/2001 10:33:18 PM)
 */
public void disconnect() {notifier.reportDebug("TreeHandlerBean::disconnect","Requesting engine to disconnect the introspectable..:");
	if (clicked instanceof OETreeNode) {
		if (clicked.isConnected()) {
			OETreeNode clk = (OETreeNode) clicked;
			clk.setChildrenDefined(true);
			clicked.disconnect();
			TreePath clkPath = new TreePath(clk.getPath());
			if (clkPath.isDescendant(tree.getSelectionPath()) || (clkPath.equals(tree.getSelectionPath()))) {
	            this.selectedNode=clk;
	            tree.setSelectionPath(clkPath);
				handler.setObject(null);
			}
			tree.collapsePath(clkPath);
			clk.removeAllChildren();
			((DefaultTreeModel) tree.getModel()).reload(clk);
			clk.setChildrenDefined(false);
		}
	}
	clicked = null;
}
/**
 * This method was created by a SmartGuide.
 */
public synchronized void getDevices (FirstTimeExpandedEvent event) {
   OETreeNode node=event.getTreeNode();
   notifier.reportDebug("TreeHandlerBean::getDevices", "node: "+ node + " " +node.getUserObject());
   try{
	 if (node == null) return;
	 if ((node.getUserObject() instanceof String) && (node.getUserObject().equals("root"))) {
	   notifier.reportMessage("Initializing " + accessType + " engine. Please wait...");
	   notifier.reportDebug("TreeHandlerBean::setAccess", "Selected engine: "+ accessType);
 	   setAccess(node);
	 }
	 else {
	   (new SubNodesSearching(this,node)).start();  
	 }
	 return;
   }
   catch(Exception e){
	 notifier.reportError("Error while searching for devices of "+node,e);
   }
}
/**
 * Insert the method's description here.
 * Creation date: (11/7/00 5:00:26 PM)
 * @return si.ijs.acs.objectexplorer.NotificationBean
 */
private NotificationBean getNotifier() {
	return notifier;
}
/**
 * Insert the method's description here.
 * Creation date: (3/27/2001 5:24:55 PM)
 * @return si.ijs.acs.objectexplorer.ObjectExplorer
 */
private ObjectExplorer getParent() {
	return parent;
}
/**
 * Insert the method's description here.
 * Creation date: (9/28/98 5:30:11 PM)
 * @return si.ijs.acs.objectexplorer.OETree
 */
public OETree getTree() {
	return tree;
}
/**
 * Insert the method's description here.
 * Creation date: (3/27/2001 5:24:55 PM)
 * @return si.ijs.acs.objectexplorer.ObjectExplorer
 */
private void initializeDetailsHandler() {
  handler=new ListsHandlerBean();
  handler.setNotifier(notifier);
  handler.setReporter(parent.getReporter());
  handler.setNodeRequestListener(this);
  parent.setDetailsPanel(handler.getEditorPanel());
}
/**
 * removeNode method comment.
 */
public void removeNode(javax.swing.tree.TreeNode node) {
	if ((node.getParent()==null) || tree.getSelectionPath()==null)return;
	DefaultTreeModel model = (DefaultTreeModel)tree.getModel();
	if (tree.getSelectionPath().getLastPathComponent()==node) tree.setSelectionPath(new TreePath(((DefaultMutableTreeNode)node.getParent()).getPath()));
	model.removeNodeFromParent((MutableTreeNode)node);
	
	}
/**
 * removeNodes method comment.
 */
public void removeNodes(javax.swing.tree.TreeNode[] nodes) {
	for (int i = 0; i < nodes.length; i++){
		removeNode(nodes[i]);
	}
}
/**
 * Insert the method's description here.
 * Creation date: (30.11.2000 21:48:15)
 */
public void selectionChanged() {
	if (tree.getSelectionPath()==null) {
		selectedNode=null;
		handler.setObject(null);
		return;
	}
	if (selectedNode
		!= (TreeNode) tree.getSelectionPath().getLastPathComponent()) {
		selectedNode = (TreeNode) tree.getSelectionPath().getLastPathComponent();
		if (selectedNode instanceof Introspectable)
			 ((Introspectable) selectedNode).connect();
		if ((selectedNode instanceof SimpleIntrospectable)
			&& !((selectedNode instanceof Invocation)
				&& !((Invocation) selectedNode).isControllable())) {
			handler.setObject((SimpleIntrospectable) selectedNode);
			return;
		}
		handler.setObject(null);
	}
}
/**
 * Insert the method's description here.
 * Creation date: (11/7/00 5:24:26 PM)
 */
public void setAccess(String accessType) {
  this.accessType=accessType;
}
/**
 * Insert the method's description here.
 * Creation date: (11/7/00 5:24:26 PM)
 */
private void setAccess(OETreeNode node) {
  try{
	if (remoteAccess!=null) {
		parent.getReporter().clearResponseWindows();
	    this.handler.setObject(null);
	}
	(new accessSetter(accessType,this,this.notifier,node)).start();
  }
  catch(Exception e) {
	notifier.reportError("Remote Access " + accessType + " could not be found (or initialized)",e);
	return;
  }
}
/**
 * Insert the method's description here.
 * Creation date: (3/26/2001 10:46:41 PM)
 */
public void setClicked(Introspectable node) {
	clicked=node;
}
/**
 * Insert the method's description here.
 * Creation date: (3/26/2001 10:46:41 PM)
 */
public Introspectable getClicked() {
	return clicked;
}
/**
 * Insert the method's description here.
 * Creation date: (10/2/98 6:09:47 PM)
 */
private void setNodes(TreeNode[] in_nodes, TreeNode node) {
	DefaultTreeModel model = (DefaultTreeModel)tree.getModel();
	if (in_nodes != null && node != null) {
		int[] nIndexs = new int[in_nodes.length];

		for (int i = 0; i < in_nodes.length; i++) {
			((OETreeNode)node).insert(((DefaultMutableTreeNode)in_nodes[i]), node.getChildCount());
			nIndexs[i] = node.getChildCount()-1;
		}
		model.nodesWereInserted(node, nIndexs);
		java.awt.EventQueue.invokeLater(new Runnable() {
			public void run() {
				tree.revalidate();
				tree.repaint();
			}
		});
	}
	getParent().setEnabled(true);
	return;
}
/**
 * Insert the method's description here.
 * Creation date: (11/7/00 5:00:26 PM)
 * @param newNotifier si.ijs.acs.objectexplorer.NotificationBean
 */
public void setNotifier(NotificationBean newNotifier) {
	notifier = newNotifier;
	if (tree!=null) tree.setNotifier(newNotifier);
}
/**
 * Insert the method's description here.
 * Creation date: (3/27/2001 5:24:55 PM)
 * @param newParent si.ijs.acs.objectexplorer.ObjectExplorer
 */
public void setParent(ObjectExplorer newParent) {
	parent = newParent;
}
/**
 * Insert the method's description here.
 * Creation date: (9/28/98 5:30:11 PM)
 * @param newTree si.ijs.acs.objectexplorer.OETree
 */
public void setTree(OETree newTree) {
	tree = newTree;
	if (notifier!=null) tree.setNotifier(notifier);
}
}
