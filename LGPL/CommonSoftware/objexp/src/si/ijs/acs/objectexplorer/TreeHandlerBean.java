package si.ijs.acs.objectexplorer;

/**
 * Used for handling OETree events - RemoteAccess initialization, destruction
 * node searching, connecting and disconnecting...
 *
 * @author Miha Kadunc
 *
 * 07.05.2001  Fixed destroying of RemoteAccess
 */
import java.util.Hashtable;
import java.util.Vector;

import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

import si.ijs.acs.objectexplorer.engine.Introspectable;
import si.ijs.acs.objectexplorer.engine.Invocation;
import si.ijs.acs.objectexplorer.engine.NonStickyConnectFailedRemoteException;
import si.ijs.acs.objectexplorer.engine.RemoteAccess;
import si.ijs.acs.objectexplorer.engine.SimpleIntrospectable;
import si.ijs.acs.objectexplorer.engine.BACI.BACIInvocation;
import si.ijs.acs.objectexplorer.engine.BACI.BACIRemoteAccess;
import si.ijs.acs.objectexplorer.engine.BACI.BACIRemoteNode;
import si.ijs.acs.objectexplorer.engine.BACI.BACITreeDataNode;
import si.ijs.acs.objectexplorer.engine.BACI.DelegateInvocation;
import si.ijs.acs.objectexplorer.engine.BACI.DelegateRemoteNode;
import si.ijs.acs.objectexplorer.engine.BACI.InvocationCouple;
import si.ijs.acs.objectexplorer.engine.BACI.RemoteNodeCouple;

public class TreeHandlerBean implements NodeRequestListener {
	/**
	 * Thread used to aquire subnodes of a first time expanded OETreeNode
	 * 
	 * 
	 * Creation date: (9/26/98 10:40:53 AM)
	 * 
	 * @author: Miha Kadunc
	 * @author rbertoncelj
	 */

	public class SubNodesSearching extends Thread {
		private OETreeNode rootNode = null;
		private TreeHandlerBean parent = null;

		/**
		 * ComponentNamesSearching constructor comment.
		 */
		public SubNodesSearching(TreeHandlerBean in_parent, OETreeNode node) {
			super();
			parent = in_parent;
			rootNode = node;
		}

		/**
		 * Thread that searches for subnodes of a selected/clicked node in the tree. It uses RemoteAccess to
		 * connect to the node and to get the array of subnodes, then adds them to the selected node. Appropriate
		 * nodes are added to the treeByType and also the corresponding DelegateRemoteNodes are created and
		 * added to the treeByName.
		 * 
		 * @author: Miha Kadunc
		 * @author: rbertoncelj
		 */
		public void run() {
			try {
				parent.getParent().setReady(false);
				OETreeNode node = rootNode;
				RemoteNodeCouple rncRoot = (RemoteNodeCouple)devices.get(node.getUserObject().toString());
				/*
				 * Check if we're searching for the subnodes of tree's root. If so, we must set the
				 * node to null because RemoteAccess' explodeTreeNode(node) searches for root's subnodes
				 * when node argument is null.
				 */
				if ((node.getLevel() == 0)
						&& (("Objects").equals(node.getName()))
						&& (("root").equals(node.getUserObject().toString()))) {
					node = null;
				} else {
					/*
					 * We must always do all the operations in the treeByType and then make manual
					 * changes to the treeByName, except when we explode a DUMMY node.
					 * So if the selected node is from the treeByName, find the
					 * corresponding node from the treeByType and assign it to node variable so that
					 * the method will process the device node from the treeByType.
					 */
					if (node instanceof DelegateRemoteNode) {
						node = rncRoot.deviceByType;
					} else if (node instanceof BACITreeDataNode && node.getNodeType() == BACIRemoteAccess.DUMMY) {
						OETreeNode[] out_Nodes = ((BACIRemoteAccess) parent.remoteAccess).explodeDummyNode((BACITreeDataNode)node);
						parent.setNodesTreeByName(out_Nodes, rootNode); //Add nodes to the treeByName
						parent.getParent().setReady(true);
						return;
					} else {
						//System.out.println("DEBUG: THIS REALLY SUCKS! "+node.getClass()+" - "+node);
					}
				}
				
				OETreeNode[] out_Nodes = parent.remoteAccess.explodeTreeNode(node); //search for subnodes
				parent.setNodes(out_Nodes, rootNode); //Add nodes to the treeByType
				
				/*
				 * Here we take care of treeByName - if we are exploding the root node, we have to call
				 * RemoteAccess' special method for this, otherwise we just loop trough all the nodes that
				 * were added to the treeByType and do some stuff - see the code below. The point is that
				 * we have to take care of adding attributes and other dinamically created nodes to the
				 * treeByName - that ofcourse only happens if user expanded the device node or attribute
				 * node, so we skip this step if root is TYPE or DOMAIN node.
				 */
				if (node == null) {
					OETreeNode[] outNodes2 = ((BACIRemoteAccess)parent.remoteAccess).explodeRootNodeByName();
					
					parent.setNodesTreeByName(outNodes2, (OETreeNode)parent.treeByName.getModel().getRoot());
				} else if (node.getNodeType() == BACIRemoteAccess.TYPE || node.getNodeType() == BACIRemoteAccess.DOMAIN) {
					//System.out.println("DEBUG: Skipping TYPE or DOMAIN node ("+node+") synchronization.");
					parent.getParent().setReady(true);
					return;
				} else {
					if (rncRoot != null) {
						/*
						 * create corresponding DelegateRemoteNode for each node from out_Nodes that is instance of BACIRemoteNode
						 * and add it to the outNodeTreeByName so that they will be added to the treeByName later.
						 */
						Vector outNodesTreeByName = new Vector();
						for (int i = 0; i < out_Nodes.length; i++) {
							//System.out.print("DEBUG: Processing node "+out_Nodes[i]);
							if (out_Nodes[i] instanceof BACIRemoteNode) {
								RemoteNodeCouple rnc;
								String curl;
								
								if (out_Nodes[i].getUserObject() instanceof String) {
									/*
									 * This usually means that we got a node representing a device and since we put
									 * all the devices to the Hashtable devices, we retreive the rnc for this node from
									 * the hashtable.
									 */
									curl = (String)out_Nodes[i].getUserObject();
									rnc = (RemoteNodeCouple)devices.get(curl);	
								} else {
									/*
									 * If user object is not String, it is usually an attribte discribtion. We never have
									 * those in the Hashtable devices at the begining, so we have to create and add them.
									 */
									curl = (String)rncRoot.deviceByType.getUserObject()+":"+out_Nodes[i].toString();
									//System.out.print(" curl=="+curl);
									RemoteNodeCouple newRnc = (RemoteNodeCouple)devices.get(curl);
									if (newRnc == null) {
										newRnc = new RemoteNodeCouple((BACIRemoteNode)out_Nodes[i], null);
										newRnc.deviceByName = new DelegateRemoteNode(
												out_Nodes[i].toString(),
												parent,
												(BACIRemoteNode)out_Nodes[i]
											);
										devices.put(curl, newRnc);
									}
									rnc = newRnc;
								}
														
								/*
								 * Skip nodes that cause an error (should not ever happen though...)
								 */
								if (rnc == null) {
									notifier.reportError("SubNodesSearching::run - Unexpected null pointer (rnc).");
									continue;
								}
								
								outNodesTreeByName.add(rnc.deviceByName);
								//System.out.println(" * added.");
							} else {
								//System.out.println(" * skipping - not instance of BACIRemoteNode - "+out_Nodes[i]);
								continue;
							}
						}

						parent.setNodesTreeByName(outNodesTreeByName, rncRoot.deviceByName); //Add nodes to the treeByName
						
						//System.out.println("DEBUG: Added to treeByName." + devices.size());
					} else {
						//System.out.println("DEBUG: No rncRoot.");
					}
				}
				

			} catch (Throwable t) {
				parent.getParent().setEnabled(true);
				parent.getNotifier().reportError("Error while searching for subnodes of " + rootNode, t);
				parent.getParent().setReady(true);
			}
			parent.getParent().setReady(true);
		}
	}

	/*
	 * A thread used to set and initialize RemoteAccess
	 * 
	 * Constructor parameters:
	 * 
	 * @accessType (String) name of the RemoteAccessClass e.g. Abeans, BACI
	 * class has to be in the package
	 * si.ijs.acs.objectexplorer.engine."accessType" and has to be named
	 * "accessType"RemoteAccess e.g.
	 * si.ijs.acs.objectexplorer.engine.Abeans.AbeansRemoteAccess
	 * 
	 * @params (Object[]) reference to TreeHandlerBean and NotifierBean (in that
	 * order), i.e. Constructor parameters of the RemoteAccess class
	 * 
	 */
	private class accessSetter extends Thread {
		private String type = null;
		private TreeHandlerBean parent = null;
		private NotificationBean notifier = null;
		private OETreeNode node = null;

		public accessSetter(String accessType,
				NotificationBean notifier, OETreeNode node, TreeHandlerBean parent, Hashtable devices) {

			this.type = accessType;
			this.notifier = notifier;
			this.node = node;
			this.parent = parent;
		}

		public void run() {
			notifier.reportDebug("accessSetter.run", "starting set");
			try {
				ReporterBean.raID++;
				parent.destroyRA();
				parent.getParent().removeEngineMenu();
			} catch (Throwable e) {
				notifier.reportError("Remote Access " + remoteAccess
						+ " could not be properly destroyed", e);
			}
			parent.remoteAccess = null;
			try {
				RemoteAccess ra = constructRemoteAccess(type);
				ra.initialize();
				parent.remoteAccess = ra;
				javax.swing.JMenu engineMenu = ra.getEngineMenu();
				if (engineMenu != null)
					parent.getParent().addEngineMenu(engineMenu);
				else
					notifier.reportMessage(type + " engine menu is missing");
				(new SubNodesSearching(parent, node)).start();
				parent.parent.getReporter().killResponseWindows();
			} catch (Throwable e) {
				notifier.reportError("Remote Access " + type
						+ " could not be found (or initialized)", e);
				parent.getParent().setEnabled(true);
				return;
			}
			initializeDetailsHandler();
			notifier.reportDebug("accessSetter.run", "finishing set");
		}

		private RemoteAccess constructRemoteAccess(String type)
				throws ClassNotFoundException,
				java.lang.reflect.InvocationTargetException,
				InstantiationException, IllegalAccessException {
			String raName = type;
			Object[] parameters = { parent, notifier, devices };
			if (type.indexOf(".") == -1)
				raName = "si.ijs.acs.objectexplorer.engine." + type + "."
						+ type + "RemoteAccess";
			RemoteAccess ra = (RemoteAccess) Class.forName(raName).getConstructors()[0]
			                                                     					.newInstance(parameters);
			return ra;
		}

	}

	/*
	 * A thread used to destroy RemoteAccess
	 * 
	 * Constructor parameters: @remoteAccess (RemoteAccess)
	 * 
	 * 
	 */
	private class accessDestroyer extends Thread {
		private RemoteAccess ra = null;

		private AccessDestroyWindow adw = null;

		public accessDestroyer(RemoteAccess ra, AccessDestroyWindow adw) {
			super();
			this.ra = ra;
			this.adw = adw;
		}

		public void run() {
			notifier.reportDebug("accessDestroyer.run", "starting destroy");
			try {
				notifier.setShowError(false);
				notifier.setAccessDestroyWindow(adw);
				ra.destroy();
			} catch (Exception e) {
				notifier.reportError("Error while destroying RemoteAccess", e,
						false);
			}
			notifier.setShowError(true);
			notifier.setAccessDestroyWindow(null);
			adw.terminate(true);
			notifier.reportDebug("accessDestroyer.run", "end of destroy");

		}
	}

	private String accessType = null;
	private transient Introspectable clicked = null;
	private transient OETree tree = null;
	private transient RemoteAccess remoteAccess = null;
	private transient NotificationBean notifier = null;
	private transient ObjectExplorer parent = null;
	private transient TreeNode selectedNode = null;
	private ObjectDetailsHandler handler = null;
	
	// ----- variables added for new hierarchy -----
	private Hashtable devices = null;
	private Hashtable invList = new Hashtable();
	private OETree treeByName = null;
	// ----- * by rbertoncelj
	
	/**
	 * returns the invList
	 * @return
	 * @author rbertoncelj
	 */
	public Hashtable getInvList() {
		return invList;
	}
	
	/**
	 * Returns the remoteAccess. Used in DelegateRemoteNode's constructor.
	 * @author rbertoncelj
	 */
	public RemoteAccess getRemoteAccess() {
		return remoteAccess;
	}
	
	public TreeHandlerBean(Hashtable devices, OETree treeByType, OETree treeByName) {
		this.devices = devices;
		this.tree = treeByType;
		this.treeByName = treeByName;
	}

	/**
	 * @author rbertoncelj
	 */
	public void addNode(javax.swing.tree.TreeNode node,
			javax.swing.tree.TreeNode parentNode) {
		addNodes(new TreeNode[] { node }, parentNode);
		if (node instanceof BACIInvocation) {
			BACIInvocation invoc = (BACIInvocation)node;
			InvocationCouple ic = new InvocationCouple(invoc, null);
			ic.invocationByName = new DelegateInvocation(invoc, treeByName, (BACIRemoteAccess)remoteAccess);
			
			String curl = (String)((OETreeNode)parentNode.getParent()).getUserObject()+":"+parentNode.toString();
			RemoteNodeCouple rnc = (RemoteNodeCouple)devices.get(curl);
			if (rnc == null) {
				notifier.reportError("TreeHandlerBean::addNode - Unexpected null pointer (rnc).");
				return;
			}
			
			// yatagai : the name can not be the proper key -- does not work
			// when more than 1 sub node are added.
			// getInvList().put(invoc.getName(),ic);
			getInvList().put(invoc,ic);
			
			setNodesTreeByName(new TreeNode[] { ic.invocationByName }, rnc.deviceByName);
			//System.out.println("DEBUG Added to invList "+invoc.getName()+"!");
		}
	}
	
	public InvocationCouple getInvocationCouple(Invocation invoc) {
		return (InvocationCouple)invList.get(invoc);
	}

	/**
	 * addNodes method comment.
	 */
	public void addNodes(javax.swing.tree.TreeNode[] nodes,
			javax.swing.tree.TreeNode parentNode) {
		setNodes(nodes, parentNode);
	}

	/**
	 * @author Miha Kadunc
	 * @author rbertoncelj
	 */
	public void connect() {
		//System.out.println("Connecting to "+clicked);
		try {
			clicked.connect();
		} catch (NonStickyConnectFailedRemoteException nscfre) {
			// return from method gracefully
			return;
		}
		((DefaultTreeModel) tree.getModel()).reload((TreeNode) clicked);
		if ((tree.getSelectionPath() != null)
				&& (tree.getSelectionPath().getLastPathComponent() == clicked)) {
			this.selectedNode = null;
			handler.setObject(clicked);
		}
		
		if (clicked instanceof BACIRemoteNode) {
			BACIRemoteNode myNode = (BACIRemoteNode)clicked;
			RemoteNodeCouple rnc;
			String curl;
			if (myNode.getUserObject() instanceof String) {
				curl = (String)myNode.getUserObject();
				rnc = (RemoteNodeCouple)devices.get(curl);	
			} else {
				myNode.getParent();
				curl = (String)((OETreeNode)myNode.getParent()).getUserObject()+":"+myNode.toString();
				RemoteNodeCouple newRnc = (RemoteNodeCouple)devices.get(curl);
				rnc = newRnc;
			}
			if (rnc == null) {
				clicked = null;
				//System.out.println("DEBUG: thb connect - null rnc.");
				return;
			}
			((DefaultTreeModel) treeByName.getModel()).reload((TreeNode) rnc.deviceByName);
		}
		
		clicked = null;
		tree.repaint();
		treeByName.repaint();
	}

	/**
	 * Insert the method's description here. Creation date: (30.11.2000
	 * 21:48:15)
	 */
	public void destroy() {
		destroyRA();
	}

	/**
	 * Insert the method's description here. Creation date: (30.11.2000
	 * 21:48:15)
	 */
	private void destroyRA() {
		if (remoteAccess != null) {
			AccessDestroyWindow adw = new AccessDestroyWindow(parent);
			(new accessDestroyer(remoteAccess, adw)).start();
			adw.show();
		}
	}

	/**
	 * @author Miha Kadunc
	 * @author rbertoncelj
	 */
	public void disconnect() {
		notifier.reportDebug("TreeHandlerBean::disconnect",
				"Requesting engine to disconnect the introspectable..:");
		if (clicked instanceof BACIRemoteNode) {
			BACIRemoteNode myNode = (BACIRemoteNode)clicked;
			RemoteNodeCouple rnc;
			String curl;
			if (myNode.getUserObject() instanceof String) {
				curl = (String)myNode.getUserObject();
			} else {
				curl = (String)((OETreeNode)myNode.getParent()).getUserObject()+":"+myNode.toString();
			}
			rnc = (RemoteNodeCouple)devices.get(curl);
			if (rnc == null) {
				clicked = null;
				notifier.reportError("TreeHandlerBean::disconnect - Unexpected null pointer (rnc).");
				return;
			}
				/*
				 * Clean treeByType
				 */
				if (rnc.deviceByType.isConnected()) {
					OETreeNode clk = (OETreeNode) rnc.deviceByType;
					clk.setChildrenDefined(true);
					rnc.deviceByType.disconnect();
					TreePath clkPath = new TreePath(clk.getPath());
					if (clkPath.isDescendant(tree.getSelectionPath())
							|| (clkPath.equals(tree.getSelectionPath()))) {
						this.selectedNode = clk;
						tree.setSelectionPath(clkPath);
						handler.setObject(null);
					}
					tree.collapsePath(clkPath);
					clk.removeAllChildren();
					((DefaultTreeModel) tree.getModel()).reload(clk);
					clk.setChildrenDefined(false);
				}
				
				/*
				 * Clean treeByName
				 */
				OETreeNode clk = (OETreeNode) rnc.deviceByName;
				clk.setChildrenDefined(true);
				rnc.deviceByName.disconnect();
				TreePath clkPath = new TreePath(clk.getPath());
				if (clkPath.isDescendant(treeByName.getSelectionPath())
						|| (clkPath.equals(treeByName.getSelectionPath()))) {
					this.selectedNode = clk;
					treeByName.setSelectionPath(clkPath);
					handler.setObject(null);
				}
				treeByName.collapsePath(clkPath);
				purgeChildren(clk);
				clk.removeAllChildren();
				((DefaultTreeModel) treeByName.getModel()).reload(clk);
				clk.setChildrenDefined(false);
				
		} else {
			notifier.reportError("TreeHandlerBean::disconnect - clicked not instanceof BACIRemoteNode.");
			if (clicked instanceof OETreeNode) {
				if (clicked.isConnected()) {
					OETreeNode clk = (OETreeNode) clicked;
					clk.setChildrenDefined(true);
					clicked.disconnect();
					TreePath clkPath = new TreePath(clk.getPath());
					if (clkPath.isDescendant(tree.getSelectionPath())
							|| (clkPath.equals(tree.getSelectionPath()))) {
						this.selectedNode = clk;
						tree.setSelectionPath(clkPath);
						handler.setObject(null);
					}
					tree.collapsePath(clkPath);
					purgeChildren(clk);
					clk.removeAllChildren();
					((DefaultTreeModel) tree.getModel()).reload(clk);
					clk.setChildrenDefined(false);
				}
			}
		}
		clicked = null;
	}
	
	/**
	 * Recursively remove all the children of this node from devices Hashtable and from
	 * this node itself. This method should always be used when working with BACIRemoteNode nodes
	 * before nodes own removeAllChildren() method, otherwise the children will be left in
	 * Hashtable devices and that will produce unpredictable behaviour of objexp.
	 * 
	 * @author rbertoncelj
	 */
	public void purgeChildren(OETreeNode node) {
		if (node == null || node.childrenHolder == null) return;
		for (int i = 0; i < node.childrenHolder.size(); i++) {
			purgeAll((OETreeNode)node.childrenHolder.get(i));
		}
	}
	
	private void purgeAll(OETreeNode node) {
		if (node == null || node.childrenHolder == null) return;
		for (int i = 0; i < node.childrenHolder.size(); i++) {
			purgeAll((OETreeNode)node.childrenHolder.get(i));
		}
		String curl;
		if (node.getUserObject() instanceof String) {
			curl = (String)node.getUserObject();
		} else {
			node.getParent();
			curl = (String)((OETreeNode)node.getParent()).getUserObject()+":"+node.toString();
		}
		devices.remove(curl);
	}

	/**
	 * @author Miha Kadunc
	 * @author rbertoncelj
	 */
	public synchronized void getDevices(FirstTimeExpandedEvent event) {
		OETreeNode node = event.getTreeNode();
		if (event.getSource() == treeByName) {
			if (node instanceof BACIRemoteNode) {
				BACIRemoteNode myNode = (BACIRemoteNode)node;
				RemoteNodeCouple rnc;
				String curl;
				if (myNode.getUserObject() instanceof String) {
					curl = (String)myNode.getUserObject();
					rnc = (RemoteNodeCouple)devices.get(curl);
				} else {
					myNode.getParent();
					curl = (String)((OETreeNode)myNode.getParent()).getUserObject()+":"+myNode.toString();
					RemoteNodeCouple newRnc = (RemoteNodeCouple)devices.get(curl);
					rnc = newRnc;
				}
				if (rnc == null) {
					notifier.reportError("TreeHandlerBean::getDevices - Unexpected null pointer (rnc).");
					return;
				}
				rnc.deviceByType.getChildCount();
				return;
			}
		}
		
		notifier.reportDebug("TreeHandlerBean::getDevices", "node: " + node
				+ " " + node.getUserObject());
		
		try {
			if (node == null)
				return;
			if ((node.getUserObject() instanceof String)
					&& (node.getUserObject().equals("root"))) {

				// only OETree1 can can trigger search
				if (node.getParentTree() == treeByName)
					return;

				notifier.reportMessage("Initializing " + accessType
						+ " engine. Please wait...");
				notifier.reportDebug("TreeHandlerBean::setAccess",
						"Selected engine: " + accessType);
				
				// clear treeByName
				treeByName.clearTree();
				invList.clear();
				devices.clear();
				
				setAccess(node);
			} else {
				(new SubNodesSearching(this, node)).start();
			}
			return;
		} catch (Exception e) {
			notifier.reportError(
					"Error while searching for devices of " + node, e);
		}
	}

	/**
	 * Insert the method's description here. Creation date: (11/7/00 5:00:26 PM)
	 * 
	 * @return si.ijs.acs.objectexplorer.NotificationBean
	 */
	private NotificationBean getNotifier() {
		return notifier;
	}

	/**
	 * Insert the method's description here. Creation date: (3/27/2001 5:24:55
	 * PM)
	 * 
	 * @return si.ijs.acs.objectexplorer.ObjectExplorer
	 */
	private ObjectExplorer getParent() {
		return parent;
	}

	/**
	 * Insert the method's description here. Creation date: (9/28/98 5:30:11 PM)
	 * 
	 * @return si.ijs.acs.objectexplorer.OETree
	 */
	public OETree getTree() {
		return tree;
	}

	/**
	 * Insert the method's description here. Creation date: (3/27/2001 5:24:55
	 * PM)
	 * 
	 * @return si.ijs.acs.objectexplorer.ObjectExplorer
	 */
	private void initializeDetailsHandler() {
		handler = new ListsHandlerBean();
		handler.setNotifier(notifier);
		handler.setReporter(parent.getReporter());
		handler.setNodeRequestListener(this);
		parent.setDetailsPanel(handler.getEditorPanel());
	}

	/**
	 * removeNode method comment.
	 */
	public void removeNode(javax.swing.tree.TreeNode node) {
		if (node.getParent() == null) {
			return;
		}
		
		InvocationCouple ic = getInvocationCouple((Invocation)node);
		if (ic == null) {
			notifier.reportError("TreeHandlerBean::removeNode - Unexpected null pointer (ic).");
			return;
		}
		
		/*
		 * Selecet parent if the note that is to be deleted is currently selected
		 */
		if (tree.getSelectionPath() != null) {
			if (tree.getSelectionPath().getLastPathComponent() == node)
				tree.setSelectionPath(new TreePath(((DefaultMutableTreeNode) node
						.getParent()).getPath()));
		} else if (treeByName.getSelectionPath() == null) {
			if (treeByName.getSelectionPath().getLastPathComponent() == ic.invocationByName)
				treeByName.setSelectionPath(new TreePath(((DefaultMutableTreeNode) ic.invocationByName
						.getParent()).getPath()));
		}
		
		/*
		 * remove node from treeByType
		 */
		DefaultTreeModel model = (DefaultTreeModel) tree.getModel();
		model.removeNodeFromParent((MutableTreeNode) ic.invocationByType);
		
		/*
		 * remove node from treeByName
		 */
		model = (DefaultTreeModel) treeByName.getModel();
		model.removeNodeFromParent((MutableTreeNode) ic.invocationByName);
	}

	/**
	 * removeNodes method comment.
	 */
	public void removeNodes(javax.swing.tree.TreeNode[] nodes) {
		for (int i = 0; i < nodes.length; i++) {
			removeNode(nodes[i]);
		}
	}

	/**
	 * Insert the method's description here. Creation date: (30.11.2000
	 * 21:48:15)
	 */
	public void selectionChanged(OETree sourceTree) {
		if (sourceTree.getSelectionPath() == null) {
			selectedNode = null;
			handler.setObject(null);
			return;
		}
		if (selectedNode != (TreeNode) sourceTree.getSelectionPath()
				.getLastPathComponent()) {
			selectedNode = (TreeNode) sourceTree.getSelectionPath()
					.getLastPathComponent();
			if (selectedNode instanceof Introspectable)
			{
				try {
					((Introspectable) selectedNode).connect();
				} catch (NonStickyConnectFailedRemoteException nscfre) {
					// return from method gracefully
					return;
				}
			}
			if ((selectedNode instanceof SimpleIntrospectable)
					&& !((selectedNode instanceof Invocation) && !((Invocation) selectedNode)
							.isControllable())) {
				handler.setObject((SimpleIntrospectable) selectedNode);
				return;
			}
			handler.setObject(null);
		}
	}

	/**
	 * Insert the method's description here. Creation date: (11/7/00 5:24:26 PM)
	 */
	public void setAccess(String accessType) {
		this.accessType = accessType;
	}

	/**
	 * Insert the method's description here. Creation date: (11/7/00 5:24:26 PM)
	 */
	private void setAccess(OETreeNode node) {
		try {
			if (remoteAccess != null) {
				parent.getReporter().clearResponseWindows();
				this.handler.setObject(null);
			}
			(new accessSetter(accessType, this.notifier, node, this, devices)).start();
		} catch (Exception e) {
			notifier.reportError("Remote Access " + accessType
					+ " could not be found (or initialized)", e);
			return;
		}
	}
	
	/**
	 * Returns the treeByName. Used in DelegateRemoteNode() and by BACIRemoteAccess.
	 * @return
	 * @author rbertoncelj
	 */
	public OETree getTreeByName() {
		return treeByName;
	}

	/**
	 * Insert the method's description here. Creation date: (3/26/2001 10:46:41
	 * PM)
	 */
	public void setClicked(Introspectable node) {
		clicked = node;
	}

	/**
	 * Insert the method's description here. Creation date: (3/26/2001 10:46:41
	 * PM)
	 */
	public Introspectable getClicked() {
		return clicked;
	}

	/**
	 * Insert the method's description here. Creation date: (10/2/98 6:09:47 PM)
	 */
	private void setNodes(TreeNode[] in_nodes, TreeNode node) {
		DefaultTreeModel model = (DefaultTreeModel) tree.getModel();
		if (in_nodes != null && node != null) {
			int[] nIndexs = new int[in_nodes.length];

			for (int i = 0; i < in_nodes.length; i++) {
				/*
				((OETreeNode) node).insert(
						((DefaultMutableTreeNode) in_nodes[i]), node
								.getChildCount());
				*/
				((OETreeNode) node).add(
						((DefaultMutableTreeNode) in_nodes[i]));
				nIndexs[i] = node.getChildCount() - 1;
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
	 * Add nodes to the treeByName.
	 * @param newNodes		Nodes to be added to the tree.
	 * @param parentNode	Node in the tree, to which new nodes will be added as children.
	 * 
	 * @author rbertoncelj
	 */
	private void setNodesTreeByName(Vector newNodes, TreeNode parentNode) {
		DefaultTreeModel model = (DefaultTreeModel) treeByName.getModel();
		if (newNodes != null && parentNode != null && newNodes.size() > 0) {
			int[] nIndexs = new int[newNodes.size()];

			for (int i = 0; i < newNodes.size(); i++) {
				/*
				((OETreeNode) node).insert(
						((DefaultMutableTreeNode) in_nodes[i]), node
								.getChildCount());
				*/
				((OETreeNode) parentNode).add(
						((DefaultMutableTreeNode) newNodes.get(i)));
				nIndexs[i] = parentNode.getChildCount() - 1;
			}
			model.nodesWereInserted(parentNode, nIndexs);
			java.awt.EventQueue.invokeLater(new Runnable() {
				public void run() {
					treeByName.revalidate();
					treeByName.repaint();
				}
			});
		}
		getParent().setEnabled(true);
		return;
	}
	
	/**
	 * Add nodes to the treeByName.
	 * @param newNodes		Nodes to be added to the tree.
	 * @param parentNode	Node in the tree, to which new nodes will be added as children.
	 * 
	 * @author rbertoncelj
	 */
	private void setNodesTreeByName(TreeNode[] newNodes, TreeNode parentNode) {
		DefaultTreeModel model = (DefaultTreeModel) treeByName.getModel();
		if (newNodes != null && parentNode != null) {
			int[] nIndexs = new int[newNodes.length];

			for (int i = 0; i < newNodes.length; i++) {
				/*
				((OETreeNode) node).insert(
						((DefaultMutableTreeNode) in_nodes[i]), node
								.getChildCount());
				*/
				((OETreeNode) parentNode).add(
						((DefaultMutableTreeNode) newNodes[i]));
				nIndexs[i] = parentNode.getChildCount() - 1;
			}
			model.nodesWereInserted(parentNode, nIndexs);
			java.awt.EventQueue.invokeLater(new Runnable() {
				public void run() {
					treeByName.revalidate();
					treeByName.repaint();
				}
			});
		}
		getParent().setEnabled(true);
		return;
	}

	/**
	 * Insert the method's description here. Creation date: (11/7/00 5:00:26 PM)
	 * 
	 * @param newNotifier
	 *            si.ijs.acs.objectexplorer.NotificationBean
	 */
	public void setNotifier(NotificationBean newNotifier) {
		notifier = newNotifier;
		if (tree != null)
			tree.setNotifier(newNotifier);
	}

	/**
	 * Insert the method's description here. Creation date: (3/27/2001 5:24:55
	 * PM)
	 * 
	 * @param newParent
	 *            si.ijs.acs.objectexplorer.ObjectExplorer
	 */
	public void setParent(ObjectExplorer newParent) {
		parent = newParent;
	}

	/**
	 * Insert the method's description here. Creation date: (9/28/98 5:30:11 PM)
	 * 
	 * @param newTree
	 *            si.ijs.acs.objectexplorer.OETree
	 */
	public void setTree(OETree newTree) {
		tree = newTree;
		if (notifier != null)
			tree.setNotifier(notifier);
	}
}
