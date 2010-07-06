/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.gui.loglevel.tree;

import java.util.logging.Logger;

import javax.swing.SwingWorker;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;

import org.omg.CORBA.ORB;

import si.ijs.maci.ClientInfo;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ContainerInfo;
import si.ijs.maci.Manager;
import alma.acs.gui.loglevel.tree.node.TreeClientInfo;
import alma.acs.gui.loglevel.tree.node.TreeComponentInfo;
import alma.acs.gui.loglevel.tree.node.TreeContainerInfo;
import alma.acs.gui.loglevel.tree.node.TreeContentInfo;

/**
 * The model for the tree of containers/components
 * 
 * @author acaproni
 *
 */
public class LogLvlTreeModel extends DefaultTreeModel implements LogLevelListener {

	private static final long serialVersionUID = 4164177256689824546L;

	/**
	 * A class to force a node to appear as non-leaf
	 * 
	 * @author acaproni
	 *
	 */
	public class NoLeafNode extends DefaultMutableTreeNode {

		private static final long serialVersionUID = 5681551972997000126L;

		/**
		 * Constructor
		 * 
		 * @param str The user object
		 */
		public NoLeafNode(Object obj) {
			super(obj);
		}
		
		/**
		 * Force the node to be a non-leaf
		 * 
		 * @return false
		 */
		public boolean isLeaf() {
			return false;
		}
	}
	
	// The client to get componets, conainers, clients from the manager 
	private AdministratorClient admin=null;
	
	// The ORB
	private ORB orb=null;
	
	// The logger
	private Logger log=null;
	
	// The root node of the tree
	private DefaultMutableTreeNode rootNode = null;
	
	// The non leaf nodes for containers and clients
	//
	// Components are shown on the container node and in the Components node
	private DefaultMutableTreeNode managersNode = new NoLeafNode("Managers");
	private DefaultMutableTreeNode clientsNode = new NoLeafNode("Clients");
	private DefaultMutableTreeNode containersNode = new NoLeafNode("Containers");
	
	// The node of components (it can be hidden depending on user taste)
	private DefaultMutableTreeNode componentsNode = new NoLeafNode("Components");
	
	/**
	 * Constructor
	 *
	 */
	public LogLvlTreeModel(ORB theOrb, Logger theLogger) {
		super(null);
		if (theOrb==null) {
			throw new IllegalArgumentException("Invalid null ORB");
		}
		orb=theOrb;
		if (theLogger==null) {
			throw new IllegalArgumentException("Invalid null lOGGER");
		}
		log=theLogger;
	}
	
	/**
	 * Start the computation
	 *
	 * @throws Exception if an error happens while connecting
	 */
	public void start() throws Exception {

		if (admin!=null) {
			throw new IllegalArgumentException("Invalid op: call stop before (admin!=null)");
		}

		SwingWorker<AdministratorClient, Void> worker = new SwingWorker<AdministratorClient, Void>() {
			protected AdministratorClient doInBackground() throws Exception {
				AdministratorClient tmp = new AdministratorClient(orb,log);
				try {
					tmp.connectToManager();
				} catch (Exception e) {
					throw new Exception("Exception while connecting to manager",e);
				}
				return tmp;
			}
		};
		worker.execute();

		admin = worker.get();		
		refreshTree();
		admin.addLogListener(this);
	}
	
	/**
	 * Terminate the computation
	 */
	public void stop() {
		admin.addLogListener(null); // Remove the listener
		admin.disconnect();
		admin=null;
	}
	
	/**
	 * Build the tree.
	 * It read the clients, components and containers and add them
	 * to the tree.
	 *
	 */
	public void refreshTree() {
		if (admin==null) {
			setRoot(null);
			if (rootNode!=null) {
				rootNode.removeAllChildren();
			}
			rootNode=null;
		} 
		rootNode= new DefaultMutableTreeNode("root");
		
		managersNode.removeAllChildren();
		clientsNode.removeAllChildren();
		componentsNode.removeAllChildren();
		containersNode.removeAllChildren();
		
		rootNode.add(managersNode);
		// rootNode.add(clientsNode);  // yatagai:hide for now. 
		rootNode.add(containersNode);
		// rootNode.add(componentsNode);  // yatagai: not used any more
		
		Object[] children = new Object[] {
				managersNode,
				clientsNode,
				containersNode,
				componentsNode
		};
		int[] indexes = new int[] {
				rootNode.getIndex(managersNode),
				rootNode.getIndex(clientsNode),
				rootNode.getIndex(containersNode),
				rootNode.getIndex(componentsNode)
		};
		
		fireTreeNodesInserted(rootNode, rootNode.getPath(), indexes, children);
		setRoot(rootNode);
		
		buildManagersNode();
		buildClientsNode();
		buildContainersNode();
		buildComponentsNode();
	}
	
	/**
	 * Build the node of managers
	 */
	private void buildManagersNode() {
		managersNode.add(new DefaultMutableTreeNode(formatManagerLoc(admin.getManagerLoc())));
	}
	
	/**
	 * Build the node of clients by reading the active clients
	 * 
	 * @param clients
	 */
	private void buildClientsNode() {
		ClientInfo[] cliInfos;
		try {
			cliInfos=admin.retrieveClientInfo("*");
		} catch (Exception e) {
			System.err.println("Exception caught while getting the clients: "+e.getMessage());
			e.printStackTrace(System.err);
			clientsNode.removeAllChildren();
			return;
		}
		if (cliInfos==null) {
			return;
		}
		for (ClientInfo c: cliInfos) {
			clientLoggedIn(c);
		}
	}
	
	/**
	 * Build the node of containers by reading the active containers
	 * 
	 */
	private void buildContainersNode() {
		ContainerInfo[] contInfos;
		try {
			contInfos=admin.retrieveContainerInfo("*");
		} catch (Exception e) {
			System.err.println("Exception caught while getting the clients: "+e.getMessage());
			e.printStackTrace(System.err);
			containersNode.removeAllChildren();
			return;
		}
		if (contInfos==null) {
			return;
		}
		for (ContainerInfo c: contInfos) {
			containerLoggedIn(c);
		}
	}
	
	/**
	 * Build the list of components by reading the running components
	 * 
	 * @param container
	 */
	private void buildComponentsNode() {
		ComponentInfo[] components=null;
		try {
			components=admin.retrieveComponentInfo("*");
			
		} catch (Exception e) {
			System.err.println("Exception caught while retrieving active componenst: "+e.getMessage());
			e.printStackTrace(System.err);
		}
		if (components==null) {
			return;
		}
		for (int t=0; t<components.length; t++) {
			componentLoggedIn(components[t]);
		}
	}
	
	/**
	 * Fromat the manager loc in a more readable string
	 * 
	 * @param manLoc The managerLoc (like corbaloc::....)
	 * @return A humager readable string 
	 */
	private String formatManagerLoc(String manLoc) {
		if (manLoc==null || manLoc.length()==0) {
			return "No manager connected";
		}
		StringBuilder temp = new StringBuilder("Manager ");
		if (!manLoc.startsWith("corbaloc::")) {
			temp.append(manLoc);
			return temp.toString();
		}
		temp.append("@ ");
		String[] strs = manLoc.split(":");
		temp.append(strs[2]);
		temp.append(':');
		strs=strs[3].split("/");
		temp.append(strs[0]);
		
		return temp.toString();
	}

	/**
	 * Returns if the given node is the manager
	 *  
	 * @param selNode the node to test
	 * @return true if the node is the manager
	 */
	protected boolean isManagerNode(DefaultMutableTreeNode selNode) {
		return selNode.getUserObject().equals(formatManagerLoc(admin.getManagerLoc()));
	}
	
	/**
	 * Navigate the tree to find a container with the given name or handle.
	 * The search begins from the passed node and scans all the childs
	 * until it finds the node.
	 * The search is done comparing the name with the name of the item
	 * hold by the tree node and its childreen.
	 * If the name param is null, the handle is used.
	 * 
	 * The method is recursive.
	 * 
	 * @param node The root of the subtree to look for the
	 *             node.
	 *             If it is null, the search starts from the root.
	 * @param name The name of the node i.e. the name of
	 *             the client/conatainer/component hold
	 *             by the node
	 *             If null, the handle is used during the search
	 * @param handle The handle of the client/component/container to search
	 *               This param is used only if name is null
	 * @return The first node in the tree with the given mane
	 *         null If such a node does not exist
	 */
	public DefaultMutableTreeNode findNode(DefaultMutableTreeNode node, String name, int handle) {
		if (node==null) {
			// The search starts from the root
			node = (DefaultMutableTreeNode)getRoot();
			if (node==null) {
				// Ops no root found ==> node not found
				return null;
			}
		}
		if (checkNodeContent(node, name, handle)) {
			return node;
		} 
		for (int t=0; t<node.getChildCount(); t++) {
			DefaultMutableTreeNode found=findNode((DefaultMutableTreeNode)node.getChildAt(t),name,handle);
			if (found!=null) {
				return found;
			}
		}
		return null;
	}
	
	/**
	 * Check the content of the passed node.
	 * The check is done taking the content of the Node and checking
	 * if its name (or handle) is equal to the passed parameter.
	 * The comparison is done on the name if the name param is not null,
	 * otherwise on the handle.
	 * 
	 * If the node is the root, name is checked against the string or rootNode
	 * 
	 * @param node The node whose content has to be checked
	 * @param name The name of the component/client/container
	 *             If it is null, it is ignored
	 * @param handle The handle
	 *               (ignored if the name is not null)
	 * @return true if the name or the handle contained in the node
	 *              are equal to the passed parameter
	 *         false otherwise (even if the node is null)
	 */
	private boolean checkNodeContent(DefaultMutableTreeNode node, String name, int handle) {
		if (node==null) {
			return false;
		}
		// Get the user object
		Object content=node.getUserObject();
		if (content==null) {
			return false;
		}
		if (content instanceof TreeContentInfo) {
			TreeContentInfo info=(TreeContentInfo)content;
			if (name!=null) {
				return info.compareName(name);
			} 
			return info.compareHandle(handle);
		} else if (content instanceof String) {
			// The passed node is the root, so check the content of the
			// string it contains against the name param
			// The content is a String for other nodes too (for
			// example the node Clients or Containers; i.e. some of the nodes
			// added to improve the readability of the tree)
			if (name==null) {
				// A String can never be equal to a handle!
				return false; 
			}
			return ((String)content).compareTo(name)==0;
		}
		// The type of the user object is unknown
		System.out.println("Comparison failed because the content type is unknown: "+content.getClass().getName());
		System.out.println("\t"+content.toString());
		return false;
	}
	
	/**
	 * @see LogLevelListener
	 */
	public void clientLoggedIn(ClientInfo clientInfo) {
		if (clientInfo==null) {
			throw new IllegalArgumentException("Invalid null ClientInfo");
		}
		addNode(new TreeClientInfo(clientInfo), clientsNode);
	}

	/**
	 * @see LogLevelListener
	 */
	public void clientLoggedOut(int clientHandle) {
		DefaultMutableTreeNode nodeToRemove = findNode(clientsNode, null, clientHandle);
		if (nodeToRemove!=null) {
			int idx = clientsNode.getIndex(nodeToRemove);
			clientsNode.remove(idx);
			Object[] children = new Object[] { nodeToRemove };
			int[] indexes = new int[] { idx };
			fireTreeNodesRemoved(clientsNode, clientsNode.getPath(), indexes, children);
		}
	}

	/**
	 * @see LogLevelListener
	 */
	public void componentLoggedIn(ComponentInfo compInfo) {
		updateComponent(compInfo);
		TreeComponentInfo info = new TreeComponentInfo(compInfo);
		addNode(info,componentsNode);
	}
	
	/**
	 * @see LogLevelListener
	 */
	public void componentReleased(ComponentInfo compInfo) {
		// Nothing to do because the component is still in use and must be
		// shown in its container
	}

	/**
	 * @see LogLevelListener
	 */
	public void componentLoggedOut(int compHandle) {
		removeComponent(compHandle);
		removeFromComponentSubtree(compHandle);
	}

	/**
	 * @see LogLevelListener
	 */
	public void containerLoggedIn(ContainerInfo contInfo) {
		if (findNode(null,contInfo.name, 0)!=null) {
			System.out.println(contInfo.name+" already in the tree");
		} else {
			// Add the new container
			addNode(new TreeContainerInfo(contInfo), containersNode);
		}
	}

	/**
	 * @see LogLevelListener
	 */
	public void containerLoggedOut(int conthandle) {
		DefaultMutableTreeNode contNode = findNode(containersNode,null, conthandle);
		if (contNode!=null) {
			System.out.println("Found a container node with handle "+conthandle);
			Object[] children = new Object[] { contNode };
			int idx=containersNode.getIndex(contNode);
			int[] indexes = new int[] { idx };
			containersNode.remove(contNode);
			fireTreeNodesRemoved(containersNode, containersNode.getPath(), indexes, children);
		} 
	}
	

	/**
	 * Add a component to its container if it is not already there
	 * 
	 * @param info The component to add to the container
	 */
	private void updateComponent(ComponentInfo info) {
		if (info==null) {
			throw new IllegalArgumentException("Invalid null ComponentInfo");
		}
		// Check if the component is already present (it can happen
		// if it is referenced by more then one client)
		if (findNode(containersNode,info.name, 0)!=null) {
			return;
		}
		// Look for a container with the name of the container of the
		// component
		DefaultMutableTreeNode contNode = findNode(containersNode,info.container_name, 0);
		if (contNode==null) {
			throw new IllegalStateException("Trying to add a component but the container does not exist!");
		}
		addNode(new TreeComponentInfo(info),contNode);
	}
	
	/**
	 * Remove all the occurrencies of the component with the given
	 * handle from the tree
	 * 
	 * @param handle The handle of the component to remove
	 */
	private void removeComponent(int handle) {
		// Look for a node with the given handle
		DefaultMutableTreeNode compNode = findNode(containersNode,null, handle);
		if (compNode==null) {
			// The node is not in the tree: nothing to do
			return;
		}
		DefaultMutableTreeNode parent = (DefaultMutableTreeNode)compNode.getParent();
		int idx = parent.getIndex(compNode);
		parent.remove(compNode);
		
		Object[] children = new Object[] { compNode };
		int[] indexes = new int[] { idx };
		fireTreeNodesRemoved(parent,parent.getPath(), indexes, children);
	}
	
	/**
	 * Remove a node from the subtree of components
	 * 
	 * @param compHandle The handle of the component to remove
	 */
	private void removeFromComponentSubtree(int compHandle) {
		DefaultMutableTreeNode child = findNode(componentsNode, null, compHandle);
		if (child==null) {
			// The component is not in the list
			return;
		}
		int pos = componentsNode.getIndex(child);
		componentsNode.remove(pos);
		int[] indexes = new int[] { pos };
		Object[] children = new Object[] { child };
		fireTreeNodesRemoved(componentsNode, componentsNode.getPath(), indexes, children);
	}
	
	/**
	 * show/hide the components subtree
	 * 
	 * @param show If true the components node is set to visible
	 */
	public void showComponents(boolean show) {
		boolean isComponentsNodeVisible=findNode(null, "Components", 0)!=null;
		if (show) {
			if (isComponentsNodeVisible) {
				return;
			}
			// Add the node
			rootNode.add(componentsNode);
			int[] indexes = new int[] {
					rootNode.getIndex(componentsNode)
			};
			Object[] children = new Object[] { componentsNode };
			fireTreeNodesInserted(rootNode, rootNode.getPath(), indexes, children);
		} else {
			if (!isComponentsNodeVisible) {
				return;
			}
			// Remove the node
			int pos = rootNode.getIndex(componentsNode);
			rootNode.remove(componentsNode);
			int[] indexes = new int[] { pos };
			Object[] children = new Object[] { componentsNode };
			fireTreeNodesRemoved(rootNode, rootNode.getPath(), indexes, children);
		}
	}
	
	/**
	 * show/hide the clients subtree
	 * 
	 * @param show If true the clients node is set to visible
	 */
	public void showClients(boolean show) {
		boolean isCclientsNodeVisible=findNode(null, "Clients", 0)!=null;
		if (show) {
			if (isCclientsNodeVisible) {
				return;
			}
			// Add the node
			rootNode.add(clientsNode);
			int[] indexes = new int[] {
					rootNode.getIndex(clientsNode)
			};
			Object[] children = new Object[] { clientsNode };
			fireTreeNodesInserted(rootNode, rootNode.getPath(), indexes, children);
		} else {
			if (!isCclientsNodeVisible) {
				return;
			}
			// Remove the node
			int pos = rootNode.getIndex(clientsNode);
			rootNode.remove(clientsNode);
			int[] indexes = new int[] { pos };
			Object[] children = new Object[] { clientsNode };
			fireTreeNodesRemoved(rootNode, rootNode.getPath(), indexes, children);
		}
	}
	
	/**
	 * Add an entry to a node.
	 * The new node is inserted ordered by its name.
	 * 
	 * @param newItem The new entry to add
	 * @param node The node where the entry has to be addded
	 * 
	 */
	private void addNode(TreeContentInfo newItem, DefaultMutableTreeNode node) {
		if (newItem==null) {
			throw new IllegalArgumentException("Invalid null item to add");
		}
		if (node==null) {
			throw new IllegalArgumentException("Invalid null node to add the item to");
		}
		DefaultMutableTreeNode newChild = new DefaultMutableTreeNode(newItem);
		// Find the position to insert the new node
		int pos=0;
		boolean found=false;
		if (node.getChildCount()!=0) {
			for (int t=0; t<node.getChildCount(); t++) {
				DefaultMutableTreeNode child = (DefaultMutableTreeNode)node.getChildAt(t);
				String childName = ((TreeContentInfo)child.getUserObject()).getName();
				if (childName.compareTo(newItem.getName())>0) {
					pos=t;
					found=true;
					break;
				}
			}
		} 
		// Add the node in the right position
		if (found) {
			node.insert(newChild, pos);
		} else {
			// The node must be appended at the end of the list
			node.add(newChild);
		}
		// Trigger a refresh of the tree
		int[] indexes = new int[node.getChildCount()];
		for (int t=0; t<indexes.length; t++) {
			indexes[t]=t;
		}
		Object[] children = new Object[indexes.length];
		for (int t=0; t<indexes.length; t++) {
			children[t]=node.getChildAt(t);
		}
		fireTreeStructureChanged(node, node.getPath(), indexes, children);
	}
	
	/**
	 * Return the manager reference 
	 * 
	 * @return The manager reference or
	 *         null if the reference is not available
	 */
	public Manager getManagerRef() {
		if (admin==null) {
			return null;
		}
		return admin.getManagerRef();
	}
	
}
