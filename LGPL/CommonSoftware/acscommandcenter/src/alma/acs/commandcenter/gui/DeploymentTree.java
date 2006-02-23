/*
 * Created on Oct 22, 2003 by mschilli
 */
package alma.acs.commandcenter.gui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Enumeration;
import java.util.logging.Logger;

import javax.swing.AbstractAction;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.event.TreeModelEvent;
import javax.swing.event.TreeModelListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

import si.ijs.maci.ClientInfo;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ContainerInfo;
import alma.acs.commandcenter.meta.Firestarter;
import alma.acs.commandcenter.meta.IMaciSupervisor;
import alma.acs.commandcenter.meta.MaciSupervisorFactory;
import alma.acs.commandcenter.meta.MaciSupervisor.SortingTreeNode;
import alma.acs.util.AcsLocations;

/**
 * @author mschilli
 */
public class DeploymentTree extends JTree {

	protected ContextMenu containerContextMenu;
	protected ContextMenu managerContextMenu;
	protected ContextMenu clientContextMenu;
	protected ContextMenu componentContextMenu;
	protected ContextMenu folderContextMenu;
	protected ContextMenu folderComponentsContextMenu;

	protected TreeEventForwarder treeEventForwarder;
	protected Renderer cellRenderer;

	// assigned on each invokation of show
	protected DefaultMutableTreeNode target;
	// assigned on each invokation of show
	protected MaciSupervisorWrapper supervisor;
	// external logic that can create supervisor instances
	protected DeploymentTreeController ctrl;
	
	public DeploymentTree(DeploymentTreeController ctrl) {

		super(new DefaultMutableTreeNode("Deployment Info"));
		this.setRootVisible(false);
		this.setShowsRootHandles(true);

		// --- cell renderer
		this.setCellRenderer(cellRenderer = new Renderer());

		// --- forward events from the MaciTree-Models
		//     (there will be one for each macimanager) to our own, all-in-one TreeModel
		DefaultTreeModel model = (DefaultTreeModel) this.getModel();
		treeEventForwarder = new TreeEventForwarder(model);

		// --- setup context menus
		clientContextMenu = new ClientContextMenu();
		managerContextMenu = new ManagerContextMenu();
		containerContextMenu = new ContainerContextMenu();
		componentContextMenu = new ComponentContextMenu();
		folderContextMenu = new FolderContextMenu();
		folderComponentsContextMenu = new FolderComponentsContextMenu();

		this.addMouseListener(new MouseAdapter() {

			public void mouseClicked (MouseEvent evt) {
				if (SwingUtilities.isRightMouseButton(evt)) {
					showContextMenu(evt);
				}
			}
		});
		this.getSelectionModel().addTreeSelectionListener(new SelectionListener());
		
		this.ctrl = ctrl;
	}

	/**
	 * @param evt
	 */
	protected void showContextMenu (MouseEvent evt) {

		TreePath targetPath = this.getClosestPathForLocation(evt.getX(), evt.getY());
		setSelectionPath(targetPath);

		if (targetPath.getPathCount() == 1) {
			// clicked on the descriptive super-root node "Deployment Info"
			// that has no function besides looking good
			return;
		}


		// the supervisor (which is in the rootnode) for this subtree
		supervisor = maciSupervisorWrapper(((DefaultMutableTreeNode) targetPath.getPathComponent(1)));
		// the node the mouse was clicked on
		target = (DefaultMutableTreeNode) targetPath.getLastPathComponent();
		Object userObject = target.getUserObject();

		ContextMenu menu;
		if (userObject instanceof MaciSupervisorWrapper) {
			menu = managerContextMenu;
		} else if (userObject instanceof ContainerInfo) {
			menu = containerContextMenu;
		} else if (userObject instanceof ClientInfo) {
			menu = clientContextMenu;
		} else if (userObject instanceof ComponentInfo) {
			menu = componentContextMenu;
		} else if (userObject instanceof alma.acs.commandcenter.meta.MaciSupervisor.FolderInfo) {
			String name = ((alma.acs.commandcenter.meta.MaciSupervisor.FolderInfo) userObject).name;
			if (name.equals("Components"))
				menu = folderComponentsContextMenu;
			else
				menu = folderContextMenu;

		} else {
			return;
		}

		menu.show(this, target, evt.getX(), evt.getY());
	}
	
	// get the supervisor stored the tree node
	protected MaciSupervisorWrapper maciSupervisorWrapper(DefaultMutableTreeNode managerNode) {
		return (MaciSupervisorWrapper)managerNode.getUserObject();
	}
	
	protected DefaultMutableTreeNode getRoot () {
		return (DefaultMutableTreeNode) super.getModel().getRoot();
	}

	protected DefaultTreeModel getTreeModel () {
		return (DefaultTreeModel) super.getModel();
	}

	public void addManager (String managerHost, String managerPort) {
		addManager(AcsLocations.convertToManagerLocation(managerHost, managerPort));
	}

	
	public void addManager (String managerLoc) {
		try {
			
			IMaciSupervisor mrf = ctrl.giveMaciSupervisor(managerLoc);
			addManager(mrf);
			
		} catch (Exception e) {
			String msg = "Failed to connect to manager at " + managerLoc + ".";
			JOptionPane.showMessageDialog(this, msg, "Connect Failed", JOptionPane.INFORMATION_MESSAGE);
		}
	}

	
	public void addManager (IMaciSupervisor mrf) {
		
		// create a wrapper around the given supervisor
		MaciSupervisorWrapper mrfotogen = new MaciSupervisorWrapper(mrf);
				
		// retrieve the tree-structured info that the manager offers as a TreeModel
		DefaultTreeModel maciTree = mrfotogen.getMaciInfo();
		
		// if an exception occurred, the returned tree is null. stop.
		if (maciTree == null) {
			return;
		}
		// steal the root node from the TreeModel (we'll throw away the TreeModel)
		DefaultMutableTreeNode managerNode = (DefaultMutableTreeNode) maciTree.getRoot();
		
		// have events forwarded from the TreeModel to our own all-in-one TreeModel
		treeEventForwarder.addSource(maciTree);
		
		// we replace the current user object (which is just the boring string "Manager")
		// with the MaciSupervisor instance. This will be our reference from now on.
		managerNode.setUserObject(mrfotogen);
		
		// store the additional manager node in our tree
		getRoot().add(managerNode);
		
		// force the model to publish an event
		getTreeModel().nodeStructureChanged(getRoot());
	}

	
	public boolean removeManager (String managerHost, String managerPort, boolean dismissManager) {
		return removeManager(AcsLocations.convertToManagerLocation(managerHost, managerPort), dismissManager);
	}
	
	public boolean removeManager (String managerLoc, boolean dismissManager) {
		DefaultMutableTreeNode managerNode = getManagerNode(managerLoc);

		if (managerNode == null) {
			return false;
		}

		// make the MaciSupervisor forget its manager connection
		if (dismissManager) {
			maciSupervisorWrapper(managerNode).fullpower.dismissManager();
		}
		
		removeNode(managerNode);
		return true;
	}


	public void refreshManager (String managerLoc) {
		DefaultMutableTreeNode managerNode = getManagerNode(managerLoc);

		if (managerNode == null) {
			return;
		}

		refreshManagerNode(managerNode);
	}

	public void refreshManagers () {
		// iterate over root's children
		for (Enumeration en = getRoot().children(); en.hasMoreElements();) {
			refreshManagerNode((DefaultMutableTreeNode) en.nextElement());
		}
	}


	protected void refreshManagerNode (DefaultMutableTreeNode managerNode) {

		// update the tree node model
		maciSupervisorWrapper(managerNode).getMaciInfo();

		// make known by hand again
		getTreeModel().nodeStructureChanged(managerNode);
		// expand containers node
		expandPath(new TreePath(new Object[]{getRoot(), managerNode, managerNode.getChildAt(0)}));
		// expand cliental applications node
		expandPath(new TreePath(new Object[]{getRoot(), managerNode, managerNode.getChildAt(1)}));
	}


	/**
	 * Finds the manager node with the given managerLocation inside
	 */
	protected DefaultMutableTreeNode getManagerNode (String managerLoc) {
		String toFind = managerLoc;

		// iterate over root's children
		for (Enumeration en = getRoot().children(); en.hasMoreElements();) {
			// inspect each child
			DefaultMutableTreeNode managerNode = (DefaultMutableTreeNode) en.nextElement();
			String managerLocation = maciSupervisorWrapper(managerNode).fullpower.getManagerLocation();
			// compare corbalocs
			if (managerLocation.equals(toFind)) {
				return managerNode;
			}
		}
		return null;
	}

	protected void removeNode (DefaultMutableTreeNode node) {

		// let mrfotogen clean up his things
		// (2004-10-11): don't stop the manager here 
		/*
		 * if (node.getUserObject() instanceof MaciSupervisor)
		 *    maciSupervisor(node).stop();
		 */

		DefaultMutableTreeNode parent = (DefaultMutableTreeNode) node.getParent();

		// index needed for event we'll publish subsequently
		int index = parent.getIndex(node);

		// remove node from tree
		parent.remove(node);

		// force the model to publish an event
		// getTreeModel().nodeStructureChanged(getRoot()); // dramatic event, too coarse
		getTreeModel().nodesWereRemoved(parent, new int[]{index}, new Object[]{node});
	}

	/**
	 * Rearranges the node's children using the InfoDetail key.
	 */
	protected void sortNode (DefaultMutableTreeNode node, String key) {
		// the nodes created by MaciSupervisor have a special capability: they can sort
		// their kids
		alma.acs.commandcenter.meta.MaciSupervisor.SortingTreeNode casted = (alma.acs.commandcenter.meta.MaciSupervisor.SortingTreeNode) node;
		casted.sortChildrenByInfoDetail(key);
		// inform the model that something has changed
		getTreeModel().nodeStructureChanged(casted);
	}


	/**
	 * Signals to the user that an action takes longer.
	 */
	protected void setBusy (boolean b) {
		if (b) {
			this.setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
		} else {
			this.setCursor(Cursor.getDefaultCursor());
		}
	}


	/**
	 * @param caption
	 * @return the input or <code>null</code> if cancelled
	 */
	protected String showInputDialog (String caption) {
		return JOptionPane.showInputDialog(this, caption);
	}


	/**
	 *  
	 */
	protected void seemsManagerIsDown (MaciSupervisorWrapper w) {

		String managerLoc = w.fullpower.getManagerLocation();
		String msg = "Seems the manager at " + managerLoc + " is down.\n" + "Ok to remove it from the View?";
		int answer = JOptionPane.showConfirmDialog(this, msg, "Communication Failed", JOptionPane.YES_NO_OPTION);

		if (answer == JOptionPane.OK_OPTION) {
			removeManager(managerLoc, false);
		}
	}


	protected void seemsManagerHasChangedOrHasCutConnection (MaciSupervisorWrapper w) {

		String managerLoc = w.fullpower.getManagerLocation();
		String msg = "Seems the manager at " + managerLoc + " has changed or\n" + "has cut the connection. Will try to reconnect.";
		JOptionPane.showMessageDialog(this, msg, "Communication Failed", JOptionPane.INFORMATION_MESSAGE);

		// need to call stop() although it will
		// provoke another NO_PERMISSION exception
		w.stop();

		// dismiss old supervisor...
		removeManager(managerLoc, false);

		// ...and re-add it
		addManager(managerLoc);
	}


	/**
	 *  
	 */
	protected void seemsWeHaveDisconnected (MaciSupervisorWrapper w) {

		String managerLoc = w.fullpower.getManagerLocation();
		String msg = "I have no connection to the manager at " + managerLoc + ".\n" + "Do you want to connect now?";
		int answer = JOptionPane.showConfirmDialog(this, msg, "Communication Failed", JOptionPane.YES_NO_OPTION);

		if (answer == JOptionPane.OK_OPTION) {
			addManager(managerLoc);
		}
	}



	//
	// =========================================================
	// ===================== Inner Types =======================
	// =========================================================
	//

	protected class MaciSupervisorWrapper {

		protected IMaciSupervisor fullpower;

		protected MaciSupervisorWrapper(IMaciSupervisor orig) {
			this.fullpower = orig;
		}

		protected void handleException (Throwable exc) {

			if (exc instanceof org.omg.CORBA.NO_PERMISSION) {
				seemsManagerHasChangedOrHasCutConnection(this);

			} else if (exc instanceof IllegalStateException) {
				seemsWeHaveDisconnected(this);

			} else if (exc instanceof org.omg.CORBA.OBJECT_NOT_EXIST) {
				seemsManagerIsDown(this);

			} else if (exc instanceof org.omg.CORBA.TRANSIENT) {
				seemsManagerIsDown(this);

			} else {
				System.err.println("ERROR: couldn't refresh manager info for unknown reason.");
				System.err.println("Please report the following lines to the Acs Team: "+exc);
				System.err.println("---------------------------");
				exc.printStackTrace(System.err);
				System.err.println("---------------------------");
			}
				
		}

		public org.omg.CORBA.Object getComponent (String curl) {
			try {
				return fullpower.getComponent(curl);

			} catch (Exception exc) {
				handleException(exc);
				return null;
			}
		}

		
		public void forceReleaseComponent (String curl) {
			try {
				fullpower.forceReleaseComponent(curl);

			} catch (Exception exc) {
				handleException(exc);
			}
		}

		public void disconnectContainer (String name) {
			try {
				fullpower.disconnectContainer(name);
				
			} catch (Exception exc) {
				handleException(exc);
			}
		}

		public DefaultTreeModel getMaciInfo () throws RuntimeException {
			try {
				// throws RuntimeExceptions that wrap the actual exceptions
				return fullpower.getMaciInfo();

			} catch (RuntimeException exc) {
				handleException(exc.getCause());
				return null;
			}
		}

		public void logoutClient (ClientInfo info) {
			try {
				fullpower.logoutClient(info);

			} catch (Exception exc) {
				handleException(exc);
			}
		}

		public void logoutContainer (ContainerInfo info) {
			try {
				fullpower.logoutContainer(info);

			} catch (Exception exc) {
				handleException(exc);
			}
		}

		public void pingContainer (String name) {
			try {
				fullpower.pingContainer(name);

			} catch (Exception exc) {
				handleException(exc);
			}
		}

		public ClientInfo[] retrieveClientInfo () {
			try {
				return fullpower.retrieveClientInfo();

			} catch (Exception exc) {
				handleException(exc);
				return new ClientInfo[0];
			}
		}

		public ComponentInfo[] retrieveComponentInfo (int[] cobHandles, String name_wildcard, String type_wildcard,
				boolean active_only) {
			try {
				return fullpower.retrieveComponentInfo(cobHandles, name_wildcard, type_wildcard, active_only);

			} catch (Exception exc) {
				handleException(exc);
				return new ComponentInfo[0];
			}
		}

		public ContainerInfo[] retrieveContainerInfo (String name_wildcard) {
			try {
				return fullpower.retrieveContainerInfo(name_wildcard);

			} catch (Exception exc) {
				handleException(exc);
				return new ContainerInfo[0];
			}
		}

		public void shutdownContainer (String name) {
			try {
				fullpower.shutdownContainer(name);

			} catch (Exception exc) {
				handleException(exc);
			}
		}

		public void shutdownContainers () {
			try {
				fullpower.shutdownContainers();

			} catch (Exception exc) {
				handleException(exc);
			}
		}

		public void shutdownManager () {
			try {
				fullpower.shutdownManager();

			} catch (Exception exc) {
				handleException(exc);
			}
		}

		public void stop () {
			try {
				fullpower.stop();

			} catch (Exception exc) {
				handleException(exc);
			}
		}
	}


	/**
	 * Provides appropriate treatment for all the node types.
	 */
	protected class Renderer extends DefaultTreeCellRenderer {

		Border bluelineBorder = new LineBorder(Color.blue, 1);
		Border graylineBorder = new LineBorder(Color.gray, 1);
		Border emptyBorder = new EmptyBorder(1, 1, 1, 1);
		Color grayBackground = Color.lightGray;
		
		int[] currentlySelectedHandles = new int[]{};

		public Component getTreeCellRendererComponent (JTree tree, Object value, boolean selected, boolean expanded, boolean leaf,
				int row, boolean hasFocus) {

			super.getTreeCellRendererComponent (tree, value, selected, expanded, leaf, row, hasFocus);

			this.setIcon(null);
			
			// ==== caption ====

			String text; /* compiler will optimize the string operations */

			DefaultMutableTreeNode node = (DefaultMutableTreeNode) value;
			Object userObject = node.getUserObject();

			if (userObject == null) {
				text = "empty node (strange)";

			} else if (userObject instanceof MaciSupervisorWrapper) {
				String mgrLoc = maciSupervisorWrapper(node).fullpower.getManagerLocation();
				String[] mgr = AcsLocations.convert(mgrLoc);
				text = "Manager on " + mgr[0] + ", port " + mgr[1];

			} else if (userObject instanceof ContainerInfo) {
				ContainerInfo casted = (ContainerInfo) userObject;
				int nCOBs = (casted.components != null) ? casted.components.length : -1;
				text = "'" + casted.name + "' [id " + casted.h + "] : " + nCOBs + " component" + ((nCOBs == 1) ? "" : "s");

			} else if (userObject instanceof ClientInfo) {
				ClientInfo casted = (ClientInfo) userObject;
				text = "'" + casted.name + "' [id " + casted.h + "]";

			} else if (userObject instanceof ComponentInfo) {
				ComponentInfo casted = (ComponentInfo) userObject;
				int nClients = (casted.clients != null) ? casted.clients.length : -1;
				text = "'" + casted.name + "' [id " + casted.h + "] : " + nClients + " client" + ((nClients == 1) ? "" : "s");

			} else if (userObject instanceof alma.acs.commandcenter.meta.MaciSupervisor.InfoDetail) {
				/*
				 * InfoDetail nodes are hanging below Components and Clients and Containers.
				 * Thus, they all share the same visualization of, for instance, the info
				 * detail "reference"
				 */
				alma.acs.commandcenter.meta.MaciSupervisor.InfoDetail casted = (alma.acs.commandcenter.meta.MaciSupervisor.InfoDetail) userObject;

				if (casted.key.equals("container_name"))
					text = "needs: " + casted.value;
				else if (casted.key.equals("reference"))
					text = "reference: " + (casted.value.equals("null") ? "invalid" : "ok");
				else if (casted.key.equals("container"))
					text = "hosted by: " + (casted.value.equals("0") ? "no container" : casted.value);
				else if (casted.key.equals("access"))
					text = "access: " + (casted.value.equals("0") ? "unrestricted" : casted.value);
				else
					text = casted.key + ": " + casted.value;


			} else if (userObject instanceof alma.acs.commandcenter.meta.MaciSupervisor.FolderInfo) {
				text = ((alma.acs.commandcenter.meta.MaciSupervisor.FolderInfo) userObject).name;
				text = text + " ("+node.getChildCount()+")"; 

			} else
				text = String.valueOf(node);

			this.setText(text);


			// ==== selection ====

			/* We want the real selection blue, and the "deputy"-selections gray. 
			 * Since our superclass implementation determines the selection background
			 * in paint() and not in getRendererComponent(), we need to set the
			 * "backgroundNonSelectionColor" property which is getting evaluated by paint()
			 */
			
			Border border = null;

			if (selected) {
				border = bluelineBorder;
				if (node instanceof SortingTreeNode) {
					currentlySelectedHandles = ((SortingTreeNode) node).representedHandles;
				}
			} else {
				
				Color backgroundNonSelectionColor = null;
				
				if (node instanceof SortingTreeNode) {
					SortingTreeNode casted = (SortingTreeNode) node;
					search: for (int i = 0; i < currentlySelectedHandles.length; i++) {
						for (int j = 0; j < casted.representedHandles.length; j++) {
							if (currentlySelectedHandles[i] == casted.representedHandles[j]) {
								border = graylineBorder;
								backgroundNonSelectionColor = grayBackground;
								break search;
							}
						}
					}
				}

				// sets the non-selection background to either "null" or "gray"
				this.setBackgroundNonSelectionColor(backgroundNonSelectionColor);

				if (border == null)
					border = emptyBorder;
			}

			this.setBorder(border);



			// === done ===

			return this;
		}
	}


	/**
	 * Listens on selection-changes in the tree to trigger a repaint-event.
	 * 
	 * @author mschilli
	 */
	protected class SelectionListener implements TreeSelectionListener {

		public void valueChanged (TreeSelectionEvent e) {
			Object node = e.getPath().getLastPathComponent();
			if (node instanceof SortingTreeNode)
				cellRenderer.currentlySelectedHandles = ((SortingTreeNode) node).representedHandles;
			repaint();
		}

	}

	/**
	 * Forwards events from one or more TreeModels to a TreeModel.
	 * 
	 * This will accept events from any TreeModels it is registered with. It will then
	 * raise that event on the target TreeModel.
	 * 
	 * @author mschilli
	 */
	protected class TreeEventForwarder implements TreeModelListener {

		protected DefaultTreeModel forwardTarget;

		// --- construction / setup ---

		public TreeEventForwarder(DefaultTreeModel forwardTarget) {
			this.forwardTarget = forwardTarget;
		}

		// register as a listener with specified TreeModel
		public void addSource (TreeModel source) {
			source.addTreeModelListener(this);
		}

		// --- forwarding logic ---

		synchronized public void treeStructureChanged (TreeModelEvent e) {
			forwardTarget.nodeStructureChanged((TreeNode) e.getTreePath().getLastPathComponent());
		}

		public void treeNodesChanged (TreeModelEvent e) {}

		public void treeNodesInserted (TreeModelEvent e) {}

		public void treeNodesRemoved (TreeModelEvent e) {}
	}

	// 
	// =================== Context Menus =====================
	// 


	protected class ContextMenu extends JPopupMenu {

		protected ContextMenu() {
		// (Apr 15, 2004) msc: commented out
		// When one removes a folder like "Containers", it disappears unrecoverable
		// When one removes an element like "bilboContainer", it reappears with the next
		// Maci-Refresh
		// This asymetry is not user-friendly, the feature by itself is not so useful
		// anyway.
		// It has now gone to ManagerContextMenu.
		//this.add(new RemoveFromViewAction());
		}

		public void show (DeploymentTree tree, DefaultMutableTreeNode target, int x, int y) {
			super.show(tree, x, y);
		}

	}

	protected class ManagerContextMenu extends ContextMenu {

		protected ManagerContextMenu() {
			super();
			this.add(new ManagerRefreshAction());

			this.add(new JPopupMenu.Separator());
			this.add(new ManagerShutdownAction());

			this.add(new JPopupMenu.Separator());
			this.add(new RemoveFromViewAction());
		}

	}

	protected class FolderContextMenu extends ContextMenu {

		protected FolderContextMenu() {
			super();
			this.add(new FolderSortByNameAction());
		}

	}

	protected class FolderComponentsContextMenu extends FolderContextMenu {

		protected FolderComponentsContextMenu() {
			super();
			this.add(new FolderSortByContainerNameAction());
		}

	}

	protected class ContainerContextMenu extends ContextMenu {

		protected ContainerContextMenu() {
			super();
			this.add(new ContainerPingAction());
			this.add(new ContainerMessageAction());
			this.add(new ContainerDisconnectAction());
			this.add(new ContainerShutdownAction());

			this.add(new JPopupMenu.Separator());
			this.add(new ContainerLogoutAction());
		}

	}

	protected class ClientContextMenu extends ContextMenu {

		protected ClientContextMenu() {
			super();
			this.add(new ClientPingAction());
			this.add(new ClientMessageAction());
			this.add(new ClientDisconnectAction());

			this.add(new JPopupMenu.Separator());
			this.add(new ClientLogoutAction());
		}

	}

	protected class ComponentContextMenu extends ContextMenu {

		protected ComponentContextMenu() {
			super();
			this.add(new ComponentActivateAction());

			this.add(new JPopupMenu.Separator());
			this.add(new ComponentForceReleaseAction());
		}

	}

	// 
	// ======================= Actions =========================
	// 

	/**
	 * Performs the work to be done within the event-dispatcher thread
	 */
	protected abstract class ImmediateAction extends AbstractAction {

		protected ImmediateAction(String name) {
			super(name);
		}

		final public void actionPerformed (ActionEvent e) {
			actionPerformed();
		}

		protected abstract void actionPerformed ();
	}

	/**
	 * Performs the work to be done NOT within event-dispatcher thread but within an extra
	 * thread. Used for most actions in the context menus.
	 */
	protected abstract class DelayedAction extends AbstractAction {

		protected DelayedAction(String name) {
			super(name);
		}

		final public void actionPerformed (ActionEvent e) {
			new Thread() { // TODO(msc): re-use threads

				public void run () {
					setBusy(true);
					try {
						actionPerformed();
					} catch (RuntimeException e) {
						throw e;
					} finally {
						setBusy(false);
					}
				}
			}.start();
		}

		protected abstract void actionPerformed ();
	}

	protected class RemoveFromViewAction extends ImmediateAction {

		protected RemoveFromViewAction() {
			super("Remove from View");
		}

		public void actionPerformed () {
			removeNode(target);
		}
	}

	protected class ManagerRefreshAction extends DelayedAction {

		protected ManagerRefreshAction() {
			super("Refresh Info");
		}

		public void actionPerformed () {
			refreshManagerNode(target);
		}

	}

	protected class ManagerShutdownAction extends DelayedAction {

		protected ManagerShutdownAction() {
			super("Send Shutdown Request");
		}

		public void actionPerformed () {
			supervisor.shutdownManager();
		}
	}

	protected class FolderSortByNameAction extends DelayedAction {

		protected FolderSortByNameAction() {
			super("Sort by name");
		}

		public void actionPerformed () {
			sortNode(target, "name");
		}
	}

	protected class FolderSortByContainerNameAction extends DelayedAction {

		protected FolderSortByContainerNameAction() {
			super("Sort by container needed");
		}

		public void actionPerformed () {
			sortNode(target, "container_name");
		}
	}

	protected class ContainerPingAction extends DelayedAction {

		protected ContainerPingAction() {
			super("Send Ping Request");
		}

		public void actionPerformed () {
			supervisor.fullpower.pingContainer((ContainerInfo) target.getUserObject());
		}
	}

	protected class ContainerShutdownAction extends DelayedAction {

		protected ContainerShutdownAction() {
			super("Send Shutdown Request");
		}

		public void actionPerformed () {
			supervisor.fullpower.shutdownContainer((ContainerInfo) target.getUserObject());
		}
	}

	protected class ContainerMessageAction extends DelayedAction {

		protected ContainerMessageAction() {
			super("Send Message...");
		}

		public void actionPerformed () {
			String msg = showInputDialog("Enter message text:");
			if (msg != null)
				supervisor.fullpower.sendMessageToContainer((ContainerInfo) target.getUserObject(),
						IMaciSupervisor.MSG_INFORMATION, msg);
		}
	}

	protected class ClientPingAction extends DelayedAction {

		protected ClientPingAction() {
			super("Send Ping Request");
		}

		public void actionPerformed () {
			supervisor.fullpower.pingClient((ClientInfo) target.getUserObject());
		}
	}

	protected class ContainerDisconnectAction extends DelayedAction {

		protected ContainerDisconnectAction() {
			super("Send Disconnect Request");
		}

		public void actionPerformed () {
			supervisor.fullpower.disconnectContainer((ContainerInfo) target.getUserObject());
		}
	}

	protected class ClientDisconnectAction extends DelayedAction {

		protected ClientDisconnectAction() {
			super("Send Disconnect Request");
		}

		public void actionPerformed () {
			supervisor.fullpower.disconnectClient((ClientInfo) target.getUserObject());
		}
	}

	protected class ClientMessageAction extends DelayedAction {

		protected ClientMessageAction() {
			super("Send Message...");
		}

		public void actionPerformed () {
			String msg = showInputDialog("Enter message text:");
			if (msg != null)
				supervisor.fullpower.sendMessageToClient((ClientInfo) target.getUserObject(),
						IMaciSupervisor.MSG_INFORMATION, msg);
		}
	}

	protected class ContainerLogoutAction extends DelayedAction {

		protected ContainerLogoutAction() {
			super("Have logged out by Manager");
		}

		public void actionPerformed () {
			supervisor.logoutContainer((ContainerInfo) target.getUserObject());
		}
	}

	protected class ClientLogoutAction extends DelayedAction {

		protected ClientLogoutAction() {
			super("Have logged out by Manager");
		}

		public void actionPerformed () {
			supervisor.logoutClient((ClientInfo) target.getUserObject());
		}
	}

	protected class ComponentActivateAction extends DelayedAction {

		protected ComponentActivateAction() {
			super("Have activated by Manager");
		}

		public void actionPerformed () {
			String name = ((ComponentInfo) target.getUserObject()).name;
			supervisor.getComponent(name);
		}
	}

	protected class ComponentForceReleaseAction extends DelayedAction {

		protected ComponentForceReleaseAction() {
			super("Have system-widely deactivated by Manager");
		}

		public void actionPerformed () {
			String name = ((ComponentInfo) target.getUserObject()).name;
			supervisor.forceReleaseComponent(name);
		}
	}

	
}

///////////////////////////////////////////////////// //
/// API / //
///////////////////////////////////////////////////// //

///////////////////////////////////////////////////// //
/// Internal / //
///////////////////////////////////////////////////// //
