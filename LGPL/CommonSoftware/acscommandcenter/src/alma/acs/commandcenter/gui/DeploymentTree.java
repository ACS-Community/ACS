/*
 * Created on Oct 22, 2003 by mschilli
 */
package alma.acs.commandcenter.gui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.Vector;

import javax.swing.AbstractAction;
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
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

import si.ijs.maci.ClientInfo;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ContainerInfo;
import alma.acs.commandcenter.meta.GuiMaciSupervisor;
import alma.acs.commandcenter.meta.IMaciSupervisor;
import alma.acs.commandcenter.meta.MaciInfo;
import alma.acs.commandcenter.meta.Firestarter.OrbInitException;
import alma.acs.commandcenter.meta.IMaciSupervisor.CannotRetrieveManagerException;
import alma.acs.commandcenter.meta.IMaciSupervisor.CorbaNoPermissionException;
import alma.acs.commandcenter.meta.IMaciSupervisor.CorbaNotExistException;
import alma.acs.commandcenter.meta.IMaciSupervisor.CorbaTransientException;
import alma.acs.commandcenter.meta.IMaciSupervisor.NotConnectedToManagerException;
import alma.acs.commandcenter.meta.IMaciSupervisor.UnknownErrorException;
import alma.acs.commandcenter.meta.MaciInfo.FolderInfo;
import alma.acs.commandcenter.meta.MaciInfo.InfoDetail;
import alma.acs.commandcenter.meta.MaciInfo.SortingTreeNode;
import alma.acs.util.AcsLocations;
import alma.maciErrType.CannotGetComponentEx;
import alma.maciErrType.ComponentConfigurationNotFoundEx;
import alma.maciErrType.ComponentNotAlreadyActivatedEx;
import alma.maciErrType.NoPermissionEx;

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
	protected List<ModelConverter> modelConverters;
	protected Renderer cellRenderer;


	// assigned on each invokation of show
	protected DefaultMutableTreeNode target;
	// assigned on each invokation of show
	protected GuiMaciSupervisor selectedSupervisor;
	// external logic that can create supervisor instances
	protected DeploymentTreeController ctrl;

	public DeploymentTree(DeploymentTreeController ctrl) {

		super(new DefaultMutableTreeNode("Deployment Info"));
		this.setRootVisible(false);
		this.setShowsRootHandles(true);

		// --- cell renderer
		this.setCellRenderer(cellRenderer = new Renderer());

		// --- forward events from the MaciTree-Models
		// (there will be one for each macimanager) to our own, all-in-one TreeModel
		DefaultTreeModel model = (DefaultTreeModel) this.getModel();
		treeEventForwarder = new TreeEventForwarder(model);

		modelConverters = new Vector<ModelConverter>();

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
		selectedSupervisor = maciSupervisor(((DefaultMutableTreeNode) targetPath.getPathComponent(1)));
		// the node the mouse was clicked on
		target = (DefaultMutableTreeNode) targetPath.getLastPathComponent();
		Object userObject = target.getUserObject();

		ContextMenu menu;
		if (userObject instanceof IMaciSupervisor) {
			menu = managerContextMenu;
		} else if (userObject instanceof ContainerInfo) {
			menu = containerContextMenu;
		} else if (userObject instanceof ClientInfo) {
			menu = clientContextMenu;
		} else if (userObject instanceof ComponentInfo) {
			menu = componentContextMenu;
		} else if (userObject instanceof FolderInfo) {
			String name = ((FolderInfo) userObject).name;
			if (name.equals("Components"))
				menu = folderComponentsContextMenu;
			else
				menu = folderContextMenu;

		} else {
			return;
		}

		menu.show(this, target, evt.getX(), evt.getY());
	}

	// get the supervisor stored in the tree node
	protected GuiMaciSupervisor maciSupervisor (DefaultMutableTreeNode managerNode) {
		return (GuiMaciSupervisor) managerNode.getUserObject();
	}

	protected DefaultMutableTreeNode getRoot () {
		return (DefaultMutableTreeNode) super.getModel().getRoot();
	}

	protected DefaultTreeModel getTreeModel () {
		return (DefaultTreeModel) super.getModel();
	}


	protected GuiMaciSupervisor getMaciSupervisor (String managerLoc) throws OrbInitException {
		GuiMaciSupervisor mrf = ctrl.giveMaciSupervisor(managerLoc);
		mrf.setConnectsAutomatically(false);
		return mrf;
	}

	protected void startAndAddMaciSupervisor (GuiMaciSupervisor mrf) throws NoPermissionEx, CannotRetrieveManagerException,
			CorbaTransientException, CorbaNotExistException, UnknownErrorException {
		mrf.start(); // start is smart - can be called repeatedly
		addManager(mrf);
	}

	public void addManager (GuiMaciSupervisor mrfotogen) throws NoPermissionEx, CorbaTransientException, CorbaNotExistException,
			UnknownErrorException {

		// retrieve the tree-structured info that the manager offers as a TreeModel
		MaciInfo maciInfo = null;
		try {
			maciInfo = mrfotogen.getMaciInfo();

		} catch (NotConnectedToManagerException exc) {
			// really shouldn't happen (the passed in supervisor is started)
			throw new AssertionError("This supervisor has obviously not been started: " + mrfotogen);
		}

		// no exception occurred, we have valid info

		// the root of maciInfo is just a node with the boring string "Manager",
		// the root of guiInfo will be a node holding the MaciSupervisor.
		DefaultMutableTreeNode managerNode = new DefaultMutableTreeNode(mrfotogen);
		DefaultTreeModel guiInfo = new DefaultTreeModel(managerNode);
		ModelConverter mc = new ModelConverter(maciInfo, guiInfo);
		modelConverters.add(mc);
		maciInfo.addTreeModelListener(mc);

		// have events forwarded from the TreeModel to our own all-in-one TreeModel
		treeEventForwarder.addSource(guiInfo);

		// store the additional manager node in our tree
		getTreeModel().insertNodeInto(managerNode, getRoot(), getRoot().getChildCount());
		// getRoot().add(managerNode);


		// populate guitree, will make the tree display
		mc.convertCompleteModel();
	}


	public boolean removeManager (String managerLoc, boolean dismissManager) {
		DefaultMutableTreeNode managerNode = getManagerNode(managerLoc);

		if (managerNode == null) {
			return false;
		}

		GuiMaciSupervisor ms = maciSupervisor(managerNode);

		// make the MaciSupervisor forget its manager connection
		if (dismissManager) {
			ms.dismissManager();
		}

		for (ModelConverter mc : modelConverters) {
			if (maciSupervisor(mc.managerNode()) == ms) {
				mc.sourceModel.removeTreeModelListener(mc);
				modelConverters.remove(mc);
				break;
			}
		}

		removeNode(managerNode);
		return true;
	}


	public void refreshManagers () {
		// iterate over root's children
		for (Enumeration en = getRoot().children(); en.hasMoreElements();) {
			DefaultMutableTreeNode managerNode = (DefaultMutableTreeNode) en.nextElement();
			shieldedRefreshManager(maciSupervisor(managerNode));
		}
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
			String managerLocation = maciSupervisor(managerNode).getManagerLocation();
			// compare corbalocs
			if (managerLocation.equals(toFind)) {
				return managerNode;
			}
		}
		return null;
	}


	/**
	 * Little helper for the renderer. Just because it seems nice for the user. The
	 * specified node must contain a ClientInfo user object.
	 */
	protected boolean isMyself (DefaultMutableTreeNode node) {
		try {
			// find manager node
			DefaultMutableTreeNode n = node;
			do {
				n = (DefaultMutableTreeNode) n.getParent();
			} while (!(n.getUserObject() instanceof IMaciSupervisor));
			// compare handles
			ClientInfo info = (ClientInfo) node.getUserObject();
			return (maciSupervisor(n).myMaciHandle() == info.h);

		} catch (Exception e) {
			// instead of making the above code rock-stable, let's do it cheap
			return false;
		}
	}

	/**
	 * Removes an arbitrary node from the gui
	 */
	protected void removeNode (DefaultMutableTreeNode node) {

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
		// the nodes created by MaciSupervisor have a special capability:
		// they can sort their kids
		SortingTreeNode casted = (SortingTreeNode) node;
		casted.sortChildrenByInfoDetail(key);
		// inform the model that something has changed
		getTreeModel().nodeStructureChanged(casted);
	}


	/**
	 * Signals to the user that an action takes longer.
	 */
	protected void setBusy (boolean b) {
		int cursor = (b) ? Cursor.WAIT_CURSOR : Cursor.DEFAULT_CURSOR;
		this.setCursor(Cursor.getPredefinedCursor(cursor));
	}



	//
	// =========================================================
	// ===================== Inner Types =======================
	// =========================================================
	//



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

			super.getTreeCellRendererComponent(tree, value, selected, expanded, leaf, row, hasFocus);

			this.setIcon(null);

			// ==== caption ====

			String text; /* compiler will optimize the string operations */

			DefaultMutableTreeNode node = (DefaultMutableTreeNode) value;
			Object userObject = node.getUserObject();

			if (userObject == null) {
				text = "empty node (strange)";

			} else if (userObject instanceof IMaciSupervisor) {
				String mgrLoc = maciSupervisor(node).getManagerLocation();
				String[] mgr = AcsLocations.convert(mgrLoc);
				text = "Manager on " + mgr[0] + ", port " + mgr[1];

			} else if (userObject instanceof ContainerInfo) {
				ContainerInfo casted = (ContainerInfo) userObject;
				int nCOBs = (casted.components != null) ? casted.components.length : -1;
				text = "'" + casted.name + "' [id " + casted.h + "] : " + nCOBs + " component" + ((nCOBs == 1) ? "" : "s");

			} else if (userObject instanceof ClientInfo) {
				ClientInfo casted = (ClientInfo) userObject;
				text = "'" + casted.name + "' [id " + casted.h + "]";
				text += (isMyself(node)) ? " (myself)" : "";

			} else if (userObject instanceof ComponentInfo) {
				ComponentInfo casted = (ComponentInfo) userObject;
				int nClients = (casted.clients != null) ? casted.clients.length : -1;
				text = "'" + casted.name + "' [id " + casted.h + "] : " + nClients + " client" + ((nClients == 1) ? "" : "s");

			} else if (userObject instanceof InfoDetail) {
				/*
				 * InfoDetail nodes are hanging below Components and Clients and Containers.
				 * Thus, they all share the same visualization of, for instance, the info
				 * detail "reference"
				 */
				InfoDetail casted = (InfoDetail) userObject;

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


			} else if (userObject instanceof FolderInfo) {
				text = ((FolderInfo) userObject).name;
				text = text + " (" + node.getChildCount() + ")";

			} else
				text = String.valueOf(node);

			this.setText(text);


			// ==== selection ====

			/*
			 * We want the real selection blue, and the "deputy"-selections gray. Since our
			 * superclass implementation determines the selection background in paint() and
			 * not in getRendererComponent(), we need to set the
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
	 */
	protected class SelectionListener implements TreeSelectionListener {

		public void valueChanged (TreeSelectionEvent e) {
			Object node = e.getPath().getLastPathComponent();
			if (node instanceof SortingTreeNode)
				cellRenderer.currentlySelectedHandles = ((SortingTreeNode) node).representedHandles;
			repaint();
		}

	}

	protected class ModelConverter implements TreeModelListener {

		protected MaciInfo sourceModel;
		protected DefaultTreeModel targetModel;

		public ModelConverter(MaciInfo sourceModel, DefaultTreeModel targetModel) {
			this.sourceModel = sourceModel;
			this.targetModel = targetModel;
		}

		public DefaultMutableTreeNode managerNode () {
			return (DefaultMutableTreeNode) targetModel.getRoot();
		}

		// --- conversion logic ---

		public void treeNodesChanged (TreeModelEvent e) {}

		public void treeNodesInserted (TreeModelEvent e) {}

		public void treeNodesRemoved (TreeModelEvent e) {}

		public void treeStructureChanged (TreeModelEvent e) {
			convertCompleteModel();
		}

		protected void convertCompleteModel () {
			/* System.err.println("MC: "+Thread.currentThread().getName()); */

			final DefaultMutableTreeNode src = (DefaultMutableTreeNode) sourceModel.getRoot();
			final DefaultMutableTreeNode trg = (DefaultMutableTreeNode) targetModel.getRoot();

			/*
			 * the model update will replace all the nodes: the tree will collapse, will jump
			 * to first row, will forget the selection - quite annoying for the user.
			 * therefore, we'll try to restore partially what the tree looked like
			 */
			final Rectangle viewBeforeUpdate = getVisibleRect();

			// do a complete conversion of the whole model
			Runnable r = new Runnable() {

				public void run () {
					trg.removeAllChildren();
					convert(src, trg);
					targetModel.nodeStructureChanged(trg);
				}
			};

			/*
			 * this is the main point of the whole model conversion business: modify the
			 * jtree's tree model only in the swing thread. the maci info is modified by the
			 * supervisor in whatever thread it uses. we listen to those changes and transfer
			 * them to the tree's underlying tree model.... using the swing thread...
			 */
			runInSwingThread(r);

			r = new Runnable() {

				public void run () {
					expandPath(new TreePath(new Object[]{getRoot(), trg, trg.getChildAt(0)}));
					expandPath(new TreePath(new Object[]{getRoot(), trg, trg.getChildAt(1)}));
					expandPath(new TreePath(new Object[]{getRoot(), trg, trg.getChildAt(2)}));
					scrollRectToVisible(viewBeforeUpdate);
				}
			};
			runInSwingThread(r);
		}

		@SuppressWarnings("unchecked")
		// JDK library is not type-safe
		private void convert (DefaultMutableTreeNode src, DefaultMutableTreeNode trg) {
			List<DefaultMutableTreeNode> children = Collections.list((Enumeration<DefaultMutableTreeNode>) src.children());
			for (DefaultMutableTreeNode subSrc : children) {
				DefaultMutableTreeNode subTrg = (DefaultMutableTreeNode) subSrc.clone();
				trg.add(subTrg);
				convert(subSrc, subTrg);
			}
		}

		// so much hassle... see above
		private void runInSwingThread (Runnable r) {
			if (SwingUtilities.isEventDispatchThread())
				r.run();
			else
				try {
					SwingUtilities.invokeAndWait(r);
				} catch (Exception exc) {
					// exc.printStackTrace();
				}
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
			/* System.err.println("TEF: "+Thread.currentThread().getName()); */
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
		/*
		 * (Apr 15, 2004) msc: commented out When one removes a folder like "Containers", it
		 * disappears unrecoverable When one removes an element like "bilboContainer", it
		 * reappears with the next Maci-Refresh This asymetry is not user-friendly, the
		 * feature by itself is not so useful anyway. It has now gone to ManagerContextMenu.
		 * 
		 * this.add(new RemoveFromViewAction());
		 */
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
			this.add(new ManagerPingAction());
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
			this.add(new ComponentRequestAction());
			this.add(new ComponentReleaseAction());

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
			try {
				actionPerformed();

			} catch (Exception exc) {
				/*
				 * This catch-clause will rarely be executed: the menu-item actions mostly use
				 * the shielded API which catches exceptions way before
				 */
				ErrorBox.showErrorDialog(DeploymentTree.this, "\"" + getValue(NAME) + "\" failed", exc);
			}
		}

		protected abstract void actionPerformed ();
	}

	/**
	 * Performs the work to be done delayed but also within the event-dispatcher thread.
	 * Used for most actions in the context menus.
	 */
	protected abstract class DelayedAction extends AbstractAction {

		protected DelayedAction(String name) {
			super(name);
		}

		final public void actionPerformed (ActionEvent e) {
			SwingUtilities.invokeLater(new Runnable() {

				public void run () {
					setBusy(true);

					try {
						actionPerformed();

					} catch (Exception exc) {
						/*
						 * This catch-clause will rarely be executed: the menu-item actions
						 * mostly use the shielded API which catches exceptions way before
						 */
						ErrorBox.showErrorDialog(DeploymentTree.this, "\"" + getValue(NAME) + "\" failed", exc);

					} finally {
						setBusy(false);
					}
				}
			});
		}

		protected abstract void actionPerformed () throws Exception;
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
			shieldedRefreshManager(selectedSupervisor);
		}

	}

	protected class ManagerPingAction extends DelayedAction {

		protected ManagerPingAction() {
			super("Send Ping Request");
		}

		public void actionPerformed () {
			shieldedPingManager(selectedSupervisor);
		}

	}

	protected class ManagerShutdownAction extends DelayedAction {

		protected ManagerShutdownAction() {
			super("Send Shutdown Request");
		}

		public void actionPerformed () throws Exception {
			shieldedShutdownManager(selectedSupervisor);
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

		public void actionPerformed () throws Exception {
			selectedSupervisor.containerPing((ContainerInfo) target.getUserObject());
		}
	}

	protected class ContainerShutdownAction extends DelayedAction {

		protected ContainerShutdownAction() {
			super("Send Shutdown Request");
		}

		public void actionPerformed () throws Exception {
			selectedSupervisor.containerShutdown((ContainerInfo) target.getUserObject());
		}
	}

	protected class ContainerMessageAction extends DelayedAction {

		protected ContainerMessageAction() {
			super("Send Message...");
		}

		public void actionPerformed () throws Exception {
			String msg = JOptionPane.showInputDialog(DeploymentTree.this, "Enter message text:");
			if (msg != null)
				selectedSupervisor.containerMessage((ContainerInfo) target.getUserObject(), IMaciSupervisor.MSG_INFORMATION, msg);
		}
	}

	protected class ClientPingAction extends DelayedAction {

		protected ClientPingAction() {
			super("Send Ping Request");
		}

		public void actionPerformed () throws Exception {
			selectedSupervisor.clientPing((ClientInfo) target.getUserObject());
		}
	}

	protected class ContainerDisconnectAction extends DelayedAction {

		protected ContainerDisconnectAction() {
			super("Send Disconnect Request");
		}

		public void actionPerformed () throws Exception {
			selectedSupervisor.containerDisconnect((ContainerInfo) target.getUserObject());
		}
	}

	protected class ClientDisconnectAction extends DelayedAction {

		protected ClientDisconnectAction() {
			super("Send Disconnect Request");
		}

		public void actionPerformed () throws Exception {
			selectedSupervisor.clientDisconnect((ClientInfo) target.getUserObject());
		}
	}

	protected class ClientMessageAction extends DelayedAction {

		protected ClientMessageAction() {
			super("Send Message...");
		}

		public void actionPerformed () throws Exception {
			String msg = JOptionPane.showInputDialog(DeploymentTree.this, "Enter message text:");
			if (msg != null)
				selectedSupervisor.clientMessage((ClientInfo) target.getUserObject(), IMaciSupervisor.MSG_INFORMATION, msg);
		}
	}

	protected class ContainerLogoutAction extends DelayedAction {

		protected ContainerLogoutAction() {
			super("Have logged out by Manager");
		}

		public void actionPerformed () throws Exception {
			shieldedLogoutContainer(selectedSupervisor, (ContainerInfo) target.getUserObject());
		}
	}

	protected class ClientLogoutAction extends DelayedAction {

		protected ClientLogoutAction() {
			super("Have logged out by Manager");
		}

		public void actionPerformed () throws Exception {
			shieldedLogoutClient(selectedSupervisor, (ClientInfo) target.getUserObject());
		}
	}

	protected class ComponentRequestAction extends DelayedAction {

		protected ComponentRequestAction() {
			super("Have activated");
		}

		public void actionPerformed () throws Exception {
			String name = ((ComponentInfo) target.getUserObject()).name;
			shieldedGetComponent(selectedSupervisor, name);
		}
	}

	protected class ComponentReleaseAction extends DelayedAction {

		protected ComponentReleaseAction() {
			super("Release own reference");
		}

		public void actionPerformed () throws Exception {
			String name = ((ComponentInfo) target.getUserObject()).name;
			shieldedReleaseComponents(selectedSupervisor, new String[]{name});
		}
	}

	protected class ComponentForceReleaseAction extends DelayedAction {

		protected ComponentForceReleaseAction() {
			super("Force system-wide deactivation");
		}

		public void actionPerformed () throws Exception {
			String name = ((ComponentInfo) target.getUserObject()).name;
			shieldedForceReleaseComponent(selectedSupervisor, name);
		}
	}



	// 
	// ======================= Shielded API =========================
	// 

	public void shieldedRefreshManager (GuiMaciSupervisor supervisor) {
		try {
			supervisor.getMaciInfo();

		} catch (NoPermissionEx exc) {
			mce.handleException(supervisor, exc);
		} catch (NotConnectedToManagerException exc) {
			mce.handleException(supervisor, exc);
		} catch (CorbaTransientException exc) {
			mce.handleException(supervisor, exc);
		} catch (CorbaNotExistException exc) {
			mce.handleException(supervisor, exc);
		} catch (UnknownErrorException exc) {
			mce.handleException(supervisor, exc);
		}
	}

	public void shieldedAddManager (String managerLoc) {
		GuiMaciSupervisor supervisor = null;

		try {
			supervisor = getMaciSupervisor(managerLoc);

		} catch (OrbInitException exc1) {
			mce.handleException(exc1);
		}

		try {
			startAndAddMaciSupervisor(supervisor);

		} catch (CannotRetrieveManagerException exc) {
			mce.handleException(supervisor, exc);
		} catch (NoPermissionEx exc) {
			mce.handleException(supervisor, exc);
		} catch (CorbaTransientException exc) {
			mce.handleException(supervisor, exc);
		} catch (CorbaNotExistException exc) {
			mce.handleException(supervisor, exc);
		} catch (UnknownErrorException exc) {
			mce.handleException(supervisor, exc);
		}
	}



	/**
	 * Ping Manager (shielded)
	 */
	public void shieldedPingManager (GuiMaciSupervisor supervisor) {
		try {
			supervisor.managerPing();

		} catch (NotConnectedToManagerException exc) {
			mce.handleException(supervisor, exc);
		} catch (CorbaTransientException exc) {
			mce.handleException(supervisor, exc);
		} catch (CorbaNotExistException exc) {
			mce.handleException(supervisor, exc);
		} catch (UnknownErrorException exc) {
			mce.handleException(supervisor, exc);
		}
	}

	/**
	 * Shutdown Manager (shielded)
	 */
	public void shieldedShutdownManager (GuiMaciSupervisor supervisor) {
		try {
			supervisor.managerShutdown();

		} catch (NotConnectedToManagerException exc) {
			mce.handleException(supervisor, exc);
		} catch (CorbaNoPermissionException exc) {
			mce.handleException(supervisor, exc);
		} catch (CorbaTransientException exc) {
			mce.handleException(supervisor, exc);
		} catch (CorbaNotExistException exc) {
			mce.handleException(supervisor, exc);
		} catch (UnknownErrorException exc) {
			mce.handleException(supervisor, exc);
		}
	}

	/**
	 * Logout Container (shielded)
	 */
	public void shieldedLogoutContainer (GuiMaciSupervisor supervisor, ContainerInfo info) {
		try {
			supervisor.managerLogout(info);

		} catch (NoPermissionEx exc) {
			mce.handleException(supervisor, exc);
		} catch (NotConnectedToManagerException exc) {
			mce.handleException(supervisor, exc);
		} catch (CorbaTransientException exc) {
			mce.handleException(supervisor, exc);
		} catch (CorbaNotExistException exc) {
			mce.handleException(supervisor, exc);
		} catch (UnknownErrorException exc) {
			mce.handleException(supervisor, exc);
		}
	}

	/**
	 * Logout Client (shielded)
	 */
	public void shieldedLogoutClient (GuiMaciSupervisor supervisor, ClientInfo info) {
		try {
			supervisor.managerLogout(info);

		} catch (NoPermissionEx exc) {
			mce.handleException(supervisor, exc);
		} catch (NotConnectedToManagerException exc) {
			mce.handleException(supervisor, exc);
		} catch (CorbaTransientException exc) {
			mce.handleException(supervisor, exc);
		} catch (CorbaNotExistException exc) {
			mce.handleException(supervisor, exc);
		} catch (UnknownErrorException exc) {
			mce.handleException(supervisor, exc);
		}
	}


	/**
	 * Retrieve component (shielded)
	 * 
	 * @return component or <code>null</code>
	 * 
	 * @throws ComponentNotAlreadyActivatedEx
	 * @throws CannotGetComponentEx
	 * @throws ComponentConfigurationNotFoundEx
	 */
	public org.omg.CORBA.Object shieldedGetComponent (GuiMaciSupervisor supervisor, String curl)
			throws ComponentNotAlreadyActivatedEx, CannotGetComponentEx, ComponentConfigurationNotFoundEx {
		try {
			return supervisor.managerGetComponent(curl);

		} catch (NoPermissionEx exc) {
			mce.handleException(supervisor, exc);
		} catch (NotConnectedToManagerException exc) {
			mce.handleException(supervisor, exc);
		} catch (CorbaTransientException exc) {
			mce.handleException(supervisor, exc);
		} catch (CorbaNotExistException exc) {
			mce.handleException(supervisor, exc);
		} catch (UnknownErrorException exc) {
			mce.handleException(supervisor, exc);
		}
		return null;
	}


	/**
	 * Release components (shielded)
	 */
	public void shieldedReleaseComponents (GuiMaciSupervisor supervisor, String[] curls) {
		try {
			supervisor.managerReleaseComponents(curls);

		} catch (NoPermissionEx exc) {
			mce.handleException(supervisor, exc);
		} catch (NotConnectedToManagerException exc) {
			mce.handleException(supervisor, exc);
		} catch (CorbaTransientException exc) {
			mce.handleException(supervisor, exc);
		} catch (CorbaNotExistException exc) {
			mce.handleException(supervisor, exc);
		} catch (UnknownErrorException exc) {
			mce.handleException(supervisor, exc);
		}
	}


	/**
	 * Force-release component (shielded)
	 */
	public void shieldedForceReleaseComponent (GuiMaciSupervisor supervisor, String curl) {
		try {
			supervisor.managerForceReleaseComponent(curl);

		} catch (NoPermissionEx exc) {
			mce.handleException(supervisor, exc);
		} catch (NotConnectedToManagerException exc) {
			mce.handleException(supervisor, exc);
		} catch (CorbaTransientException exc) {
			mce.handleException(supervisor, exc);
		} catch (CorbaNotExistException exc) {
			mce.handleException(supervisor, exc);
		} catch (UnknownErrorException exc) {
			mce.handleException(supervisor, exc);
		}
	}


	final protected ManagerConnectionExceptionHandler mce = new ManagerConnectionExceptionHandler();

	protected class ManagerConnectionExceptionHandler {

		protected void handleException (OrbInitException exc) {
			String msg = "Failed to initialize local orb. This prevents all corba connectivity.";
			ErrorBox.showErrorDialog(DeploymentTree.this, msg,	exc);
		}

		protected void handleException (GuiMaciSupervisor ms, NoPermissionEx exc) {
			seemsManagerHasChangedOrHasCutConnection(ms);
		}

		protected void handleException (GuiMaciSupervisor ms, CorbaNoPermissionException exc) {
			seemsManagerHasChangedOrHasCutConnection(ms);
		}

		protected void handleException (GuiMaciSupervisor ms, NotConnectedToManagerException exc) {
			seemsWeHaveDisconnected(ms);
		}

		protected void handleException (GuiMaciSupervisor ms, CannotRetrieveManagerException exc) {
			seemsManagerDoesNotExist(ms);
		}

		protected void handleException (GuiMaciSupervisor ms, CorbaNotExistException exc) {
			seemsManagerDoesNotExist(ms);
		}

		protected void handleException (GuiMaciSupervisor ms, CorbaTransientException exc) {
			seemsManagerIsDown(ms);
		}

		protected void handleException (IMaciSupervisor ms, UnknownErrorException exc) {
			String msg = "Unforeseen error talking to manager! Please report this to the Acs team.";
			ErrorBox.showErrorDialog(DeploymentTree.this, msg,	exc);
		}


		protected void seemsManagerIsDown (GuiMaciSupervisor ms) {

			String managerLoc = ms.getManagerLocation();
			String msg = "Seems the manager at " + managerLoc + " is down.\n" + "Ok to remove it from the View?";
			int answer = JOptionPane.showConfirmDialog(DeploymentTree.this, msg, "Communication Failed", JOptionPane.YES_NO_OPTION);

			try {
				if (answer == JOptionPane.OK_OPTION) {
					removeManager(managerLoc, false);
				}

			} catch (Exception exc1) {
				ErrorBox.showErrorDialog(DeploymentTree.this, "Failed to remove manager from view", exc1);
			}
		}

		protected void seemsManagerHasChangedOrHasCutConnection (GuiMaciSupervisor ms) {

			String managerLoc = ms.getManagerLocation();
			String msg = "Seems the manager at " + managerLoc + " has changed or\n"
					+ "has cut the connection. Will try to reconnect.";
			JOptionPane.showMessageDialog(DeploymentTree.this, msg, "Communication Failed", JOptionPane.INFORMATION_MESSAGE);

			try {
				// dismiss old supervisor...
				removeManager(managerLoc, true);
				// ...and re-add it
				ms.start();
				addManager(ms);

			} catch (Exception exc1) {
				ErrorBox.showMessageDialog(DeploymentTree.this, "Failed to reconnect to manager", false);
			}
		}

		protected void seemsWeHaveDisconnected (GuiMaciSupervisor ms) {

			String managerLoc = ms.getManagerLocation();
			String msg = "I have no connection to the manager at " + managerLoc + ".\n" + "Do you want to connect now?";
			int answer = JOptionPane.showConfirmDialog(DeploymentTree.this, msg, "Communication Failed",
					JOptionPane.YES_NO_OPTION);

			try {
				if (answer == JOptionPane.OK_OPTION) {
					// dismiss supervisor...
					removeManager(managerLoc, true);
					// ...and re-add it
					ms.start();
					addManager(ms);
				}

			} catch (Exception exc1) {
				ErrorBox.showMessageDialog(DeploymentTree.this, "Failed to connect to manager", false);
			}
		}

		protected void seemsManagerDoesNotExist (GuiMaciSupervisor ms) {

			String managerLoc = ms.getManagerLocation();
			String msg = "No manager exists at " + managerLoc + ".\n" + "Do you want to retry to connect?";
			for (;;) {
				int answer = JOptionPane.showConfirmDialog(DeploymentTree.this, msg, "Communication Failed",
						JOptionPane.YES_NO_OPTION);

				try {
					if (answer == JOptionPane.OK_OPTION) {
						ms.start();
						addManager(ms);
					}
					break;
				} catch (Exception exc) {
					continue;
				}
			}
		}

	}

}
