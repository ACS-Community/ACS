/*
 * Created on Oct 22, 2003 by mschilli
 */
package alma.acs.commandcenter.gui;

import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.Vector;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.swing.AbstractAction;
import javax.swing.Box;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.ToolTipManager;
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
import javax.swing.tree.MutableTreeNode;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreeNode;
import javax.swing.tree.TreePath;

import si.ijs.maci.ClientInfo;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ContainerInfo;
import alma.acs.commandcenter.meta.Firestarter.OrbInitException;
import alma.acs.commandcenter.meta.GuiMaciSupervisor;
import alma.acs.commandcenter.meta.IMaciSupervisor;
import alma.acs.commandcenter.meta.IMaciSupervisor.CannotRetrieveManagerException;
import alma.acs.commandcenter.meta.IMaciSupervisor.CorbaNoPermissionException;
import alma.acs.commandcenter.meta.IMaciSupervisor.CorbaNotExistException;
import alma.acs.commandcenter.meta.IMaciSupervisor.CorbaTransientException;
import alma.acs.commandcenter.meta.IMaciSupervisor.CorbaUnknownException;
import alma.acs.commandcenter.meta.IMaciSupervisor.NotConnectedToManagerException;
import alma.acs.commandcenter.meta.IMaciSupervisor.UnknownErrorException;
import alma.acs.commandcenter.meta.MaciInfo;
import alma.acs.commandcenter.meta.MaciInfo.FolderInfo;
import alma.acs.commandcenter.meta.MaciInfo.InfoDetail;
import alma.acs.commandcenter.meta.MaciInfo.SortingTreeNode;
import alma.acs.commandcenter.meta.MaciSupervisor;
import alma.acs.util.AcsLocations;
import alma.maciErrType.CannotGetComponentEx;
import alma.maciErrType.ComponentConfigurationNotFoundEx;
import alma.maciErrType.ComponentNotAlreadyActivatedEx;
import alma.maciErrType.NoPermissionEx;
import alma.maciErrType.wrappers.AcsJCannotDeactivateComponentEx;
import alma.maciErrType.wrappers.AcsJComponentDeactivationFailedEx;
import alma.maciErrType.wrappers.AcsJComponentDeactivationUncleanEx;

import com.xtmod.util.collections.TreeMerge;

/**
 * @author mschilli
 */
public class DeploymentTree extends JTree {

	protected ContextMenu containerContextMenu;
	protected ContextMenu managerContextMenu;
	protected ContextMenu clientContextMenu;
	protected ContextMenu componentContextMenu;
	protected ContextMenu folderContextMenu;

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
		this (ctrl, true);
	}

	// msc 2009-04: introducing "allowControl" flag for permission handling (used by OMC)
	public DeploymentTree(DeploymentTreeController ctrl, boolean allowControl) {
		
		super(new SortingTreeNode("Deployment Info"));
		this.setRootVisible(false);
		this.setShowsRootHandles(true);

		// --- cell renderer
		ToolTipManager.sharedInstance().registerComponent(this);
		this.setCellRenderer(cellRenderer = new Renderer());

		// --- forward events from the MaciTree-Models
		// (there will be one for each macimanager) to our own, all-in-one TreeModel
		treeEventForwarder = new TreeEventForwarder (this.getTreeModel());

		modelConverters = new Vector<ModelConverter>();

		// --- setup context menus
		clientContextMenu = new ClientContextMenu (allowControl);
		managerContextMenu = new ManagerContextMenu (allowControl);
		containerContextMenu = new ContainerContextMenu (allowControl);
		componentContextMenu = new ComponentContextMenu (allowControl);
		folderContextMenu = new FolderContextMenu (allowControl);

		this.addMouseListener(new MouseAdapter() {

			@Override
			public void mouseClicked (MouseEvent evt) {
				if (SwingUtilities.isRightMouseButton(evt)) {
					showContextMenu(evt);
				}
			}
		});
		this.getSelectionModel().addTreeSelectionListener(new SelectionListener());

		this.ctrl = ctrl;
	}


	// synchronous run in swing thread
	private void runSwingNow (Runnable r) {
		if (SwingUtilities.isEventDispatchThread())
			r.run();
		else
			try {
				SwingUtilities.invokeAndWait(r);
			} catch (Exception exc) {
				 /* exc.printStackTrace(); */
			}
	}

	
	/**
	 * @param evt
	 */
	protected void showContextMenu (MouseEvent evt) {

		TreePath targetPath = this.getClosestPathForLocation(evt.getX(), evt.getY());
		if (targetPath == null) {
			// clicked into a totally empty tree (no manager shown): ignore click.
			return;
		}

		setSelectionPath(targetPath);

		if (targetPath.getPathCount() == 1) {
			// clicked on the descriptive super-root node "Deployment Info"
			// that has no function besides looking good
			return;
		}


		// the supervisor (which is in the rootnode) for this subtree
		selectedSupervisor = maciSupervisor(((SortingTreeNode) targetPath.getPathComponent(1)));
		// the node the mouse was clicked on
		target = (SortingTreeNode) targetPath.getLastPathComponent();
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
			menu = folderContextMenu;
		} else if (userObject instanceof InfoDetail) {
			// msc 2012-06: help user navigate in large datasets (cf. COMP-3684)
			// this menu is built dynamically from rather expensive information.
			menu = new ContextMenu(false);
			int[] selectedHandles = ((SortingTreeNode)target).representedHandles;
			if (selectedHandles.length>0) {
				menu.add(new JLabel(" Scroll to ..."));
				menu.add(new JSeparator(JSeparator.HORIZONTAL));
				SortingTreeNode mgrNode = (SortingTreeNode)target.getPath()[1];
				List<Object> list = new ArrayList<Object>();
				list.add(mgrNode);
				for (SortingTreeNode top : mgrNode.childrens())
					list.addAll(Collections.list(top.children()));

				for (Object obj : list) {
					final SortingTreeNode elem = (SortingTreeNode)obj;
					for (int i=0; i<selectedHandles.length; i++) {
						if (elem.represents(selectedHandles[i])) {
							Object elemUserObject = elem.getUserObject();
							String elemName=null;
							if (elemUserObject instanceof ComponentInfo)
								elemName = ((ComponentInfo)elemUserObject).name;
							else if (elemUserObject instanceof ContainerInfo)
								elemName = ((ContainerInfo)elemUserObject).name;
							else if (elemUserObject instanceof ClientInfo)
								elemName = ((ClientInfo)elemUserObject).name;
							else if (elemUserObject instanceof MaciSupervisor)
								elemName = "Manager";
							if (elemName!=null) {
								menu.add(new AbstractAction(selectedHandles[i]+" = "+elemName){
									@Override public void actionPerformed (ActionEvent evt) {
										DeploymentTree.this.scrollPathToVisible(new TreePath(elem.getPath()));
									}
								});
							}
						}
					}
				}
			}

		} else {
			return;
		}

		menu.show(this, evt.getX(), evt.getY());
	}

	// get the supervisor stored in the tree node
	protected GuiMaciSupervisor maciSupervisor (DefaultMutableTreeNode managerNode) {
		return (GuiMaciSupervisor) managerNode.getUserObject();
	}

	protected SortingTreeNode getRoot () {
		return (SortingTreeNode) super.getModel().getRoot();
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

		// the root of maciInfo is just a node with the boring string "Manager"...
		final SortingTreeNode managerNode = ((SortingTreeNode)maciInfo.getRoot()).clone();
		// ...but the root of guiInfo will be a node holding the MaciSupervisor
		managerNode.setUserObject(mrfotogen);
		DefaultTreeModel guiInfo = new DefaultTreeModel(managerNode);
		ModelConverter mc = new ModelConverter(maciInfo, guiInfo);
		modelConverters.add(mc);
		maciInfo.addTreeModelListener(mc);

		// have events forwarded from the TreeModel to our own all-in-one TreeModel
		treeEventForwarder.addSource(guiInfo);

		// store the additional manager node in our tree
		getTreeModel().insertNodeInto(managerNode, getRoot(), getRoot().getChildCount());

		// populate guitree, will make the tree display
		mc.convertCompleteModel();

		// must expand the new manager node otherwise the subtree won't be visible
		runSwingNow(new Runnable(){
			public void run () {
				expandPath(new TreePath(new Object[]{getRoot(), managerNode}));

				// for user joy also expand the manager's subnodes
				expandPath(new TreePath(new Object[]{getRoot(), managerNode, managerNode.getChildAt(0)}));
				expandPath(new TreePath(new Object[]{getRoot(), managerNode, managerNode.getChildAt(1)}));
				expandPath(new TreePath(new Object[]{getRoot(), managerNode, managerNode.getChildAt(2)}));
			}
		});
		
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
		// iterate over manager nodes
		for (SortingTreeNode managerNode : getRoot().childrens()) {
			shieldedRefreshManager(maciSupervisor(managerNode));
		}
	}

	

	/**
	 * Finds the manager node with the given managerLocation inside
	 */
	protected DefaultMutableTreeNode getManagerNode (String managerLoc) {
		String toFind = managerLoc;

		// iterate over macisupervisor nodes
		for (SortingTreeNode managerNode : getRoot().childrens()) {
			// compare corbalocs
			String managerLocation = maciSupervisor(managerNode).getManagerLocation();
			if (managerLocation.equals(toFind))
				return managerNode;
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
	 * Signals to the user that an action takes longer.
	 */
	protected void setBusy (boolean b) {
		int cursor = (b) ? Cursor.WAIT_CURSOR : Cursor.DEFAULT_CURSOR;
		this.setCursor(Cursor.getPredefinedCursor(cursor));
	}


	// --- "freeze view" logic ---

	protected boolean isViewFrozen;

	/**
	 * Will make the model converters pause, so the deployment
	 * trees remain unchanged so the user can navigate the
	 * trees without disturbance.
	 */
	public void setViewFrozen (boolean newValue) {
		isViewFrozen = newValue;

		/* when "freeze" gets switched off, there may be
		 * changes pending in some model converters */
		if (newValue == false) {
			for (ModelConverter mc : modelConverters)
				mc.convertCompleteModelIfDirty();
		}
	}
	
	/**
	 * This is public so outside code like a toggle
	 * button could update its state.
	 */
	public boolean isViewFrozen() {
		return isViewFrozen;
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
		
		Component hiddenNode = Box.createRigidArea(new Dimension(0,0));

		@Override
		public Component getTreeCellRendererComponent (JTree tree, Object value, boolean sel, boolean expand, boolean leaf, int row, boolean foc) {

			super.getTreeCellRendererComponent(tree, value, sel, expand, leaf, row, foc);

			this.setIcon(null);

			DefaultMutableTreeNode node = (DefaultMutableTreeNode) value;

			// ==== caption ====

			String[] captions = getCaptions(node);
			String text =  captions[0];
			String tooltip = captions[1];

			this.setText(text);
			this.setToolTipText(tooltip);


			// ==== tree filtering ====

			// idea for this cheap filtering taken from:
			// http://stackoverflow.com/questions/9234297/filtering-on-a-jtree
			if (node instanceof SortingTreeNode)
				if (((SortingTreeNode)node).filtered)
					return hiddenNode;
			
			// ==== selection ====

			/*
			 * We want the real selection blue, and the "deputy"-selections gray. Since our
			 * superclass implementation determines the selection background in paint() and
			 * not in getRendererComponent(), we need to set the
			 * "backgroundNonSelectionColor" property which is getting evaluated by paint()
			 */

			Border border = null;

			if (sel) {
				border = bluelineBorder;
				if (node instanceof SortingTreeNode) {
					currentlySelectedHandles = ((SortingTreeNode) node).representedHandles;
				}
			} else {

				Color backgroundNonSelectionColor = null;

				if (node instanceof SortingTreeNode) {
					SortingTreeNode casted = (SortingTreeNode) node;
					for (int i=0; i<currentlySelectedHandles.length; i++) {
						if (casted.represents(currentlySelectedHandles[i])) {
							border = graylineBorder;
							backgroundNonSelectionColor = grayBackground;
							break;
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
		
		/** @return (text,tooltip) */
		String[] getCaptions (DefaultMutableTreeNode node) {

			String text; /* compiler will optimize the string operations */
			String tooltip = null;

			Object userObject = node.getUserObject();

			
			if (userObject == null) {
				text = "empty node (strange)";

			} else if (userObject instanceof IMaciSupervisor) {
				String mgrLoc = maciSupervisor(node).getManagerLocation();
				String[] mgr = AcsLocations.convert(mgrLoc);
				text = "Manager on " + mgr[0] + ":" + mgr[1];

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

				
				if (node instanceof SortingTreeNode) {
					if (((SortingTreeNode)node).representedHandles.length > 0)
						tooltip = "Right-Click for Navigation";
				}

			} else if (userObject instanceof FolderInfo) {
				FolderInfo info = (FolderInfo) userObject;
				text = info.name;
				text = text + " (" + node.getChildCount() + ")";
				if (info.hasFilter())
					text += ", showing only '"+info.filter+"'";
				tooltip = "Right-Click for Filtering";

			} else
				text = String.valueOf(node);

			return new String[] {text, tooltip};
		}
		
	}

	/**
	 * Listens on selection-changes in the tree to trigger a repaint-event.
	 */
	protected class SelectionListener implements TreeSelectionListener {

		public void valueChanged (TreeSelectionEvent e) {
			Object node = e.getPath().getLastPathComponent();
			if (node instanceof SortingTreeNode)
				if (!((SortingTreeNode)node).filtered)
					cellRenderer.currentlySelectedHandles = ((SortingTreeNode) node).representedHandles;
			repaint();
		}

	}

	void applyFilters (DefaultTreeModel tm) {
		SortingTreeNode mgrNode = (SortingTreeNode)tm.getRoot();
		for (SortingTreeNode folder : mgrNode.childrens()) {
			applyFilter(folder);
		}
	}

	void applyFilter (SortingTreeNode folder) {
		FolderInfo folderInfo = (FolderInfo)folder.getUserObject();
		for (SortingTreeNode entry : folder.childrens()) {
		 	String caption = cellRenderer.getCaptions(entry)[0];
		 	boolean passes = !folderInfo.hasFilter() || caption.contains (folderInfo.filter);
		 	for (Enumeration<?> subtree = entry.breadthFirstEnumeration(); subtree.hasMoreElements();) 
		 		((SortingTreeNode)(subtree.nextElement())).filtered = !passes;
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

		// --- "freeze view" logic ---
		
		protected boolean isDirty;

		protected void convertCompleteModelIfDirty () {
			if (isDirty) {
				isDirty = false;
				convertCompleteModel();
			}
		}		
		
		// --- conversion logic ---

		public void treeNodesChanged (TreeModelEvent e) {}
		public void treeNodesInserted (TreeModelEvent e) {}
		public void treeNodesRemoved (TreeModelEvent e) {}

		public void treeStructureChanged (TreeModelEvent e) {
			if (isViewFrozen()) {
				isDirty = true;
				return;
			}
			convertCompleteModel();
			applyFilters (targetModel);
		}


			protected final TreeMerge<String> treemerger = new TreeMerge<String>(){

				@Override protected String identifyExisting (TreeNode x) {
					return identify((DefaultMutableTreeNode)x);
				}

				@Override protected String identifyIncoming (TreeNode x) {
					return identify((DefaultMutableTreeNode)x);
				}

				String identify (DefaultMutableTreeNode x) {
					Object userObject = x.getUserObject();

					if (userObject instanceof ComponentInfo)
						return ((ComponentInfo) userObject).name;
					
					if (userObject instanceof ClientInfo)
						return String.valueOf(((ClientInfo) userObject).h);

					if (userObject instanceof ContainerInfo)
						return ((ContainerInfo) userObject).name;

					if (userObject instanceof FolderInfo)
						return ((FolderInfo) userObject).name;

					return x.toString();
				}

				@Override protected boolean isUpdate (TreeNode exist, TreeNode incom) {
					/* our tree-renderer adds the childcount to some nodes (the folder info nodes),
					 * so if the childcount changes we want a tree-change event to be sent to the
					 * jtree. we could run the renderer here to find out if "incom" would be
					 * rendered differently from "exist" and thus we consider this an update. but
					 * for simplicity, i'm re-implementing a tiny portion of renderer logic here. */
					if (exist.getChildCount() != incom.getChildCount())
						return true;

					/* Keeping it kind of simple by looking at the toString() of the treenode
					 * which is in fact a toString() of the userobject. One could also do some
					 * instanceof here and look at the userobjects more closely to find out
					 * whether they have different contents than before. Checking via
					 * toString() means that nodes that have a corbastruct as their userobject
					 * will be updated at each refresh. This is a bit costly but makes sure the
					 * tree always shows up-to-date info. */
					return !String.valueOf(exist).equals(String.valueOf(incom));
				}

				@Override protected MutableTreeNode create (TreeNode x) {
					return  (DefaultMutableTreeNode) ((DefaultMutableTreeNode) x).clone();
				}

				@Override protected void applyUpdate (TreeNode exist, TreeNode incom) {
					((DefaultMutableTreeNode)exist).setUserObject(((DefaultMutableTreeNode)incom).getUserObject());
					if (exist instanceof SortingTreeNode && incom instanceof SortingTreeNode) // always true?
						((SortingTreeNode)exist).representedHandles = ((SortingTreeNode)incom).representedHandles;
				}

			};

		/** Used to force data visibility between threads, as per Java Memory Model */
		private AtomicBoolean jmmFlushRefresh = new AtomicBoolean();

		protected void convertCompleteModel() {

			treemerger.diff (targetModel, sourceModel);

			/* this is the main point of the whole model conversion business: modify the
			 * jtree's tree model only in the swing thread. the maci info is modified by the
			 * supervisor in whatever thread it uses. we listen to those changes and transfer
			 * them to the tree's underlying tree model.... using the swing thread... */
			if (!treemerger.areEqual()) {

				// flush
				jmmFlushRefresh.set(true);

				runSwingNow(new Runnable() {
					public void run () {

						// refresh
						jmmFlushRefresh.get();

						treemerger.merge();
					}
				});
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
		/* System.err.println("TEF: "+Thread.currentThread().getName()+" treeStructureChanged");*/
			TreeNode n = (TreeNode) e.getTreePath().getLastPathComponent();
			forwardTarget.nodeStructureChanged(n);
		}

		public void treeNodesChanged (TreeModelEvent e) {
		/* System.err.println("TEF: "+Thread.currentThread().getName()+" treeNodesChanged "+e);*/
			TreeNode n = (TreeNode) e.getTreePath().getLastPathComponent();
			forwardTarget.nodesChanged(n, e.getChildIndices());
		}

		public void treeNodesInserted (TreeModelEvent e) {
		/*	System.err.println("TEF: "+Thread.currentThread().getName()+" treeNodesInserted "+e);*/
			TreeNode n = (TreeNode) e.getTreePath().getLastPathComponent();
			forwardTarget.nodesWereInserted (n, e.getChildIndices());
		}

		public void treeNodesRemoved (TreeModelEvent e) {
		/*	System.err.println("TEF: "+Thread.currentThread().getName()+" treeNodesRemoved "+e);*/
			TreeNode n = (TreeNode) e.getTreePath().getLastPathComponent();
			forwardTarget.nodesWereRemoved(n, e.getChildIndices(), e.getChildren());
		}
	}

	// 
	// =================== Context Menus =====================
	// 


	/** 
	 * Base class for our context menus
	 */
	protected class ContextMenu extends JPopupMenu {

		protected ContextMenu (boolean allowControl) {}

		/**
		 * If this menu is empty, we don't bother to show it.
		 */
		@Override public void show (Component invoker, int x, int y) {
			if (getComponentCount() == 0)
				return;
			super.show(invoker, x, y);
		}

	}

	protected class ManagerContextMenu extends ContextMenu {

		protected ManagerContextMenu (boolean allowControl) {
			super(allowControl);

			this.add(new ManagerRefreshAction());

			if (allowControl) {
				this.add(new JPopupMenu.Separator());
				this.add(new ManagerPingAction());
				this.add(new ManagerShutdownAction());
			}

			/* 
			 * msc (2008-10): "remove from view" hardly needed: if a
			 * manager goes away, we offer to remove it anyhow. and
			 * other than that, this entry is undesirable inside the omc.
			 * 
			 * this.add(new JPopupMenu.Separator());
			 * this.add(new RemoveFromViewAction());
			 */
		}

	}

	
	protected class FolderContextMenu extends ContextMenu {

		protected JTextField txtFilter = new JTextField(8);

		protected FolderContextMenu (boolean allowControl) {
			super (allowControl);

			Box b = Box.createHorizontalBox();
			b.add(new JLabel(" Show only "));
			b.add(txtFilter);
			b.add(new JLabel(" "));
			this.add(b);
			this.add(new JSeparator(JSeparator.HORIZONTAL));
			this.add(new AbstractAction("Apply"){
				@Override public void actionPerformed (ActionEvent e) {
					((FolderInfo)target.getUserObject()).filter = txtFilter.getText().trim();
					applyFilter((SortingTreeNode)target);

					// this re-renders the tree, and collapses the subtrees.
					getTreeModel().reload(target);
				}
			});
			this.add(new AbstractAction("Reset"){
				@Override public void actionPerformed (ActionEvent e) {
					txtFilter.setText(null);
					((FolderInfo)target.getUserObject()).filter = txtFilter.getText().trim();
					applyFilter((SortingTreeNode)target);

					// this re-renders the tree, and collapses the subtrees.
					getTreeModel().reload(target);
				}
			});
		}

		// updates the menu gui before show
		@Override protected void firePopupMenuWillBecomeVisible () {
			txtFilter.setText(((FolderInfo)target.getUserObject()).filter);
			super.firePopupMenuWillBecomeVisible();
		}

	}

	protected class ContainerContextMenu extends ContextMenu {

		protected ContainerContextMenu (boolean allowControl) {
			super (allowControl);

			if (allowControl) {
				this.add(new ContainerPingAction());
				this.add(new ContainerMessageAction());
				this.add(new ContainerDisconnectAction());
				this.add(new ContainerShutdownAction());
	
				this.add(new JPopupMenu.Separator());
				this.add(new ContainerLogoutAction());
			}
		}

	}

	protected class ClientContextMenu extends ContextMenu {

		protected ClientContextMenu (boolean allowControl) {
			super (allowControl);

			if (allowControl) {
				this.add(new ClientPingAction());
				this.add(new ClientMessageAction());
				this.add(new ClientDisconnectAction());
	
				this.add(new JPopupMenu.Separator());
				this.add(new ClientLogoutAction());
			}
		}

	}

	protected class ComponentContextMenu extends ContextMenu {

		protected ComponentContextMenu (boolean allowControl) {
			super (allowControl);

			if (allowControl) {
				this.add(new ComponentRequestAction());
				this.add(new ComponentReleaseAction());
	
				this.add(new JPopupMenu.Separator());
				this.add(new ComponentForceReleaseAction());
			}
		}

	}

	// 
	// ======================= Actions =========================
	// 

	/**
	 * Performs the work to be done within the event-dispatcher thread
	 */
	protected abstract class SwingAction extends AbstractAction {

		protected SwingAction(String name) {
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
	 * Performs the work to be done delayed, and NOT within the event-dispatcher thread.
	 * Used for most actions in the context menus.
	 */
	protected abstract class BackgroundAction extends AbstractAction {

		protected BackgroundAction(String name) {
			super(name);
		}

		final public void actionPerformed (ActionEvent e) {
			ctrl.getBackgroundExecutor().execute(new Runnable() {
				public void run () {
					
					setBusy(true);

					try {
						actionPerformed();

					} catch (Exception exc) {
						/* This catch-clause will rarely be executed: the menu-item actions
						 * mostly use the shielded API which catches exceptions way before */
						ErrorBox.showErrorDialog(DeploymentTree.this, "\"" + getValue(NAME) + "\" failed", exc);

					} finally {
						setBusy(false);
					}
				}
			});
		}

		protected abstract void actionPerformed () throws Exception;
	}

	protected class RemoveFromViewAction extends SwingAction {

		protected RemoveFromViewAction() {
			super("Remove from View");
		}

		@Override
		public void actionPerformed () {
			removeNode(target);
		}
	}

	
	protected class ManagerRefreshAction extends BackgroundAction {

		protected ManagerRefreshAction() {
			super("Refresh Info");
		}

		@Override
		public void actionPerformed () {
			shieldedRefreshManager(selectedSupervisor);
		}

	}

	protected class ManagerPingAction extends BackgroundAction {

		protected ManagerPingAction() {
			super("Send Ping Request");
		}

		@Override
		public void actionPerformed () {
			shieldedPingManager(selectedSupervisor);
		}

	}

	protected class ManagerShutdownAction extends BackgroundAction {

		protected ManagerShutdownAction() {
			super("Send Shutdown Request");
		}

		@Override
		public void actionPerformed () throws Exception {
			shieldedShutdownManager(selectedSupervisor);
		}
	}

	protected class ContainerPingAction extends BackgroundAction {

		protected ContainerPingAction() {
			super("Send Ping Request");
		}

		@Override
		public void actionPerformed () throws Exception {
			selectedSupervisor.containerPing((ContainerInfo) target.getUserObject());
		}
	}

	protected class ContainerShutdownAction extends BackgroundAction {

		protected ContainerShutdownAction() {
			super("Send Shutdown Request");
		}

		@Override
		public void actionPerformed () throws Exception {
			selectedSupervisor.containerShutdown((ContainerInfo) target.getUserObject());
		}
	}

	protected class ContainerMessageAction extends BackgroundAction {

		protected ContainerMessageAction() {
			super("Send Message...");
		}

		@Override
		public void actionPerformed () throws Exception {
			String msg = JOptionPane.showInputDialog(DeploymentTree.this, "Enter message text:");
			if (msg != null)
				selectedSupervisor.containerMessage((ContainerInfo) target.getUserObject(), IMaciSupervisor.MSG_INFORMATION, msg);
		}
	}

	protected class ClientPingAction extends BackgroundAction {

		protected ClientPingAction() {
			super("Send Ping Request");
		}

		@Override
		public void actionPerformed () throws Exception {
			selectedSupervisor.clientPing((ClientInfo) target.getUserObject());
		}
	}

	protected class ContainerDisconnectAction extends BackgroundAction {

		protected ContainerDisconnectAction() {
			super("Send Disconnect Request");
		}

		@Override
		public void actionPerformed () throws Exception {
			selectedSupervisor.containerDisconnect((ContainerInfo) target.getUserObject());
		}
	}

	protected class ClientDisconnectAction extends BackgroundAction {

		protected ClientDisconnectAction() {
			super("Send Disconnect Request");
		}

		@Override
		public void actionPerformed () throws Exception {
			selectedSupervisor.clientDisconnect((ClientInfo) target.getUserObject());
		}
	}

	protected class ClientMessageAction extends BackgroundAction {

		protected ClientMessageAction() {
			super("Send Message...");
		}

		@Override
		public void actionPerformed () throws Exception {
			String msg = JOptionPane.showInputDialog(DeploymentTree.this, "Enter message text:");
			if (msg != null)
				selectedSupervisor.clientMessage((ClientInfo) target.getUserObject(), IMaciSupervisor.MSG_INFORMATION, msg);
		}
	}

	protected class ContainerLogoutAction extends BackgroundAction {

		protected ContainerLogoutAction() {
			super("Have logged out by Manager");
		}

		@Override
		public void actionPerformed () throws Exception {
			shieldedLogoutContainer(selectedSupervisor, (ContainerInfo) target.getUserObject());
		}
	}

	protected class ClientLogoutAction extends BackgroundAction {

		protected ClientLogoutAction() {
			super("Have logged out by Manager");
		}

		@Override
		public void actionPerformed () throws Exception {
			shieldedLogoutClient(selectedSupervisor, (ClientInfo) target.getUserObject());
		}
	}

	protected class ComponentRequestAction extends BackgroundAction {

		protected ComponentRequestAction() {
			super("Acquire reference ( Activate )");
		}

		@Override
		public void actionPerformed () throws Exception {
			String name = ((ComponentInfo) target.getUserObject()).name;
			shieldedGetComponent(selectedSupervisor, name);
		}
	}

	protected class ComponentReleaseAction extends BackgroundAction {

		protected ComponentReleaseAction() {
			super("Release reference");
		}

		@Override
		public void actionPerformed () throws Exception {
			String name = ((ComponentInfo) target.getUserObject()).name;
			shieldedReleaseComponents(selectedSupervisor, new String[]{name});
		}
	}

	protected class ComponentForceReleaseAction extends BackgroundAction {

		protected ComponentForceReleaseAction() {
			super("Force system-wide deactivation");
		}

		@Override
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

		} catch (CorbaUnknownException exc) {
			/* thrown by manager if component was never retrieved, we ignore this.*/
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
		} catch (AcsJCannotDeactivateComponentEx ex) { // @TODO remove after change in maci.idl
			mce.handleException(supervisor, ex);
		} catch (AcsJComponentDeactivationUncleanEx ex) {
			mce.handleException(supervisor, ex);
		} catch (AcsJComponentDeactivationFailedEx ex) {
			mce.handleException(supervisor, ex);
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

		protected void handleException (GuiMaciSupervisor ms, AcsJCannotDeactivateComponentEx exc) { // @TODO remove after change in maci.idl
			seemsComponentDeactivationFailed(ms);
		}

		protected void handleException (GuiMaciSupervisor ms, AcsJComponentDeactivationUncleanEx exc) {
			seemsComponentDeactivationFailed(ms);
		}

		protected void handleException (GuiMaciSupervisor ms, AcsJComponentDeactivationFailedEx exc) {
			seemsComponentDeactivationFailed(ms);
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

		private void seemsComponentDeactivationFailed (GuiMaciSupervisor ms) {
			String msg = "The manager reported a problem taking down the component.\nThe component may still be active.";
			ErrorBox.showMessageDialog(DeploymentTree.this, msg, true);
		}

	}

}
