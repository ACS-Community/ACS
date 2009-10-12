package alma.acs.eventbrowser.views;

import java.util.ArrayList;
import java.util.Iterator;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.part.*;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.graphics.Image;
import org.eclipse.jface.action.*;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.*;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.SWT;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.jobs.Job;

import alma.acs.eventbrowser.Application;
import alma.acs.eventbrowser.model.ChannelData;
import alma.acs.eventbrowser.model.EventModel;
import alma.acs.eventbrowser.Application.*;


/**
 * This sample class demonstrates how to plug-in a new
 * workbench view. The view shows data obtained from the
 * model. The sample creates a dummy model on the fly,
 * but a real implementation would connect to the model
 * available either in this or another plug-in (e.g. the workspace).
 * The view is connected to the model using a content provider.
 * <p>
 * The view uses a label provider to define how model
 * objects should be presented in the view. Each
 * view can present the same model objects using
 * different labels and icons, if needed. Alternatively,
 * a single label provider can be shared between views
 * in order to ensure that objects of the same type are
 * presented in the same way everywhere.
 * <p>
 */

public class ChannelTreeView extends ViewPart {
	private TreeViewer viewer;
	private DrillDownAdapter drillDownAdapter;
	private Action refreshAction;
	private Action startMonitoringAction;
	private Action doubleClickAction;
	private ViewContentProvider vcp;
	private IViewSite vs;
	
	private Thread monitorThread;
	
	private EventModel em;
	private long howOften = 10000l; // Default is every 10 seconds
	
	public static final String ID = "alma.acs.eventbrowser.views.channeltree";

	/*
	 * The content provider class is responsible for
	 * providing objects to the view. It can wrap
	 * existing objects in adapters or simply return
	 * objects as-is. These objects may be sensitive
	 * to the current input of the view, or ignore
	 * it and always show the same content 
	 * (like Task List, for example).
	 */
	 
	class TreeObject implements IAdaptable {
		private String name;
		private TreeParent parent;
		
		public TreeObject(String name) {
			this.name = name;
		}
		public String getName() {
			return name;
		}
		public void setParent(TreeParent parent) {
			this.parent = parent;
		}
		public TreeParent getParent() {
			return parent;
		}
		public String toString() {
			return getName();
		}
		public Object getAdapter(Class key) {
			return null;
		}
	}
	
	class TreeParent extends TreeObject {
		private ArrayList<TreeObject> children;
		public TreeParent(String name) {
			super(name);
			children = new ArrayList<TreeObject>();
		}
		public void addChild(TreeObject child) {
			children.add(child);
			child.setParent(this);
		}
		public void removeChild(TreeObject child) {
			children.remove(child);
			child.setParent(null);
		}
		public TreeObject [] getChildren() {
			return (TreeObject [])children.toArray(new TreeObject[children.size()]);
		}
		public boolean hasChildren() {
			return children.size()>0;
		}
	}

	class ViewContentProvider implements IStructuredContentProvider, 
										   ITreeContentProvider {
		private TreeParent invisibleRoot;

		public void inputChanged(Viewer v, Object oldInput, Object newInput) {
			viewer = (TreeViewer)v;
		}
		public void dispose() {
		}
		
		public Object[] getElements(Object parent) {
			if (parent.equals(getViewSite())) {
				if (invisibleRoot==null) initialize();
				return getChildren(invisibleRoot);
			}
			return getChildren(parent);
		}
		public Object getParent(Object child) {
			if (child instanceof TreeObject) {
				return ((TreeObject)child).getParent();
			}
			return null;
		}
		public Object [] getChildren(Object parent) {
			if (parent instanceof TreeParent) {
				return ((TreeParent)parent).getChildren();
			}
			return new Object[0];
		}
		public boolean hasChildren(Object parent) {
			if (parent instanceof TreeParent)
				return ((TreeParent)parent).hasChildren();
			return false;
		}
/*
 * We will set up a dummy model to initialize tree hierarchy.
 * In a real code, you will connect to a real model and
 * expose its hierarchy.
 */
		public void initialize() {
			ArrayList<ChannelData> clist = null;
			try {
				em = EventModel.getInstance();
				clist = em.getChannelStatistics();
			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			TreeParent root = new TreeParent("Notify Service");

			if (clist != null) {
				for (Iterator<ChannelData> iterator = clist.iterator(); iterator
						.hasNext();) {
					ChannelData cd = iterator.next();
					TreeParent tp = new TreeParent(cd.getName());
					TreeObject tcon = new TreeObject(cd
							.getNumConsumersAndDelta());
					TreeObject tsup = new TreeObject(cd
							.getNumSuppliersAndDelta());
					tp.addChild(tcon);
					tp.addChild(tsup);
					root.addChild(tp);
				}
			}
			invisibleRoot = new TreeParent("");
			invisibleRoot.addChild(root);
//			System.out.println("Root has following children: "+root.children);
		}
	}
	class ViewLabelProvider extends LabelProvider {

		public String getText(Object obj) {
			return obj.toString();
		}
		public Image getImage(Object obj) {
			String imageKey = ISharedImages.IMG_OBJ_ELEMENT;
			if (obj instanceof TreeParent)
			   imageKey = ISharedImages.IMG_OBJ_FOLDER;
			return PlatformUI.getWorkbench().getSharedImages().getImage(imageKey);
		}
	}
	class NameSorter extends ViewerSorter {
	}

	/**
	 * The constructor.
	 */
	public ChannelTreeView() {
	}

	/**
	 * This is a callback that will allow us
	 * to create the viewer and initialize it.
	 */
	public void createPartControl(Composite parent) {
		viewer = new TreeViewer(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		drillDownAdapter = new DrillDownAdapter(viewer);
		vcp = new ViewContentProvider();
		viewer.setContentProvider(vcp);
		viewer.setLabelProvider(new ViewLabelProvider());
		viewer.setSorter(new NameSorter());
		vs = getViewSite();
		viewer.setInput(getViewSite());

		// Create the help context id for the viewer's control
		PlatformUI.getWorkbench().getHelpSystem().setHelp(viewer.getControl(), "alma.acs.eventbrowser.viewer");
		makeActions();
		hookContextMenu();
		hookDoubleClickAction();
		contributeToActionBars();
		if (Application.isMonitoring()) {
			startMonitoringAction.setEnabled(false);
			startMonitoring();
		}
	}
	
	public TreeViewer getViewer() {
		return viewer;
	}


	private void hookContextMenu() {
		MenuManager menuMgr = new MenuManager("#PopupMenu");
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {
			public void menuAboutToShow(IMenuManager manager) {
				ChannelTreeView.this.fillContextMenu(manager);
			}
		});
		Menu menu = menuMgr.createContextMenu(viewer.getControl());
		viewer.getControl().setMenu(menu);
		getSite().registerContextMenu(menuMgr, viewer);
	}

	private void contributeToActionBars() {
		IActionBars bars = getViewSite().getActionBars();
		fillLocalPullDown(bars.getMenuManager());
		fillLocalToolBar(bars.getToolBarManager());
	}

	private void fillLocalPullDown(IMenuManager manager) {
		manager.add(refreshAction);
		manager.add(new Separator());
		manager.add(startMonitoringAction);
	}

	private void fillContextMenu(IMenuManager manager) {
		manager.add(refreshAction);
		manager.add(startMonitoringAction);
		manager.add(new Separator());
		drillDownAdapter.addNavigationActions(manager);
		// Other plug-ins can contribute their actions here
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	}
	
	private void fillLocalToolBar(IToolBarManager manager) {
		manager.add(refreshAction);
		manager.add(startMonitoringAction);
		manager.add(new Separator());
		drillDownAdapter.addNavigationActions(manager);
	}

	private void makeActions() {
		refreshAction = new Action() {
			public void run() {
				vcp.initialize();
				viewer.refresh();
			}
		};
		refreshAction.setText("Refresh");
		refreshAction.setToolTipText("Update the channel statistics");
		refreshAction.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages().
			getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
		
		startMonitoringAction = new Action() {
			public void run() {
				startMonitoring();
				setEnabled(false);
			}
		};
		startMonitoringAction.setText("Start monitoring");
		startMonitoringAction.setToolTipText("Begin periodic updating of channel data");
		startMonitoringAction.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages().
				getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
		doubleClickAction = new Action() {
			public void run() {
				ISelection selection = viewer.getSelection();
				Object obj = ((IStructuredSelection)selection).getFirstElement();
				showMessage("Double-click detected on "+obj.toString());
			}
		};
	}

	private void hookDoubleClickAction() {
		viewer.addDoubleClickListener(new IDoubleClickListener() {
			public void doubleClick(DoubleClickEvent event) {
				doubleClickAction.run();
			}
		});
	}
	private void showMessage(String message) {
		MessageDialog.openInformation(
			viewer.getControl().getShell(),
			"ChannelTreeView",
			message);
	}

	/**
	 * Passing the focus request to the viewer's control.
	 */
	public void setFocus() {
		viewer.getControl().setFocus();
	}
	
	
	public void startMonitoring() {
		Application.setMonitoring(true);

		Runnable t = new Runnable()  {
			int i = 0;
			public Runnable r = new Runnable() {
				public void run() {
					final Display display = viewer.getControl().getDisplay();
					if (!display.isDisposed()) {
						viewer.refresh();
						viewer.expandAll();
					}
				}
			};
			
			public void run() {
				final Display display = viewer.getControl().getDisplay();

				while (Application.isMonitoring()) {
					vcp.initialize();
					if (!display.isDisposed())
						display.asyncExec(r);
					try {
						Thread.sleep(howOften);
						System.out.println("Iteration "+ ++i);
					} catch (InterruptedException e) {
						System.out.println("Monitoring was interrupted!");
						Application.setMonitoring(false);
						startMonitoringAction.setEnabled(true);
					}
				}
				Application.setMonitoring(false);
				startMonitoringAction.setEnabled(true);
			}
		};
		Thread th = new Thread(t);
		th.start();
	}

}