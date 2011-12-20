/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.eventbrowser.views;

import org.eclipse.core.runtime.IAdapterFactory;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.model.BaseWorkbenchContentProvider;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.eclipse.ui.part.DrillDownAdapter;
import org.eclipse.ui.part.ViewPart;

import alma.acs.eventbrowser.Application;
import alma.acs.eventbrowser.model.ChannelParticipantName;
import alma.acs.eventbrowser.model.ChannelData;
import alma.acs.eventbrowser.model.EventModel;
import alma.acs.eventbrowser.model.MCStatistics;
import alma.acs.eventbrowser.model.NotifyServiceData;
import alma.acs.eventbrowser.model.NotifyServices;


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
	private Action refreshAction;
	private Action startMonitoringAction;
	
	private IAdapterFactory adapterFactory = new EventGuiAdapterFactory();
		
	private long howOften = 10000l; // Default is every 10 seconds
	
	public static final String ID = "alma.acs.eventbrowser.views.channeltree";
	private Thread channelTreeThread;
	private SubscribeToChannelAction subscribeAction;
	private SubscribeToAllChannelsAction subscribeToAllAction;

	private class NameComparator extends ViewerComparator {
		@Override
		public int compare(Viewer viewer, Object e1, Object e2) {
			if (e1 instanceof MCStatistics && e2 instanceof MCStatistics)
				return 0; // leave the statistics in the order I added them in ChannelData!!
			return super.compare(viewer, e1, e2);
		}
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
		// For the rationale behind the use of adapters here, see the comment at the beginning of
		// EventGuiAdapterFactory and the reference within.
		Platform.getAdapterManager().registerAdapters(adapterFactory,ChannelData.class);
		Platform.getAdapterManager().registerAdapters(adapterFactory,NotifyServiceData.class);
		Platform.getAdapterManager().registerAdapters(adapterFactory,NotifyServices.class);
		Platform.getAdapterManager().registerAdapters(adapterFactory,MCStatistics.class);
		Platform.getAdapterManager().registerAdapters(adapterFactory,ChannelParticipantName.class);

		new DrillDownAdapter(viewer);
		viewer.setContentProvider(new BaseWorkbenchContentProvider());
		//viewer.setLabelProvider(new ViewLabelProvider());
		viewer.setLabelProvider(new WorkbenchLabelProvider());
		viewer.setComparator(new NameComparator());
		try {
			EventModel.getInstance();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		viewer.setInput(NotifyServices.getInstance()); // TODO: Complete the hierarchy and add an adapter for the NotifyServiceData
		getSite().setSelectionProvider(viewer);
		// Create the help context id for the viewer's control
		PlatformUI.getWorkbench().getHelpSystem().setHelp(viewer.getControl(), "alma.acs.eventbrowser.viewer");
		makeActions();
		hookContextMenu();
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
		manager.add(subscribeToAllAction);
	}

	private void fillContextMenu(IMenuManager manager) {
		manager.add(subscribeAction);
		manager.add(new Separator());
		// Other plug-ins can contribute their actions here
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	}
	
	private void fillLocalToolBar(IToolBarManager manager) {
		manager.add(refreshAction);
		manager.add(startMonitoringAction);
		manager.add(new Separator());
	}

	private void makeActions() {
		refreshAction = new Action() {
			public void run() {
				try {
					EventModel.getInstance().getChannelStatistics();
				} catch (Exception e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
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
		
		
		try {
			subscribeAction = new SubscribeToChannelAction(getSite().getWorkbenchWindow());
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		try {
			subscribeToAllAction = new SubscribeToAllChannelsAction();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
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
				public void run() { // TODO: Should this method be synchronized?
					final Display display = viewer.getControl().getDisplay();
					if (!display.isDisposed()) {
						viewer.refresh();
						viewer.expandAll();
					}
				}
			};
			
			public void run() {
				final Display display = viewer.getControl().getDisplay();

				while (Application.isMonitoring() && !Thread.currentThread().isInterrupted()) {

					try {
						EventModel.getInstance().getChannelStatistics();
						if (!display.isDisposed())
							display.asyncExec(r);

						Thread.sleep(howOften);
						System.out.println("Channel monitoring iteration "+ ++i);
					} catch (InterruptedException e) {
						System.out.println("Channel monitoring was interrupted!");
						break;
					} catch (SWTException e) {
						// eat it
						break;
					} catch (Exception e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
				}
				Application.setMonitoring(false);
				startMonitoringAction.setEnabled(true);
			}
		};
		channelTreeThread = new Thread(t,"Channel Tree");
		channelTreeThread.start();
	}
	
	public void dispose() {
		Platform.getAdapterManager().unregisterAdapters(adapterFactory);
		super.dispose();
	}

}