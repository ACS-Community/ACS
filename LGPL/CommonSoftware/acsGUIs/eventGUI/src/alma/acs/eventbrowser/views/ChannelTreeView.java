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
import org.eclipse.jface.viewers.ViewerSorter;
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
	private DrillDownAdapter drillDownAdapter;
	private Action refreshAction;
	private Action startMonitoringAction;
	
	private IAdapterFactory adapterFactory = new EventGuiAdapterFactory();
		
	private EventModel em;
	private long howOften = 10000l; // Default is every 10 seconds
	
	public static final String ID = "alma.acs.eventbrowser.views.channeltree";
	private Thread channelTreeThread;
	private SubscribeToChannelAction subscribeAction;

	/*
	 * The content provider class is responsible for
	 * providing objects to the view. It can wrap
	 * existing objects in adapters or simply return
	 * objects as-is. These objects may be sensitive
	 * to the current input of the view, or ignore
	 * it and always show the same content 
	 * (like Task List, for example).
	 */
	 

	static class NameSorter extends ViewerSorter {
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
		Platform.getAdapterManager().registerAdapters(adapterFactory,ChannelData.class);
		Platform.getAdapterManager().registerAdapters(adapterFactory,NotifyServiceData.class);
		Platform.getAdapterManager().registerAdapters(adapterFactory,NotifyServices.class);
		Platform.getAdapterManager().registerAdapters(adapterFactory,MCStatistics.class);

		drillDownAdapter = new DrillDownAdapter(viewer);
		viewer.setContentProvider(new BaseWorkbenchContentProvider());
		//viewer.setLabelProvider(new ViewLabelProvider());
		viewer.setLabelProvider(new WorkbenchLabelProvider());
		viewer.setSorter(new NameSorter());
		try {
			em = EventModel.getInstance();
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
	}

	private void fillContextMenu(IMenuManager manager) {
		manager.add(subscribeAction);
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
				//vcp.initialize();
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

		
//		MenuManager menuMgr = new MenuManager("#contactsPopup");
//		menuMgr.add(subscribeAction);
//		Menu menu = menuMgr.createContextMenu(viewer.getControl());
//		viewer.getControl().setMenu(menu);
//		getSite().registerContextMenu(menuMgr, viewer);
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
				public void run() { // TODO: Should this method be synchronized
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
						System.out.println("Iteration "+ ++i);
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