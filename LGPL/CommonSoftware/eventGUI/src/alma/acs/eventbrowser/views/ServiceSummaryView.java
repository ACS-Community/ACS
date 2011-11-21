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


import java.util.ArrayList;
import java.util.Arrays;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import alma.acs.eventbrowser.Application;
import alma.acs.eventbrowser.model.AbstractNotifyServiceElement;
import alma.acs.eventbrowser.model.ChannelData;
import alma.acs.eventbrowser.model.EventModel;
import alma.acs.eventbrowser.model.NotifyServiceData;
import alma.acs.exceptions.AcsJException;


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

public class ServiceSummaryView extends ViewPart {
	private TableViewer viewer;
	private Action refresh;
	private Action startMonitoringAction;
	private Action doubleClickAction;
	
	private EventModel em;

	private long howOften = 10000l;
	
	public static final String ID = "alma.acs.eventbrowser.views.servicesummary";
	private Thread serviceMonitoringThread;

	/*
	 * The content provider class is responsible for
	 * providing objects to the view. It can wrap
	 * existing objects in adapters or simply return
	 * objects as-is. These objects may be sensitive
	 * to the current input of the view, or ignore
	 * it and always show the same content 
	 * (like Task List, for example).
	 */
	 
	class ViewContentProvider implements IStructuredContentProvider {
		public void inputChanged(Viewer v, Object oldInput, Object newInput) {
		}
		public void dispose() {
			if (serviceMonitoringThread != null) serviceMonitoringThread.interrupt();
		}
		public Object[] getElements(Object parent) {
			NotifyServiceData[] a = new NotifyServiceData[4];
			ArrayList<NotifyServiceData> cdlist = null;
			try {
				cdlist = em.getNotifyServiceTotals();
			} catch (AcsJException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			NotifyServiceData[] l;
			if (cdlist != null)
				l = cdlist.toArray(a);
			else
				return new NotifyServiceData[]{};
			Arrays.sort(l);
			return l;
		}
	}
	
	static class ServiceNameLabelProvider extends ColumnLabelProvider {

		@Override
		public String getText(Object element) {
			if (element instanceof NotifyServiceData)
				return ((NotifyServiceData)element).getName();
			return "";
		}

		@Override
		public Image getImage(Object element) {
			return null;
		}
		
	}
	
	static class NumConsumersLabelProvider extends ColumnLabelProvider {
		
		@Override
		public String getText(Object element) {
			if (element instanceof NotifyServiceData) 
				return ""+((NotifyServiceData)element).getNumberConsumers();
			else
				return "";
		}
		
		@Override
		public Image getImage(Object element) {
			return null;
		}
		
	}
	
	static class NumSuppliersLabelProvider extends ColumnLabelProvider {
		
		@Override
		public String getText(Object element) {
			if (element instanceof NotifyServiceData) 
				return ""+((NotifyServiceData)element).getNumberSuppliers();
			else
				return "";
		}
		
		@Override
		public Image getImage(Object element) {
			return null;
		}
	}
	
	static class ServiceViewerComparator extends ViewerComparator {

		@Override
		public int compare(Viewer viewer, Object e1, Object e2) {
			String s1 = ((NotifyServiceData)e1).getName();
			String s2 = ((NotifyServiceData)e2).getName();
			return s1.compareTo(s2);
		}
		
	}

	/**
	 * The constructor.
	 */
	public ServiceSummaryView() {
	}

	/**
	 * This is a callback that will allow us
	 * to create the viewer and initialize it.
	 */
	public void createPartControl(Composite parent) {
		try {
			em = EventModel.getInstance();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		viewer = new TableViewer(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		
		Table table = viewer.getTable();
		table.setHeaderVisible(true);
		table.setLinesVisible(true);
		
		TableViewerColumn tvcol = new TableViewerColumn(viewer,SWT.NONE, 0);
		tvcol.setLabelProvider(new ServiceNameLabelProvider());
		TableColumn col = tvcol.getColumn();
		col.setText("Notify Service");
		col.setWidth(110);
		col.setAlignment(SWT.LEFT);

		tvcol = new TableViewerColumn(viewer,SWT.NONE, 1);
		tvcol.setLabelProvider(new NumConsumersLabelProvider());
		col = tvcol.getColumn();
		col.setText("#consumers");
		col.setWidth(50);
		col.setAlignment(SWT.LEFT);
		tvcol = new TableViewerColumn(viewer,SWT.NONE, 2);
		tvcol.setLabelProvider(new NumSuppliersLabelProvider());
		col = tvcol.getColumn();
		col.setText("#suppliers");
		col.setWidth(50);
		col.setAlignment(SWT.LEFT);
		
		viewer.setContentProvider(new ViewContentProvider());
		viewer.setComparator(new ServiceViewerComparator());
		viewer.setInput(getViewSite());

		makeActions();
		hookContextMenu();
		hookDoubleClickAction();
		contributeToActionBars();
		if (Application.isMonitoring()) {
			startMonitoringAction.setEnabled(false);
			startMonitoring();
		}
	}
	

	private void hookContextMenu() {
		MenuManager menuMgr = new MenuManager("#PopupMenu");
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {
			public void menuAboutToShow(IMenuManager manager) {
				ServiceSummaryView.this.fillContextMenu(manager);
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
		manager.add(refresh);
		manager.add(new Separator());
		manager.add(startMonitoringAction);
	}

	private void fillContextMenu(IMenuManager manager) {
		manager.add(refresh);
		manager.add(startMonitoringAction);
		// Other plug-ins can contribute there actions here
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	}
	
	private void fillLocalToolBar(IToolBarManager manager) {
		manager.add(refresh);
		manager.add(startMonitoringAction);
	}

	private void makeActions() {
		refresh = new Action() {
			public void run() {
				viewer.refresh();
				//showMessage("Refresh executed");
			}
		};
		refresh.setText("Refresh");
		refresh.setToolTipText("Update the statistics");
		refresh.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages().
			getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
		
		startMonitoringAction = new Action() {
			public void run() {
				startMonitoring();
				setEnabled(false);
//				showMessage("Monitoring started");
			}
		};
		startMonitoringAction.setText("Start monitoring");
		startMonitoringAction.setToolTipText("Begin periodic updating of service data");
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
			"Notify Service Summary",
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
					}
				}
			};


			
			public void run() {
				final Display display = viewer.getControl().getDisplay();

				while (Application.isMonitoring() && !serviceMonitoringThread.isInterrupted()) {
					if (!display.isDisposed())
						display.asyncExec(r);
					try {
						Thread.sleep(howOften);
						System.out.println("Service monitoring iteration "+ ++i);
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
		serviceMonitoringThread = new Thread(t,"ServiceMonitoring");
		serviceMonitoringThread.start();
	}

}