/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2013
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
package alma.acs.eventbrowser.parts;


import java.util.Arrays;
import java.util.List;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.inject.Inject;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.e4.core.di.annotations.Optional;
import org.eclipse.e4.core.services.statusreporter.StatusReporter;
import org.eclipse.e4.ui.di.Focus;
import org.eclipse.e4.ui.di.UIEventTopic;
import org.eclipse.e4.ui.model.application.ui.menu.MPopupMenu;
import org.eclipse.e4.ui.workbench.swt.modeling.EMenuService;
import org.eclipse.jface.viewers.ColumnLabelProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;

import alma.acs.eventbrowser.handlers.NotifyServiceUpdateJob;
import alma.acs.nsstatistics.EventModel;
import alma.acs.nsstatistics.NotifyServiceData;



public class ServiceSummaryPart {
	
	/**
	 * Blocking (popup) status report.
	 */
	@Inject 
	private StatusReporter statusReporter;
	

	/**
	 * TableViewer comes from JFace, allows access to embedded SWT Table.
	 * http://www.vogella.com/articles/EclipseJFaceTable/article.html
	 */
	private TableViewer viewer;

	/**
	 * TODO: The EventModel instance should get injected by the Eclipse DI container,
	 * either as an OSGI service or as a node from the application model.
	 * We retrieve the NotifyServices from the EventModel.
	 */
	private EventModel eventModel; 

	private Thread serviceMonitoringThread;

	
	/**
	 * The content provider class is responsible for providing objects to the view. 
	 * It can wrap existing objects in adapters or simply return objects as-is. 
	 * These objects may be sensitive to the current input of the view,
	 * or ignore it and always show the same content (like Task List, for example).
	 */
	private class ViewContentProvider implements IStructuredContentProvider {
		public void inputChanged(Viewer v, Object oldInput, Object newInput) {
		}
		public void dispose() {
			if (serviceMonitoringThread != null) serviceMonitoringThread.interrupt();
		}
		public Object[] getElements(Object parent) {
			List<NotifyServiceData> cdlist = null;
			cdlist = eventModel.getNotifyServicesRoot().getServices();
			NotifyServiceData[] l;
			if (cdlist != null)
				l = cdlist.toArray(new NotifyServiceData[cdlist.size()]);
			else
				return new NotifyServiceData[]{};
			Arrays.sort(l);
			return l;
		}
	}
	
	private static class ServiceNameLabelProvider extends ColumnLabelProvider {

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
	
	private static class NumConsumersLabelProvider extends ColumnLabelProvider {
		
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
	
	private static class NumSuppliersLabelProvider extends ColumnLabelProvider {
		
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
	

	@PostConstruct
	public void createComposite(Composite parent, EMenuService menuService) {
		try {
			eventModel = EventModel.getInstance();
		} catch (Throwable thr) {
			thr.printStackTrace();
			IStatus someStatus = statusReporter.newStatus(IStatus.ERROR, "Connection with NCs failed.", thr);
			statusReporter.report(someStatus, StatusReporter.SHOW);
			throw new RuntimeException(thr);
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
		viewer.setInput(new Object()); //was: getViewSite() -- the dummy arg seems to become the input arg for ViewContentProvider#getElements(Object)

		hookContextMenu(menuService);
		
	}

	private void hookContextMenu(EMenuService menuService) {
		// For the case of popup (mouse) menues we actually reference the menu ID from the Application.e4xmi
		MPopupMenu menu = menuService.registerContextMenu(viewer.getTable(), "alma.acs.eventgui.popupmenu.servicesummary");
		if (menu == null) {
			System.out.println("ServiceSummaryPart popup menu is null!");
		}
	}

// TODO: Implement double-click, beyond the e3 code
//	doubleClickAction = new Action() {
//		public void run() {
//			ISelection selection = viewer.getSelection();
//			Object obj = ((IStructuredSelection)selection).getFirstElement();
//			showMessage("Double-click detected on "+obj.toString());
//		}
//	};
	
	/**
	 * The keybinding implementation requires that all parts correctly implement @Focus. Eclipse requires that one control get the focus assigned. 
	 */
	@Focus
	public void setFocus() {
		viewer.getTable().setFocus();
	}
	
	
	@PreDestroy
	public void dispose() {
	}

	/**
	 * @param s
	 * @see NotifyServiceUpdateJob
	 */
	@Inject
	@Optional
	private void refreshNotify(@UIEventTopic(NotifyServiceUpdateJob.REFRESH_UI_SIGNAL_NAME) String s) {
//		System.out.println("ServiceSummaryPart#refreshNotify() called via IEventBroker");
		viewer.refresh();
	}

}
