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

import java.util.Locale;
import java.util.logging.Logger;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.inject.Inject;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.e4.core.contexts.IEclipseContext;
import org.eclipse.e4.core.services.events.IEventBroker;
import org.eclipse.e4.core.services.statusreporter.StatusReporter;
import org.eclipse.e4.ui.di.Focus;
import org.eclipse.e4.ui.di.UISynchronize;
import org.eclipse.e4.ui.model.application.ui.menu.MPopupMenu;
import org.eclipse.e4.ui.workbench.modeling.ESelectionService;
import org.eclipse.e4.ui.workbench.swt.modeling.EMenuService;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;

import alma.acs.nsstatistics.EventData;
import alma.acs.nsstatistics.EventModel;
import alma.acs.eventbrowser.status.StatusLineWriter;


/**
 * TODO: check http://tomsondev.bestsolution.at/2011/10/07/jface-viewer-and-eclipse-databinding-with-10-000-objects/
 */
public class EventListPart implements IEventListPart {

	/**
	 * We publish a single selected table row to be used by the event details part. 
	 */
	@Inject
	private ESelectionService selectionService;
	
	@Inject 
	private UISynchronize uiSync;

	private TableViewer viewer;
	private ViewerFilter tableFilter;

	private EventModel em;
	
	private PopulateEventList pel;
	private Logger logger;
	
	/**
	 * Blocking (popup) status report.
	 */
	@Inject 
	private StatusReporter statusReporter;

	private Thread eventListThread;


	/**
	 * The constructor.
	 */
	public EventListPart() {
	}
	
	/**
	 */
	@PostConstruct
	public void postConstruct(Composite parent, final IEclipseContext context, IEventBroker eventBroker, EMenuService menuService) {
		try {
			em = EventModel.getInstance();
		} catch (Throwable thr) {
			thr.printStackTrace();
			IStatus someStatus = statusReporter.newStatus(IStatus.ERROR, "Connection with NCs failed.", thr);
			statusReporter.report(someStatus, StatusReporter.SHOW);
			throw new RuntimeException(thr);
		}

		logger = em.getLogger();
		
		GridLayout gridLayout = new GridLayout();
		gridLayout.marginHeight = 0;
		gridLayout.marginWidth = 0;
		gridLayout.verticalSpacing = 0;
		parent.setLayout(gridLayout);
		
		// TODO: We currently have the filter text control in the regular view toolbar.
		//       The e3 eventGUI had a "custom tool bar" inserted here.
		//       We should decide which way it's better.
		
		viewer = new TableViewer(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL | SWT.FULL_SELECTION);
		Table table = viewer.getTable();
		table.setHeaderVisible(true);
		table.setLinesVisible(true);

		/*
		 * "Time "+timeStamp+" "+m_channelName+" "+component+" "+count+"
		 * "+channelEventCount+" " +" "+evtTypeName+"
		 * "+evtCounter.get(evtTypeName)
		 */

		TableViewerColumn tvcol = new TableViewerColumn(viewer, SWT.NONE, 0);
		tvcol.setLabelProvider(new TimeStampLabelProvider());
		TableColumn col = tvcol.getColumn();
		col.setText("Timestamp");
		col.setWidth(180);
		col.setAlignment(SWT.LEFT);

		tvcol = new TableViewerColumn(viewer, SWT.NONE, 1);
		tvcol.setLabelProvider(new EventSourceLabelProvider());
		col = tvcol.getColumn();
		col.setText("Event source");
		col.setWidth(150);
		col.setAlignment(SWT.LEFT);

		tvcol = new TableViewerColumn(viewer, SWT.NONE, 2);
		tvcol.setLabelProvider(new CountLabelProvider());
		col = tvcol.getColumn();
		col.setText("# Events in channel");
		col.setWidth(50);
		col.setAlignment(SWT.LEFT);

		tvcol = new TableViewerColumn(viewer, SWT.NONE, 3);
		tvcol.setLabelProvider(new EventTypeLabelProvider());
		col = tvcol.getColumn();
		col.setText("Event type");
		col.setWidth(150);
		col.setAlignment(SWT.LEFT);

		tvcol = new TableViewerColumn(viewer, SWT.NONE, 4);
		tvcol.setLabelProvider(new EventTypeCountLabelProvider());
		col = tvcol.getColumn();
		col.setText("# Events this type");
		col.setWidth(50);
		col.setAlignment(SWT.LEFT);

		GridDataFactory.fillDefaults().grab(true, true).applyTo(viewer.getTable());

		viewer.setContentProvider(new EventListViewContentProvider(em));
		
		// Attach a selection listener to our event list that will post the selected event for the event details list
		viewer.addSelectionChangedListener(new ISelectionChangedListener() {
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				IStructuredSelection selection = (IStructuredSelection) event.getSelection();
				if (selection.size() == 1) {
					selectionService.setSelection(selection.getFirstElement());
				}
			}
		});
		
		viewer.setInput(new Object());

		hookContextMenu(menuService);
		
		pel = new PopulateEventList(logger, viewer, new StatusLineWriter(eventBroker), em.getEventQueue(), "NC Events");
		eventListThread = pel.getThreadForEventList();
		eventListThread.start();
	}
	

	/**
	 * @see alma.acs.eventGui2.parts.IEventListPart#notifyEventTypeFilterChange(java.lang.String)
	 */
	@Override
	public void notifyEventTypeFilterChanged(final String filterText) {
//		System.out.println("EventListPart#notifyEventTypeFilterChange : " + filterText);
		if (tableFilter != null) {
			viewer.removeFilter(tableFilter);
		}
		tableFilter = new ViewerFilter() {
			@Override
			public boolean select(Viewer viewer, Object parentElement,
					Object element) {
				if (filterText.equals("")) {
					return true;
				}
				EventData row = (EventData)element;
				String column = row.getEventTypeName();
				if (column.toUpperCase(Locale.ENGLISH).contains(filterText.toUpperCase(Locale.ENGLISH))) {
					return true;
				}
				return false;
			}
		};
		viewer.addFilter(tableFilter);
		viewer.refresh();
	}

	private void hookContextMenu(EMenuService menuService) {
		// For the case of popup (mouse) menues we actually reference the menu ID from the Application.e4xmi
		MPopupMenu menu = menuService.registerContextMenu(viewer.getTable(), "alma.acs.eventgui.popupmenu.eventlist");
		if (menu == null) {
			System.out.println("EventListPart popup menu is null!");
		}
	}
	
	@Override
	public void clearList() {
		// TODO: This asyncExec from the e3 impl is suspicious. Probably should run this as a Job in a non-UI thread and from there update the UI
		uiSync.asyncExec(new Runnable() {
			public void run() {
				viewer.getTable().removeAll();
				viewer.refresh();
			};
		});
	}

	@Focus
	public void setFocus() {
		viewer.getTable().setFocus();
	}
		
	@PreDestroy
	public void preDestroy() {
		eventListThread.interrupt();
	}

}
