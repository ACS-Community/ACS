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
package alma.acs.eventbrowser.parts;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.inject.Inject;
import javax.inject.Named;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.e4.core.di.annotations.Optional;
import org.eclipse.e4.core.services.statusreporter.StatusReporter;
import org.eclipse.e4.ui.di.Focus;
import org.eclipse.e4.ui.di.UISynchronize;
import org.eclipse.e4.ui.model.application.ui.menu.MPopupMenu;
import org.eclipse.e4.ui.services.IServiceConstants;
import org.eclipse.e4.ui.workbench.modeling.ESelectionService;
import org.eclipse.e4.ui.workbench.swt.modeling.EMenuService;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;

import alma.acs.eventbrowser.model.EventData;
import alma.acs.eventbrowser.model.EventModel;


/**
 * Displays details of an event selected in the {@link EventListPart}.
 */
public class EventDetailPart {

	@Inject 
	private UISynchronize uiSync;
	
	/**
	 * We publish selected table rows to be used by the mouse menu handler. 
	 */
	@Inject
	private ESelectionService selectionService;
	
	/**
	 * Blocking (popup) status report.
	 */
	@Inject 
	private StatusReporter statusReporter;
	
	private TableViewer viewer;

	private EventModel em;
	private DynAnyParser parser;
	private Logger logger;

	public EventDetailPart() {
	}

	@PostConstruct
	public void createPartControl(Composite parent, EMenuService menuService) {
		try {
			em = EventModel.getInstance();
		} catch (Throwable thr) {
			thr.printStackTrace();
			IStatus someStatus = statusReporter.newStatus(IStatus.ERROR, "Connection with NCs failed.", thr);
			statusReporter.report(someStatus, StatusReporter.SHOW);
			throw new RuntimeException(thr);
		}
		logger = em.getLogger();

		viewer = new TableViewer(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);

		Table table = viewer.getTable();
		table.setHeaderVisible(true);
		table.setLinesVisible(true);

		GridLayout gridLayout = new GridLayout();
		gridLayout.marginHeight = 0;
		gridLayout.marginWidth = 0;
		gridLayout.verticalSpacing = 0;
		parent.setLayout(gridLayout);

		TableViewerColumn tvcol = new TableViewerColumn(viewer, SWT.NONE, 0);
		tvcol.setLabelProvider(new DetailNameLabelProvider());
		TableColumn col = tvcol.getColumn();
		col.setText("Name");
		col.setWidth(180);
		col.setAlignment(SWT.LEFT);

		tvcol = new TableViewerColumn(viewer, SWT.NONE, 1);
		tvcol.setLabelProvider(new DetailTypeLabelProvider());
		col = tvcol.getColumn();
		col.setText("Type");
		col.setWidth(100);
		col.setAlignment(SWT.LEFT);

		tvcol = new TableViewerColumn(viewer, SWT.NONE, 2);
		tvcol.setLabelProvider(new DetailValueLabelProvider());
		col = tvcol.getColumn();
		col.setText("Value");
		col.setWidth(50);
		col.setAlignment(SWT.LEFT);

		GridDataFactory.fillDefaults().grab(true, true).applyTo(
				viewer.getTable());

		viewer.setContentProvider(new DetailContentProvider());

		hookContextMenu(menuService);
		
		// Attach a selection listener to our table, which will post selections to the ESelectionService
		// to be processed by CopyDetailsToClipboardHandler
		viewer.addSelectionChangedListener(new ISelectionChangedListener() {
			@Override
			public void selectionChanged(SelectionChangedEvent event) {
				IStructuredSelection selection = (IStructuredSelection) event.getSelection();
				
				List<ParsedAnyData> parsedAnyList = new ArrayList<ParsedAnyData>();
				for (Iterator<?> iterator = selection.iterator(); iterator.hasNext();) {
					ParsedAnyData parsedAny = (ParsedAnyData)iterator.next();
					parsedAnyList.add(parsedAny);
				}
				selectionService.setSelection(parsedAnyList.toArray(new ParsedAnyData[0]));
			}
		});
	}
	
	
	private void hookContextMenu(EMenuService menuService) {
		// For the case of popup (mouse) menus we must reference the menu ID from the file Application.e4xmi
		MPopupMenu menu = menuService.registerContextMenu(viewer.getTable(), "alma.acs.eventgui.popupmenu.eventdetail");
		if (menu == null) {
			System.out.println("Damn, failed to register popup menu for the EventDetailPart table.");
		}
	}
	
	/**
	 * Here we listen to event list selections.
	 * This gets called by the selection service.
	 */
	@Inject
	public void setEvent(@Optional @Named(IServiceConstants.ACTIVE_SELECTION) EventData eventData) {
		if (eventData != null) {
			new ParseAndDisplayEventDetailsJob(eventData).schedule();
		}
	}

	private class ParseAndDisplayEventDetailsJob extends Job {
		private EventData eventData;
		
		public ParseAndDisplayEventDetailsJob(EventData eventData) {
			super(ParseAndDisplayEventDetailsJob.class.getSimpleName());
			this.eventData = eventData;
		}
		
		@Override
		protected IStatus run(IProgressMonitor monitor) {
			// todo: use hourglass cursor etc?
			
			final String eventName = eventData.getEventTypeName();
			// TODO: port this to e4
//			setContentDescription("Details of " + eventName);

			parser = new DynAnyParser(em.getDynAnyFactory(), eventData.getEventAny(), eventName);
			final ParsedAnyData[] results = parser.getParsedResults(monitor);
			if (monitor.isCanceled()) {
				return Status.CANCEL_STATUS;
			}
			uiSync.syncExec(new Runnable() {
				public void run() {
					viewer.setInput(results);
				}
			});
			return Status.OK_STATUS;
		}
	}
	
	@Focus
	public void setFocus() {
		viewer.getTable().setFocus();
	}


	@PreDestroy
	public void dispose() {
	}

}
