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

import java.lang.reflect.InvocationTargetException;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.ISelectionService;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import alma.acs.eventbrowser.model.EventModel;

public class EventDetailView extends ViewPart {

	private TableViewer viewer;

	private EventModel em;
	private DynAnyParser parser;
	private Logger logger;

	public static final String ID = "alma.acs.eventbrowser.views.eventdetail";

	public EventDetailView() {
		// TODO Auto-generated constructor stub
	}

	@Override
	public void createPartControl(Composite parent) {
		try {
			em = EventModel.getInstance();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		viewer = new TableViewer(parent, SWT.MULTI | SWT.H_SCROLL
				| SWT.V_SCROLL);

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

		MenuManager menuManager = new MenuManager();
		Menu menu = menuManager.createContextMenu(viewer.getTable());
		// Set the MenuManager
		viewer.getTable().setMenu(menu);
		getSite().registerContextMenu(menuManager, viewer);

		// we're cooperative and also provide our selection
		// at least for the TableViewer
		getSite().setSelectionProvider(viewer);
		logger = em.getLogger();

		ISelectionService selsvc = getSite().getWorkbenchWindow()
				.getSelectionService();
		selsvc.addSelectionListener(
				alma.acs.eventbrowser.views.EventListView.ID, mylistener);

	}

	@Override
	public void setFocus() {
		// TODO Auto-generated method stub

	}

	private ISelectionListener mylistener = new ISelectionListener() {
		public void selectionChanged(IWorkbenchPart sourcepart,
				ISelection selection) {
			if (sourcepart != EventDetailView.this
					&& selection instanceof IStructuredSelection) {
				IStructuredSelection iss = (IStructuredSelection) selection;
				if (iss == null)
					return;
				Object[] arr = iss.toArray();
				if (arr.length > 0 && arr[0] instanceof EventData) {
					EventData ed = (EventData) arr[0];
					showEventDetails(sourcepart, ed);
				}

			}
		}
	};

	public void showEventDetails(IWorkbenchPart sourcepart, final EventData ed) {
		final String eventName = ed.getEventTypeName();
		setContentDescription("Details of " + eventName);
		try {
//			PlatformUI.getWorkbench().getProgressService().busyCursorWhile(
					PlatformUI.getWorkbench().getProgressService().run(true,true,
					new IRunnableWithProgress() {
						public void run(IProgressMonitor monitor) {
							parser = new DynAnyParser(ed.getEventAny(),
									eventName);
							final ParsedAnyData[] results = parser
									.getParsedResults(monitor);
							if (monitor.isCanceled()) return;
							final Display display = viewer.getControl()
									.getDisplay();
							display.asyncExec(new Runnable() {
								public void run() {
									viewer.setInput(results);
								}
							});
						}
					});
		} catch (InvocationTargetException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public void dispose() {
		ISelectionService s = getSite().getWorkbenchWindow()
				.getSelectionService();
		s.removeSelectionListener(mylistener);
		super.dispose();
	}

}
