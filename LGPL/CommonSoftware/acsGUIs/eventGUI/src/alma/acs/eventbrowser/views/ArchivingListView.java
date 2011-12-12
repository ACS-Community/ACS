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

import java.util.Locale;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.logging.Logger;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

import alma.acs.eventbrowser.Application;
import alma.acs.eventbrowser.model.AdminConsumer;
import alma.acs.eventbrowser.model.EventModel;

public class ArchivingListView extends ViewPart {

	private TableViewer viewer;
	private EventTypeFilter tableFilter;

	private EventModel em;
	private Action clearEvents;
	private Action printEventDetails;
	
	private PopulateEventList pel;
	
	private Logger logger;

	public static final String ID = "alma.acs.eventbrowser.views.archivingList";
	private Thread eventListThread;
	private Thread channelRefreshThread;
	
	private final ArrayBlockingQueue<ArchiveEventData> aqueue = Application.archQueue;
	private IActionBars bars;


	public ArchivingListView() {
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

		logger = em.getLogger();
		
		GridLayout gridLayout = new GridLayout();
		gridLayout.marginHeight = 0;
		gridLayout.marginWidth = 0;
		gridLayout.verticalSpacing = 0;
		parent.setLayout(gridLayout);
		
//		buildCustomToolBar(parent);
		
		viewer = new TableViewer(parent, SWT.MULTI | SWT.H_SCROLL
				| SWT.V_SCROLL);
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
		col.setText("Device");
		col.setWidth(100);
		col.setAlignment(SWT.LEFT);

		tvcol = new TableViewerColumn(viewer, SWT.NONE, 2);
		tvcol.setLabelProvider(new BaciParameterLabelProvider());
		col = tvcol.getColumn();
		col.setText("Parameter");
		col.setWidth(50);
		col.setAlignment(SWT.LEFT);

		tvcol = new TableViewerColumn(viewer, SWT.NONE, 3);
		tvcol.setLabelProvider(new BaciPropertyValueLabelProvider());
		col = tvcol.getColumn();
		col.setText("Value");
		col.setWidth(50);
		col.setAlignment(SWT.LEFT);

//		tvcol = new TableViewerColumn(viewer, SWT.NONE, 4);
//		tvcol.setLabelProvider(new EventTypeCountLabelProvider());
//		col = tvcol.getColumn();
//		col.setText("# Events this type");
//		col.setWidth(50);
//		col.setAlignment(SWT.LEFT);		

		GridDataFactory.fillDefaults().grab(true, true).applyTo(viewer.getTable());

		viewer.setContentProvider(new ArchivingListViewContentProvider());
		// viewer.setComparator(new ServiceViewerComparator());
		getSite().setSelectionProvider(viewer); // In order to be able to display event detail
		viewer.setInput(getViewSite());

//		makeActions();
//		hookContextMenu();
		
		bars = getViewSite().getActionBars();

		pel = new PopulateEventList(logger, viewer, bars, aqueue, "Monitor points");
		
		channelRefreshThread = pel.getChannelRefreshThread(em);
		channelRefreshThread.start();
//		em.refreshChannelSubscriptions(); // TODO: remove workaround
		
		eventListThread = pel.getThreadForEventList();
		eventListThread.start();
	}
	
//	private void buildCustomToolBar(Composite parent) {
//		Composite customToolBar = new Composite(parent, SWT.NONE);
//		customToolBar.setLayout(new RowLayout(SWT.HORIZONTAL));
//		buildTextFilter(customToolBar);
//	}
//	
//	private void buildTextFilter(Composite customToolBar) {
//		final Text text = new Text(customToolBar, SWT.BORDER | SWT.SEARCH);
////		text.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL
////				| GridData.HORIZONTAL_ALIGN_FILL));
//		text.addModifyListener(new ModifyListener() {
//			public void modifyText(ModifyEvent e) {
//				if (tableFilter != null) {
//					viewer.removeFilter(tableFilter);
//				}
//				tableFilter = new EventTypeFilter() {
//
//					@Override
//					public boolean select(Viewer viewer, Object parentElement,
//							Object element) {
//						if (text.getText().trim().equals("")) {
//							return true;
//						}
//						EventData row = (EventData)element;
//						String column = row.getEventTypeName();
//						if (column.toUpperCase(Locale.ENGLISH).contains(text.getText().
//										toUpperCase(Locale.ENGLISH))) {
//							return true;
//						}
//						return false;
//					}
//				};
//
//				viewer.addFilter(tableFilter);
//				viewer.refresh();			
//			}
//		});
//	}
//
//	private void hookContextMenu() {
//		MenuManager menuMgr = new MenuManager("#PopupMenu");
//		menuMgr.setRemoveAllWhenShown(true);
//		menuMgr.addMenuListener(new IMenuListener() {
//			public void menuAboutToShow(IMenuManager manager) {
//				fillContextMenu(manager);
//			}
//		});
//		Menu menu = menuMgr.createContextMenu(viewer.getControl());
//		viewer.getControl().setMenu(menu);
//		getSite().registerContextMenu(menuMgr, viewer);
//	}
//
//	private void fillContextMenu(IMenuManager manager) {
//		manager.add(clearEvents);
//		manager.add(printEventDetails);
//		// Other plug-ins can contribute there actions here
//		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
//	}
//
//	private void makeActions() {
//		clearEvents = new Action() {
//			public void run() {
//				final Display display = viewer.getControl().getDisplay();
//				if (!display.isDisposed())
//					display.asyncExec(new Runnable() {
//						public void run() {
//							viewer.getTable().removeAll();
//							viewer.refresh();
//						};
//					});
//			}
//		};
//		clearEvents.setText("Clear events");
//		clearEvents.setToolTipText("Clear all events from the table");
//		clearEvents.setImageDescriptor(PlatformUI.getWorkbench()
//				.getSharedImages().getImageDescriptor(
//						ISharedImages.IMG_TOOL_DELETE));
//		printEventDetails = new Action() {
//			public void run() {
//				AdminConsumer.setPrintDetails(!AdminConsumer.getPrintDetails());
//			};
//		};
//		printEventDetails.setText("Toggle event details");
//		printEventDetails.setToolTipText("Toggle display of event details to STDOUT");
//
//	}

	@Override
	public void setFocus() {
		viewer.getControl().setFocus();

	}

	@Override
	public void dispose() {
		channelRefreshThread.interrupt();
		eventListThread.interrupt();
		em.closeArchiveConsumer();
		logger.info("Average archiving rate: "+ArchiveEventData.getAverageRate()+" monitor points/s");
		super.dispose();
	}



}
