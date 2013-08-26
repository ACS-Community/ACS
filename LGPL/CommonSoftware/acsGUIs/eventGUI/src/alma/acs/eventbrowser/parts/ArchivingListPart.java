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

import java.util.logging.Logger;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.inject.Inject;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.e4.core.services.events.IEventBroker;
import org.eclipse.e4.core.services.statusreporter.StatusReporter;
import org.eclipse.e4.ui.di.Focus;
import org.eclipse.e4.ui.di.UISynchronize;
import org.eclipse.jface.layout.GridDataFactory;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TableViewerColumn;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;

import alma.acs.eventbrowser.model.ArchiveEventData;
import alma.acs.eventbrowser.model.EventModel;
import alma.acs.eventbrowser.status.StatusLineWriter;

/**
 * The "archiving list" displays events from the archiving NC, which contain monitoring data from baci properties.
 * The event data format is different from "normal" NCs.
 * <p>
 * Currently this view is simpler than the EventListPart, without view/context menu nor event details.
 * This means that there is no way to show the values of array type events besides something like "[F@2445d7".
 * Alma is not using this NC monitoring/archiving mechanism any more, so that we are not likely to enhance this view further.
 */
public class ArchivingListPart implements IEventListPart {

	@Inject 
	private UISynchronize uiSync;

	private TableViewer viewer;

	private EventModel em;
	
	private PopulateEventList pel;
	
	private Logger logger;

	/**
	 * Blocking (popup) status report.
	 */
	@Inject 
	private StatusReporter statusReporter;

	private Thread eventListThread;

	
	public ArchivingListPart() {
	}

	@PostConstruct
	public void postConstruct(Composite parent, IEventBroker eventBroker) {
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
		
//		buildCustomToolBar(parent);
		
		viewer = new TableViewer(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
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
		col.setWidth(190);
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
		col.setText("Property");
		col.setWidth(100);
		col.setAlignment(SWT.LEFT);

		tvcol = new TableViewerColumn(viewer, SWT.NONE, 3);
		tvcol.setLabelProvider(new BaciPropertyValueLabelProvider());
		col = tvcol.getColumn();
		col.setText("Value");
		col.setWidth(100);
		col.setAlignment(SWT.LEFT);

		GridDataFactory.fillDefaults().grab(true, true).applyTo(viewer.getTable());

		viewer.setContentProvider(new ArchivingListContentProvider(em));
		viewer.setInput(new Object());

		pel = new PopulateEventList(logger, viewer, new StatusLineWriter(eventBroker), em.getArchQueue(), "Monitor points");
		
		eventListThread = pel.getThreadForEventList();
		eventListThread.start();
	}
	
	
	/**
	 * Currently (when porting eventGUI from e3 to e4), there is no filtering
	 * available for the archive list. 
	 * We could have just not implemented IEventListPart, but anyway put an empty 
	 * implementation here to show how this archiving list could also have a ToolBar with an EventListFilterToolControl.
	 * 
	 * @see alma.acs.eventGui2.parts.IEventListPart#notifyEventTypeFilterChange(java.lang.String)
	 */
	@Override
	public void notifyEventTypeFilterChanged(final String filterText) {
	}


	/** 
	 * Currently not called (there is no corresponding menu / command).
	 * 
	 * @see alma.acs.eventbrowser.parts.IEventListPart#clearList()
	 */
	@Override
	public void clearList() {
		viewer.getTable().removeAll();
		viewer.refresh();
	}

	@Focus
	public void setFocus() {
		viewer.getTable().setFocus();
	}
		
	@PreDestroy
	public void preDestroy() {
		eventListThread.interrupt();
		em.closeArchiveConsumer();
		logger.info("Average archiving rate: "+ArchiveEventData.getAverageRate()+" monitor points/s");
	}
	
	
}
