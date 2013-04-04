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

package alma.acs.eventbrowser.handlers;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.e4.core.services.events.IEventBroker;
import org.eclipse.e4.ui.di.UISynchronize;

import alma.acs.eventbrowser.model.EventModel;
import alma.acs.eventbrowser.status.StatusLineWriter;

/**
 * Job that asynchronously updates the domain data from the NotifyService processes
 * and then notifies the UI elements via the IEventBroker signal {@link #REFRESH_UI_SIGNAL_NAME}.
 * 
 * @author hsommer
 */
public class NotifyServiceUpdateJob extends Job {

	public final static String REFRESH_UI_SIGNAL_NAME = "refreshUI";
	
	private final EventModel eventModel;
	private final StatusLineWriter statusLineWriter;
	private final UISynchronize sync;
	private final IEventBroker broker; 
	private final long refreshDelayMillis;
	private int iterationCount = 0;

	
	/**
	 * Single run job
	 * @param eventModel
	 * @param sync
	 * @param statusLineWriter
	 */
	public NotifyServiceUpdateJob(EventModel eventModel, UISynchronize sync, StatusLineWriter statusLineWriter, IEventBroker broker) {
		this(eventModel, sync, statusLineWriter, broker, -1);
	}

	/**
	 * Periodic job
	 * @param eventModel
	 * @param sync
	 * @param statusLineWriter
	 * @param refreshDelayMillis
	 */
	public NotifyServiceUpdateJob(EventModel eventModel, UISynchronize sync, StatusLineWriter statusLineWriter, IEventBroker broker, long refreshDelayMillis) {
		super(PeriodicRefreshHandler.class.getName() + "Job");
		this.eventModel = eventModel;
		this.sync = sync;
		this.statusLineWriter = statusLineWriter;
		this.broker = broker;
		this.refreshDelayMillis = refreshDelayMillis;
	}

	@Override
	protected IStatus run(IProgressMonitor monitor) {
		
//		System.out.println("UpdateJob#run called, with monitor " + monitor.getClass()); // expecting 'NullProgressMonitor'
		iterationCount++;
		
		// the server call
		monitor.beginTask("serverupdate" + iterationCount, 100);
		// TODO: break up server call into fine-grained calls, check "monitor.isCanceled()" in between calls.
		eventModel.getChannelStatistics();
		monitor.worked(100);
		
		// success message and refresh for UI
		sync.asyncExec(new Runnable() {
			@Override
			public void run() {
				statusLineWriter.flashMessage("Updated notify service data from the server(s)", 1); // todo: add elapsed time
				
				// clients that use the UIEventTopic annotation are anyway called in the UI thread,
				// but to be safe we publish the UI refresh event also from the UI thread...
				broker.send(REFRESH_UI_SIGNAL_NAME, null);
			}
		});
		
		// re-schedule this job if it is periodic
		if (refreshDelayMillis > 0 && !monitor.isCanceled()) {
			schedule(refreshDelayMillis);
		}
		
		monitor.done();
//		System.out.println("Done with NotifyServiceUpdateJob" + ( refreshDelayMillis > 0 ? " iteration=" + iterationCount : "") );
		
		return ( monitor.isCanceled() ? Status.CANCEL_STATUS : Status.OK_STATUS );
	}
}
