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

import javax.annotation.PostConstruct;
import javax.inject.Inject;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.e4.core.di.annotations.CanExecute;
import org.eclipse.e4.core.di.annotations.Execute;
import org.eclipse.e4.core.services.events.IEventBroker;
import org.eclipse.e4.core.services.statusreporter.StatusReporter;
import org.eclipse.e4.ui.di.UISynchronize;

import alma.acs.eventbrowser.model.EventModel;
import alma.acs.eventbrowser.status.StatusLineWriter;

/**
 * Refresh handler for parts that implement {@link INotifyServiceRefreshable}.
 * If no such part is active, calls get disabled / ignored.
 */
public class SingleRefreshHandler {

	@Inject 
	private UISynchronize sync;
	
	/**
	 * Blocking (popup) status report.
	 */
	@Inject 
	private StatusReporter statusReporter;
	
	@Inject
	private IEventBroker eventBroker;
	
	private EventModel eventModel;

	private StatusLineWriter statusLineWriter;

	
	@PostConstruct
	public void init() {
//		System.out.println("SingleRefreshHandler#init called.");
		
		try {
			eventModel = EventModel.getInstance();
		} catch (Throwable thr) {
			thr.printStackTrace();
			IStatus someStatus = statusReporter.newStatus(IStatus.ERROR, "Connection with NCs failed.", thr);
			statusReporter.report(someStatus, StatusReporter.SHOW);
			throw new RuntimeException(thr);
		}

		statusLineWriter = new StatusLineWriter(eventBroker);
	}
	
	/**
	 */
	@CanExecute
	public boolean canExecute() {
		return true;
	}

	
	@Execute
	public void execute() {

		new NotifyServiceUpdateJob(eventModel, sync, statusLineWriter, eventBroker).schedule();
		
//		System.out.println("Scheduled single refresh job.");
	}

}
