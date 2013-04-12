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

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.inject.Inject;
import javax.inject.Named;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.e4.core.di.annotations.CanExecute;
import org.eclipse.e4.core.di.annotations.Execute;
import org.eclipse.e4.core.di.annotations.Optional;
import org.eclipse.e4.core.services.statusreporter.StatusReporter;
import org.eclipse.e4.ui.di.UISynchronize;
import org.eclipse.e4.ui.model.application.ui.menu.MHandledItem;
import org.eclipse.e4.ui.services.IServiceConstants;

import alma.acs.eventbrowser.model.ChannelData;
import alma.acs.eventbrowser.model.EventModel;
import alma.acs.exceptions.AcsJException;

/**
 * 
 */
public class SubscribeNCHandler {
	
	@Inject 
	private UISynchronize uiSync;
	
	/**
	 * Blocking (popup) status report.
	 */
	@Inject 
	private StatusReporter statusReporter;
	

	private EventModel eventModel;


	/**
	 * TODO: Inject UI-specific logger instead of using the one from EventModel
	 */
	private Logger logger;

	/**
	 * Subscribes / unsubscribes to/from an NC in a non-UI thread.
	 */
	private abstract class SubscriptionChangeJob extends Job {
		
		protected final ChannelData nc;
		
		SubscriptionChangeJob(ChannelData nc, String jobname) {
			super(SubscriptionChangeJob.class.getSimpleName());
			this.nc = nc;
		}
		
		protected abstract void changeSubscription() throws AcsJException;
		
		@Override
		protected IStatus run(IProgressMonitor monitor) {
			try {
				changeSubscription();
			} catch (final Exception ex) {
				ex.printStackTrace();
				uiSync.syncExec(new Runnable() {
					public void run() {
						IStatus someStatus = statusReporter.newStatus(IStatus.ERROR, "Subscription to NC(s) failed.", ex);
						statusReporter.report(someStatus, StatusReporter.SHOW);
					}
				});
			}
			subscriptionChangeJobMap.remove(nc.getName());
			return Status.OK_STATUS;
		}
	}
	
	private class SubscribeJob extends SubscriptionChangeJob {
		private SubscribeJob(ChannelData nc) {
			super(nc, SubscribeJob.class.getSimpleName());
		}
		@Override
		protected void changeSubscription() throws AcsJException {
			eventModel.addChannelSubscription(nc);
			logger.info("Subscribed to NC " + nc.getName());
		}
	}
	
	private class UnsubscribeJob extends SubscriptionChangeJob {
		private UnsubscribeJob(ChannelData nc) {
			super(nc, UnsubscribeJob.class.getSimpleName());
		}
		@Override
		protected void changeSubscription() throws AcsJException {
			eventModel.closeSelectedConsumer(nc);
			logger.info("Unsubscribed from NC " + nc.getName());
		}
	}
	
	/**
	 * This map contains job subscriptions in progress, or is empty otherwise.
	 * <p>
	 * We could just as well loop over multiple subscriptions in a single job,
	 * because the eclipse job framework seems to run only one job at a time.
	 */
	private final Map<String, SubscriptionChangeJob> subscriptionChangeJobMap = Collections.synchronizedMap(new HashMap<String, SubscriptionChangeJob>());
	
	
	@PostConstruct
	public void init() {
		try {
			eventModel = EventModel.getInstance();
		} catch (Throwable thr) {
			thr.printStackTrace();
			IStatus someStatus = statusReporter.newStatus(IStatus.ERROR, "Connection with NCs failed.", thr);
			statusReporter.report(someStatus, StatusReporter.SHOW);
			throw new RuntimeException(thr);
		}
		logger = eventModel.getLogger();
	}

	@CanExecute
	public boolean canExecute(@Optional @Named(IServiceConstants.ACTIVE_SELECTION) Object obj, MHandledItem handledItem) {
		boolean canExecute = false;
		if (subscriptionChangeJobMap.isEmpty()) { // disable while previous subscription change actions are still running
			List<ChannelData> ncList = getSelectedNCs(obj);
			if (!ncList.isEmpty()) {
				canExecute = true;
				// if multiple NCs are selected, they must all be in the same subscription state,
				// otherwise we don't allow the subscription change
				Boolean sharedIsSubscribed = null;
				for (ChannelData nc : ncList) {
					// all selected NCs must be subscribable
					if (!nc.isSubscribable()) {
						canExecute = false;
						break;
					}
					
					if (sharedIsSubscribed == null) {
						// the first NC we are checking
						sharedIsSubscribed = new Boolean(eventModel.isSubscribed(nc));
					}
					else {
						if (eventModel.isSubscribed(nc) != sharedIsSubscribed.booleanValue()) {
							canExecute = false;
							break;
						}
					}
				}
				// The CHECK context menu item only holds one 'selected' state.
				// Since we are dealing with different NCs, the real state is in the domain model (em.isSubscribed)
				// We dynamically update the menu item state to display the correct toggle symbol.
				if (canExecute) {
					handledItem.setSelected(sharedIsSubscribed);
				}
			}
		}
		return canExecute;
	}

	/**
	 * Allows one or many NCs to be selected, which is why we use 'Object' instead of 'ChannelData' as the selection.
	 */
	@Execute
	public void execute(@Named(IServiceConstants.ACTIVE_SELECTION) Object obj, MHandledItem handledItem) {
		
		List<ChannelData> ncList = getSelectedNCs(obj);
		for (ChannelData nc : ncList) {
			
			// We simply toggle the subscription, ignoring the CHECK menu item state.
			// In fact we have to manually toggle the menu in the end, since the UI-induced toggling got lost 
			// when eclipse called 'canExecute' right before this method.
			
			boolean previousIsSubscribed = eventModel.isSubscribed(nc);
			SubscriptionChangeJob job = ( previousIsSubscribed ? new UnsubscribeJob(nc) : new SubscribeJob(nc) );
			subscriptionChangeJobMap.put(nc.getName(), job);
			job.schedule();
			
			handledItem.setSelected(!previousIsSubscribed);
		}
	}


	private List<ChannelData> getSelectedNCs(Object selection) {
		List<ChannelData> ret = new ArrayList<ChannelData>();
		if (selection instanceof ChannelData) {
			ret.add((ChannelData)selection);
		}
		else if (selection instanceof Object[]) {
			for (Object obj : (Object[])selection) {
				if (obj instanceof ChannelData) {
					ret.add((ChannelData)obj);
				}
				else {
					// One selected node that is not an NC makes the whole selection invalid.
					// Dropping it silently would be too confusing.
					ret.clear();
					break;
				}
			}
		}
		return ret;
	}
	
	@PreDestroy
	public void dispose() {
	}

}
