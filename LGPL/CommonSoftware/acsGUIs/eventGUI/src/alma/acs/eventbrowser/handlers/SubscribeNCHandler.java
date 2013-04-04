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
import java.util.List;
import java.util.logging.Logger;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.inject.Inject;
import javax.inject.Named;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.e4.core.di.annotations.CanExecute;
import org.eclipse.e4.core.di.annotations.Execute;
import org.eclipse.e4.core.di.annotations.Optional;
import org.eclipse.e4.core.services.statusreporter.StatusReporter;
import org.eclipse.e4.ui.model.application.ui.menu.MHandledItem;
import org.eclipse.e4.ui.services.IServiceConstants;

import alma.acs.eventbrowser.model.ChannelData;
import alma.acs.eventbrowser.model.EventModel;

/**
 * Refactored from 'SubscribeToChannelAction'
 */
public class SubscribeNCHandler {
	
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
		List<ChannelData> ncList = getSelectedNCs(obj);
		if (!ncList.isEmpty()) {
			canExecute = true;
			// if multiple NCs are selected, they must all be in the same subscription state,
			// otherwise we don't allow the subscription change
			Boolean sharedIsSubscribed = null;
			for (ChannelData nc : ncList) {
				if (sharedIsSubscribed == null) {
					// the first NC we are checking
					sharedIsSubscribed = new Boolean(eventModel.isSubscribed(nc.getName()));
				}
				else {
					if (eventModel.isSubscribed(nc.getName()) != sharedIsSubscribed.booleanValue()) {
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
		
		return canExecute;
	}

	/**
	 * Allows one or many NCs to be selected, which is why we use 'Object' instead of 'ChannelData' as the selection.
	 */
	@Execute
	public void execute(@Named(IServiceConstants.ACTIVE_SELECTION) Object obj, MHandledItem handledItem) {
		
		List<ChannelData> ncList = getSelectedNCs(obj);
		for (ChannelData nc : ncList) {
			
			String ncName = nc.getName();
			
			// We simply toggle the subscription, ignoring the CHECK menu item state.
			// In fact we have to manually toggle it later, since the UI-induced toggling got lost 
			// when eclipse called 'canExecute' right before this method.
			boolean previousIsSubscribed = eventModel.isSubscribed(ncName);
			if (previousIsSubscribed) {
				eventModel.closeSelectedConsumer(ncName, true);
				logger.info("Unsubscribed from NC " + ncName);
			}
			else {
				eventModel.addChannelSubscription(ncName);
				logger.info("Subscribed to NC " + ncName);
			}
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
