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

import java.util.HashMap;
import java.util.Map;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.inject.Inject;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.e4.core.di.annotations.CanExecute;
import org.eclipse.e4.core.di.annotations.Execute;
import org.eclipse.e4.core.services.events.IEventBroker;
import org.eclipse.e4.core.services.statusreporter.StatusReporter;
import org.eclipse.e4.ui.di.UISynchronize;
import org.eclipse.e4.ui.model.application.ui.menu.MHandledItem;
import org.eclipse.e4.ui.model.application.ui.menu.MItem;

import alma.acs.eventbrowser.model.EventModel;
import alma.acs.eventbrowser.status.StatusLineWriter;

/**
 * Handler for the toggling of the various popup/part menu items that control periodic refreshing
 * of the notify service data.
 */
public class PeriodicRefreshHandler {

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

	/**
	 * Is <code>null</code> when periodic updating is not enabled.
	 */
	private NotifyServiceUpdateJob job;
	
	public final int refreshDelaySeconds = 10;
	
	/**
	 * @see #canExecute(MHandledItem)
	 */
	private final Map<String, MItem> menuItemsToSync = new HashMap<String, MItem>();
	
	/**
	 * @see #canExecute(MHandledItem)
	 */
	private boolean sharedIsSelected = false;
	
	/**
	 * The application model default of the first menu item that calls this handler
	 * is used as the default for all other menu items calling this handler.
	 * This way we don't have to hardcode a default value here.
	 */
	private boolean gotDefaultFromAppModel = false;
	
	private StatusLineWriter statusLineWriter;
	
	
	@PostConstruct
	public void init() {
//		System.out.println("PeriodicRefreshHandler#init called.");
		
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
	 * Always returns true, since only CHECK menu items are expected to trigger the call, 
	 * and they get selected or unselected but always are enabled.
	 * <p>
	 * Hack: We abuse this method to 'harvest' all relevant menu items in {@link #menuItemsToSync},
	 * so that we can later synchronize their selection state (all checked or unchecked).
	 * See http://www.eclipse.org/forums/index.php/t/442970/ and http://www.eclipse.org/forums/index.php/t/444326/ 
	 * and http://comments.gmane.org/gmane.comp.ide.eclipse.e4.devel/7870
	 * about alternative workarounds, that I found not working or uglier than this harvesting.
	 * There it also explains why EModelService#findElements is not working for menu items, 
	 * and other current limitations of E4 like not having a HandlerUtil class to sync stateful menu items.
	 * I believe that having moved the menu items away from the java code into the e4xmi file, there 
	 * should also be a declarative way to form synchronization groups of stateful menu items.
	 */ 
	@CanExecute
	public boolean canExecute(MHandledItem handledItem) {
		
		// first call total?
		if (!gotDefaultFromAppModel) {
			gotDefaultFromAppModel = true;
			sharedIsSelected = handledItem.isSelected();
		}
		
		// first call from this menu item?
		if (!menuItemsToSync.containsKey(handledItem.getElementId())) {
			menuItemsToSync.put(handledItem.getElementId(), handledItem);
//			System.out.println("canExecute called from " + handledItem.getElementId() + " which is of type " + handledItem.getClass());
			handledItem.setSelected(sharedIsSelected);
		}
		
		// always enable the menu item
		return true;
	}

	
	/**
	 * Starts a {@link NotifyServiceUpdateJob} with 10 seconds refresh period,
	 * which will notify the interested parts via the IEventBroker.
	 * <p>
	 * An alternative implementation could get parameter <code>@Active MPart part</code> injected
	 * and notify the part directly, without IEventBroker.
	 */
	@Execute
	public void execute(MHandledItem handledItem) {

		// sync menu item state
		sharedIsSelected = handledItem.isSelected();
		for (MItem menuItem : menuItemsToSync.values()) {
			menuItem.setSelected(sharedIsSelected);
		}
		
		// start or stop the periodic refresh job
		if (sharedIsSelected) {
			if (job == null) {
				// start periodic refreshes
				job = new NotifyServiceUpdateJob(eventModel, sync, statusLineWriter, eventBroker, refreshDelaySeconds*1000);
				job.schedule();
				System.out.println("Scheduled refresh job.");
			}
			else {
				System.out.println("Error: refresh job is already running!");
			}
		}
		else {
			if (job != null) {
				job.cancel();
				job = null;
				System.out.println("Cancelled refresh job.");
			}
			else {
				System.out.println("Error: refresh job is already cancelled!");
			}
		}
	}

	
	@PreDestroy
	public void dispose() {
//		System.out.println("PeriodicRefreshHandler#dispose called.");
		if (job != null) {
			job.cancel();
			job = null;
		}
	}

}
