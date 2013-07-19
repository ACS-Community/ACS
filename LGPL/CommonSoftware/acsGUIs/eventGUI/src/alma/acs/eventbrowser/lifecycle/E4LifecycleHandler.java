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

package alma.acs.eventbrowser.lifecycle;

import org.eclipse.e4.core.services.events.IEventBroker;
import org.eclipse.e4.ui.workbench.lifecycle.PostContextCreate;
import org.eclipse.e4.ui.workbench.lifecycle.PreSave;

import alma.acs.eventbrowser.model.EventModel;


/**
 * This class gets registered as an extension property in plugin.xml, 
 * see http://www.vogella.com/articles/Eclipse4LifeCycle/article.html.
 * It can handle application start and stop events.
 * <p>
 * Annotations:
 * <ul>
 *   <li><code>PostContextCreate</code> Is called after the Application’s IEclipseContext is created, can be used to add objects, services, etc. to the context. 
 *       This context is created for the MApplication class.
 *   <li> <code>ProcessAdditions</code> Is called directly before the model is passed to the renderer, can be used to add additional elements to the model.
 *   <li> <code>PostStartup</code> Is called right before the  is fired.
 *   <li> <code>ProcessRemovals</code> Same as <code>ProcessAdditions</code> but for removals.
 *   <li> <code>PreSave</code> Is called before the application model is saved. You can modify the model before it is persisted.
 * </ul> 
 * See https://bugs.eclipse.org/bugs/show_bug.cgi?id=376821 about listening for UI event "org/eclipse/e4/ui/LifeCycle/activate" (UIEvents.UILifeCycle.ACTIVATE),
 * as there is no <code>PostStartup</code> annotation for this lifecycle manager.
 */
public class E4LifecycleHandler {
	
	@PostContextCreate
	void postContextCreate(final IEventBroker eventBroker) {
		// @TODO: Do something better than just logging the call, or remove the handler if it turns out we have nothing to do here...
//		System.out.println("@PostContextCreate called...");
	}
	
	/**
	 * This cleanup was previously (Eclipse e3) done in alma.acs.eventbrowser.Application
	 */
	@PreSave
	void preSave() {
		try {
			EventModel.getInstance().tearDown();
//			System.out.println("EventModel#tearDown called from E4LifecycleHandler#preSave()");
		} catch (Throwable ex) {
			ex.printStackTrace();
		}
	}

}
