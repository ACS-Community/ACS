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

import org.eclipse.e4.core.contexts.Active;
import org.eclipse.e4.core.di.annotations.CanExecute;
import org.eclipse.e4.core.di.annotations.Execute;
import org.eclipse.e4.ui.model.application.ui.basic.MPart;

import alma.acs.eventbrowser.parts.IEventListPart;


/**
 * Clear-event-list handler for parts that implement {@link IEventListPart}.
 */
public class ClearEventsHandler {
	
	/**
	 */
	@CanExecute
	public boolean canExecute() {
		return true;
	}

	
	@Execute
	public void execute(@Active MPart part) {

		if (part.getObject() instanceof IEventListPart) {
			((IEventListPart) part.getObject()).clearList();
		}
		else {
			System.out.println("Error in " + ClearEventsHandler.class.getSimpleName() + 
					": expected an MPart that represents an 'IEventListPart'. Got instead a " + part.getObject().getClass().getName());
		}
	}

}
