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

package alma.acs.eventbrowser.parts;

import javax.annotation.PostConstruct;

import org.eclipse.e4.core.contexts.Active;
import org.eclipse.e4.ui.model.application.ui.basic.MPart;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 * Some of this code was ported to e4 from alma.acs.eventbrowser.views.EventListView#buildTextFilter.
 * @author hsommer
 */
public class EventListFilterToolControl {

	/**
	 * Used to notify the part (and its event list) of filter changes.
	 * We make a direct call to the part instead of using the event broker, 
	 * to distinguish between EventListPart and ArchiveListPart and not always refilter both.
	 */
	private IEventListPart eventListPart;
	
	@PostConstruct
	void createControls(Composite parent, @Active MPart part) {

		if (part.getObject() instanceof IEventListPart) {
			eventListPart = (IEventListPart) part.getObject();
		}
		else {
			throw new IllegalArgumentException(EventListFilterToolControl.class.getSimpleName() + 
					" expects injection of an MPart that represents an 'IEventListPart'. Got instead a " + part.getObject().getClass().getName());
		}
		
		Label label = new Label(parent, SWT.None);
		label.setText("Event type filter: ");
		final Text text = new Text(parent, SWT.BORDER | SWT.SEARCH);
//		text.setLayoutData(new GridData(GridData.GRAB_HORIZONTAL | GridData.HORIZONTAL_ALIGN_FILL));
		
//		System.out.println("EventListFilterToolControl got part " + part.getObject());
		
		text.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				// TODO: Add 'inertia', to avoid resetting the table filter too often.
				String currentText = text.getText().trim();
				eventListPart.notifyEventTypeFilterChanged(currentText);
			}
		});
	}


}
