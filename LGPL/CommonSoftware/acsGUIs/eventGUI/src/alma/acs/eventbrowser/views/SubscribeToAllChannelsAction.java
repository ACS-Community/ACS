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

import org.eclipse.jface.action.Action;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
import alma.acs.eventbrowser.model.EventModel;

public class SubscribeToAllChannelsAction extends Action {
	
	private final EventModel em;
	private boolean add; // If true, subscribe; if false, unsubscribe
	private static final String addText = "Subscribe to all channels";
	private static final String addTooltip = "Subscribe to all events on all channels.";

	private static final String removeText = "Unsubscribe from all channels";
	private static final String removeTooltip = "Remove all event subscriptions from this application.";
	
	public final static String ID = "alma.acs.eventbrowser.eventgui.subscribetochannel";

	public SubscribeToAllChannelsAction() throws Exception {
		setId(ID);
		setText(addText);
		setToolTipText(addTooltip);
		setImageDescriptor(PlatformUI.getWorkbench().getSharedImages().
				getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
		em = EventModel.getInstance();
		add = true;
	}
	
	public void run() {
		if (add) {
			setEnabled(false);
			em.subscribeToAllChannels();
			setText(removeText);
			setText(removeTooltip);
			add = false;
		} else {
			setEnabled(false);
			em.closeAllConsumers();
			setText(addText);
			setText(addTooltip);
			add = true;
		}
		setEnabled(true);
	}
	
}

