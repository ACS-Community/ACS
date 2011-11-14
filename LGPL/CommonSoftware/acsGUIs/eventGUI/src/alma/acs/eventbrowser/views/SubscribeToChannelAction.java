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
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.ISelectionListener;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory.IWorkbenchAction;

import alma.acs.eventbrowser.model.AbstractNotifyServiceElement;
import alma.acs.eventbrowser.model.ChannelData;
import alma.acs.eventbrowser.model.EventModel;
import alma.acs.exceptions.AcsJException;

public class SubscribeToChannelAction extends Action implements
		ISelectionListener, IWorkbenchAction {
	
	private final IWorkbenchWindow window;
	private String channelName; // from selection
	private final EventModel em;
	private boolean add; // If true, subscribe; if false, unsubscribe
	private static final String addText = "Subscribe to channel";
	private static final String addTooltip = "Subscribe to all events on this channel.";

	private static final String removeText = "Unsubscribe from channel";
	private static final String removeTooltip = "Stop subscribing to events on this channel.";
	
	public final static String ID = "alma.acs.eventbrowser.eventgui.subscribetochannel";

	public SubscribeToChannelAction(IWorkbenchWindow window) throws Exception {
		this.window = window;
		setId(ID);
		setText(addText);
		setToolTipText(addTooltip);
		setImageDescriptor(PlatformUI.getWorkbench().getSharedImages().
				getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
		this.window.getSelectionService().addSelectionListener(this);
		em = EventModel.getInstance();
	}
	
	public void run() {
		if (add) {
			setEnabled(false);
			em.addChannelSubscription(channelName);
			setText(removeText);
			setText(removeTooltip);
		} else {
			setEnabled(false);
			em.closeSelectedConsumer(channelName, true);
			setText(addText);
			setText(addTooltip);
		}
		setEnabled(true);
	}
	
	@Override
	public void dispose() {
		// TODO Auto-generated method stub

	}

	@Override
	public void selectionChanged(IWorkbenchPart part, ISelection incoming) {
		IStructuredSelection selection;
		
		if (incoming instanceof IStructuredSelection) {
			selection = (IStructuredSelection) incoming;
			boolean enabled = selection.size() == 1 && selection.getFirstElement() instanceof ChannelData;
			if (enabled) {
				ChannelData channelData = (ChannelData)(selection.getFirstElement());
				channelName = channelData.getName();
				if (em.isSubscribed(channelName)) {
					add = false;
					setText(removeText);
					setText(removeTooltip);
					//enabled = false;
				} else {
					add = true;
					setText(addText);
					setText(addTooltip);
				}
			}
			setEnabled(enabled);
		} else {
			setEnabled(false);
		}

	}

}
