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

package alma.acs.eventbrowser.status;

import org.eclipse.e4.core.services.events.IEventBroker;

import alma.acs.eventbrowser.status.MyStatusBar.MessageWithTime;

/**
 * This class should be used by the various parts in order to post messages to the status line.
 * It communicates with {@link MyStatusBar} via the event service.
 * <p>
 * TODO: Or should we try to get the MyStatusBar instance injected directly (as a Context variable etc), 
 * which would remove the need for using the event broker and this client?
 * 
 * @author hsommer
 */
public class StatusLineWriter {
	
	private IEventBroker eventBroker; 
	
	
	public StatusLineWriter(IEventBroker eventBroker) {
		this.eventBroker = eventBroker;
	}

	
	/**
	 * Must only be called from the UI thread.
	 * @param s The message to appear in the status bar.
	 */
	public void setMessage(String s) {
		eventBroker.post(MyStatusBar.STATUS_BAR_TOPIC_ID, s); 
	}
	
	/**
	 * Flashes a message for the given time, 
	 * or until another message gets set.
	 * Then sets the status line back to the previous content.
	 * 
	 * Must only be called from the UI thread.
	 * @param s The message to appear in the status bar.
	 */
	public void flashMessage(String s, int timeSeconds) {
		MessageWithTime msgWithTime = new MessageWithTime(s, timeSeconds);
		eventBroker.post(MyStatusBar.STATUS_BAR_TOPIC_ID, msgWithTime); 
	}

}
