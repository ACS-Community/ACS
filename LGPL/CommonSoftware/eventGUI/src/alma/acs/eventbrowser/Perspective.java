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
package alma.acs.eventbrowser;

import org.eclipse.ui.IPageLayout;
import org.eclipse.ui.IPerspectiveFactory;

import alma.acs.eventbrowser.views.ChannelTreeView;
import alma.acs.eventbrowser.views.EventDetailView;
import alma.acs.eventbrowser.views.EventListView;
import alma.acs.eventbrowser.views.ServiceSummaryView;


public class Perspective implements IPerspectiveFactory {
	
	public void createInitialLayout(IPageLayout layout) {
		layout.setEditorAreaVisible(false);
//		layout.addView(ChannelTreeView.ID, IPageLayout.LEFT, 1.0f, layout.getEditorArea());
//		layout.addView(ServiceSummaryView.ID, IPageLayout.RIGHT, 0.25f, ChannelTreeView.ID);
//		layout.addShowViewShortcut(ChannelTreeView.ID);
//		layout.addShowViewShortcut(ServiceSummaryView.ID);
//		layout.addView(EventListView.ID, IPageLayout.RIGHT, 0.5f, ServiceSummaryView.ID);
//		layout.addView(EventDetailView.ID, IPageLayout.BOTTOM, 0.5f, EventListView.ID);
	}
}
