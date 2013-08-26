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

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;

import alma.acs.eventbrowser.model.MCStatistics;
import alma.acs.eventbrowser.model.NotifyServiceData;

/**
 * Comparator used for node positions in ServiceSummaryPart and ChannelTreePart.
 * Code taken and extended from ChannelTreePart.NameComparator.
 * 
 * @author hsommer
 */
public class ServiceViewerComparator extends ViewerComparator {
	
	/**
	 * This method ensures that standard notify services are listed before user notify services.
	 * Within each of these categories, the services will be sorted alphabetically using {@link NotifyServiceData#toString()}.
	 */
	@Override
	public int category(Object element) {
		if (element instanceof NotifyServiceData) {
			return ( ((NotifyServiceData)element).isStandardNotifyService() ? 0 : 1 );
		}
		return super.category(element);
	}

	@Override
	public int compare(Viewer viewer, Object e1, Object e2) {
		if (e1 instanceof MCStatistics && e2 instanceof MCStatistics) {
			return 0; // leave the statistics in the order I added them in ChannelData!!
		}
		// compare based on toString() methods
		return super.compare(viewer, e1, e2);
	}

}
