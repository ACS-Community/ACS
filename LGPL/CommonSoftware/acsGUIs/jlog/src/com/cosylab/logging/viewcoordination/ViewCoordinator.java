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
package com.cosylab.logging.viewcoordination;

import com.cosylab.logging.LoggingClient;
import com.cosylab.logging.engine.Filter;
import com.cosylab.logging.engine.FiltersVector;
import com.cosylab.logging.engine.log.ILogEntry;

import alma.acs.logging.table.LogEntryTable;


/**
 * Adapter that allows jlog to coordinate table filters and selection notification
 * with external software such as the OMC.
 * See http://ictjira.alma.cl/browse/ICT-113
 * 
 * @author hsommer
 */
public class ViewCoordinator
{
	private final LoggingClient client;
	private final LogEntryTable logEntryTable;

	public ViewCoordinator(LoggingClient client, LogEntryTable logEntryTable) {
		this.client = client;
		this.logEntryTable = logEntryTable;
	}
	
	/**
	 * The table will show only logs that pass this filter.
	 * Previously installed jlog filters will get removed.
	 * <p>
	 * If the given filter is null, all filters will be removed.
	 * 
	 * @param filter
	 */
	public void setFilter(Filter filter) {
		// Normally we'd leave existing filters intact, using
		// FiltersVector filters = getLogEntryTable().getFilters();
		// but here we simply overwrite any previously applied filters
		// including another AntennaNameFilter
		FiltersVector filters = new FiltersVector();
		filters.add(filter);
		filters.activateFilter(filter, true);

		boolean shouldAppend = false;
		// The following call would need to be sync'd if we had not disabled the user filter menu / button.
		logEntryTable.setFilters(filters, shouldAppend);
		client.setTableFilterLbl();
	}

	/**
	 * The selection callback interface, to be provided by external code.
	 * Deselection occurs when when more than one log record is selected in the table.
	 * In that case, a <code>null</code> is sent.
	 * <p>
	 * We transmit an ILogEntry object and not just the fields that are relevant to search antenna names in. 
	 * This is to allow extraction of other information in the future, e.g. timestamps. 
	 */
	public static interface SingleLogSelectionListener {
		
		public void notifyLogSelected(ILogEntry log);
	}
	
	/**
	 * Allows the external code to be notified when an alarm gets selected in the table.
	 * <p>
	 * If called multiple times, the provided listener will supersede previously registered listeners. 
	 */
	public void setLogSelectionListener(SingleLogSelectionListener listener) {
		logEntryTable.setLogSelectionListener(listener);
	}

	// this is currently handled in the OMC plugin code under EXEC, not here in jlog
//	public void disableFiltersWidgets() {
//		// We disable filters when running inside the OMC, because since http://ictjira.alma.cl/browse/ICT-113
//		// the OMC can set filters to ensure "coordinated views" and there is currently no need
//		// to implement complicated synchronization and filter merging between possibly concurrent
//		// filter changes originating from the OMC and from the user filter dialogs.
//		client.enableFiltersWidgets(false);
//	}
}
