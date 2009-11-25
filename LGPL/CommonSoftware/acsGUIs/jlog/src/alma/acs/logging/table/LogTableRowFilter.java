/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.logging.table;

import javax.swing.RowFilter;

import com.cosylab.logging.engine.Filter;
import com.cosylab.logging.engine.FiltersVector;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.LogField;

/**
 * The filters used to hide/show logs in the table.
 * <P>
 * The filters defined in the <code>FiltersVector</code> are entirely reused by this object.
 * To enhance performances only the active filters are stored into an array built 
 * by the constructor.
 * All the magic happens in the <code>include</code> method where the filters are compared against
 * the values of a row of the table.
 *  
 * @author acaproni
 *
 */
public class LogTableRowFilter extends RowFilter<LogTableDataModel, Integer> {
	
	/**
	 * The array with the filters to apply to the entries.
	 * 
	 * This array is null but when there are filters.
	 * In this case, it contains anly the active filters read from the <code>FiltersVector</code>
	 * passed as parameter in the constructor.
	 */
	private Filter[] filters = null;
	
	/**
	 * The log level.
	 * <P>
	 * All the entries whose level is less then <code>logLevel</code> are filtered out of the table.
	 */
	private LogTypeHelper logLevel=LogTypeHelper.TRACE;
	
	/**
	 * Constructor
	 * 
	 * @param filtersVector The vector of engine filters (can be <code>null</code> or empty)
	 * @param logLevel The log level of the logs to filter out
	 */
	public LogTableRowFilter(FiltersVector filtersVector, LogTypeHelper logLevel) {
		buildTableFilters(filtersVector);
		this.logLevel=logLevel;
	}
	
	/**
	 * Convert the vector of filters to a <code>LogTableRowFilter</code>.
	 * <P>
	 * The engine and the dialog define filters by means of a vector of filters that has 
	 * to be converted before being applied by this sorter.
	 * If the vector of filters is null or empty this object is set to <code>null</code> 
	 * to improve performances.
	 * All the filters defined in the vector are added in AND because this is the way
	 * they are defined in the <code>FiltersVector</code>.
	 *  
	 * @param userFilters The user defined vector of filters
	 * @return The <code>RowSorter</code> with all the filters in the passed parameter
	 */
	private void buildTableFilters(FiltersVector userFilters) {
		if (userFilters==null || userFilters.isEmpty()) {
			filters=null;
			return;
		}
		int[] activesIndexes = userFilters.getAppliedFiltersIndexes();
		if (activesIndexes!=null && activesIndexes.length>0) {
			filters = new Filter[activesIndexes.length];
			for (int t=0; t<activesIndexes.length; t++) {
				filters[t]=userFilters.get(activesIndexes[t]);
			}
		}
	}

	/**
	 * Compare the entry with the filters.
	 * <P>
	 * The entry represents a row of the table and it is possible to get the
	 * values of all the cells of the row using an index.
	 * The order of the columns is independent of the way the appear in the table
	 * and therefore is like this:
	 * <OL>
	 *   <LI> - Boolean (has data)
	 *   <LI> - LogField.TIMESTAMP
	 *   <LI> - LogField.ENTRYTYPE
	 * 	 <LI> - ..
	 * </OL>
	 * <P>
	 * The filtering is done by getting the value to check against the filter from the column (index)
	 * containing it in <code>entry</code>. 
	 * It is possible to know which field a filter wants to filter by reading the <code>Filter.field</code>
	 * property.
	 * <P>Once we have the filter and the value to filter, we can use <code>filter.applyTo(Object obj)</code>
	 * method to check if the row matches the filter.
	 * 
	 * @param entry The entry to check against filters
	 * @return <code>true</code> if the entry passed the filters and must be displayed in the table
	 * @see RowFilter.include
	 */
	@Override
	public boolean include(Entry<? extends LogTableDataModel, ? extends Integer> entry) {
		// Check the log level
		if (!checkLogLevel((LogTypeHelper)entry.getValue(LogField.ENTRYTYPE.ordinal()+1))) {
			return false;
		}
		// If there are no filters defined, the entry is accepted
		if (filters==null) {
			return true;
		}
		// Check if the entry matches with the filters
		//
		// For each filter the field to check is in the entry parameter in the index given
		// by the field.ordinal()+1 (the fist column, hasDatas must be ignored)
		boolean ret=true;
		for (int t=0; t<filters.length && ret; t++) {
			ret = ret && filters[t].applyTo(entry.getValue(filters[t].field.ordinal()+1));
		}
		return ret;
	}
	
	/**
	 * Check if the level of the log passed as parameter matches
	 * with the log level
	 * 
	 * @param level The level to check
	 * @return <code>true</code> if the parameter is greater then the log level
	 */
	private boolean checkLogLevel(LogTypeHelper level) {
		return level!=null && level.ordinal()>=this.logLevel.ordinal();
	}

}
