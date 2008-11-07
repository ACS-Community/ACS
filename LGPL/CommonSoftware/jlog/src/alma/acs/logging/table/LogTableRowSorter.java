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

import javax.swing.SwingUtilities;
import javax.swing.table.TableRowSorter;

import com.cosylab.logging.engine.Filter;
import com.cosylab.logging.engine.Filterable;
import com.cosylab.logging.engine.FiltersVector;
import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * <code>LogTableRowSorter</code> extends <code>TableRowSorter</code> to customize
 * ordering, sorting and filtering for the table of logs.
 * <P>
 * The engine and the <code>FilterChooserDialog</code> uses a <code>FiltersVector</code>
 * to apply filtering to the logs but <code>JTable</code> needs a <code>RowSorter</code> instead.
 * <BR>This object converts the engine specific vector of filters into the objects
 * expected by the table.
 *
 * @author acaproni
 *
 */
public class LogTableRowSorter extends TableRowSorter<LogTableDataModel> implements Filterable, Runnable {
	
	/** 
	 * The filters
	 */
	private final FiltersVector filters = new FiltersVector();
	
	/**
	 * The log level
	 * @param model
	 */
	private LogTypeHelper logLevel=LogTypeHelper.TRACE;
	
	/**
	 * The column for the <code>toggleSortOrder</code> that must be executed 
	 * in a separate thread <code>toggleSortOrder</code>
	 * 
	 * @see 
	 */
	private int col;
	
	public LogTableRowSorter(LogTableDataModel model) {
		super(model);
		
		setMaxSortKeys(2);
		
		// Unsorted / unfiltered
		setSortKeys(null);
		applyChanges();
	}
	
	/* (non-Javadoc)
	 * @see com.cosylab.logging.engine.Filterable#setFilters(com.cosylab.logging.engine.FiltersVector, boolean)
	 */
	@Override
	public void setFilters(FiltersVector newFilters, boolean append) {
		if (append) {
			for (int t=0; t<newFilters.size(); t++) {
				Filter f = newFilters.get(t);
				filters.addFilter(f, newFilters.isActive(t));
			}
		} else {
			if (newFilters==null) {
				filters.clear();
			} else {
				filters.setFilters(newFilters);
			}
		}
		// apply the filters
		applyChanges();
	}
	
	/**
	 * 
	 * @return A description of the active filters
	 * @see FiltersVector.getFilterString()
	 */
	public String getFiltersString() {
		if (filters==null) {
			return "Not filtered";
		} else {
			return filters.getFilterString();
		}
	}
	
	/** 
	 * Return the filters defined by the user
	 * 
	 * @return The user defined filters
	 */
	public FiltersVector getFilters() {
		return filters;
	}
	
	/**
	 * Set the new log level i.e. the level of the logs shown in the table.
	 * 
	 * @param newLevel
	 */
	public void setLogLevel(LogTypeHelper newLevel) {
		logLevel=newLevel;
		applyChanges();
	}
	
	/**
	 * Set a new filter forcing a reordering of the table
	 */
	private void applyChanges() {
		Thread t = new Thread(new Runnable() {
			public void run() {
				setRowFilter(new LogTableRowFilter(filters,logLevel));
			}
		},"LogTableRowSorter.applyChanges");
		t.setDaemon(true);
		t. start();
	}
	
	/**
	 * Change the ordering when the user presses over a column header.
	 * We need to execute this method on a separate thread otherwise the 
	 * GUI freezes until the ordering completes.
	 * 
	 * 
	 */
	public void toggleSortOrder(int column) {
		Thread t = new Thread(this,"LogTableRowSorter.toggleSortOrder");
		col=column;
		t.setDaemon(true);
		t.start();
	}
	
	/**
	 * The thread to execute the <code>super.toggleSortOrder</code> without
	 * freezing the GUI.
	 */
	public void run() {
		try {
			super.toggleSortOrder(col);
		} catch (Throwable t) {
			System.out.println("Recovered error while setting sort order: "+t.getMessage());
		}
	}
}
