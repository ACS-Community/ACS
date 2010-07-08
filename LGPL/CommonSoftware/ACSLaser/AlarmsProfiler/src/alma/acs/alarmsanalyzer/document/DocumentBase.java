/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2010
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acs.alarmsanalyzer.document;

import java.util.Collection;

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.widgets.Display;

/**
 * A base class for the views used by the tool.
 * 
 * @author acaproni
 *
 */
public abstract class DocumentBase {
	/**
	 * 
	 * @return All the alarms in the container
	 */
	public abstract Collection<?> getNumbers();
	
	/**
	 * The table showing these alarms
	 */
	protected TableViewer tableViewer=null;
	
	/**
	 * <code>true</code> if the container has been shut down
	 */
	protected boolean shutdown=false;
	
	/**
	 * Set the table viewer showing these alarms
	 * 
	 * @param table the table viewer showing these alarms
	 */
	public void setTableViewer(TableViewer table) {
		if (table==null) {
			throw new IllegalArgumentException("The viewer can't be null");
		}
		this.tableViewer=table;
	}
	
	/**
	 * Refresh the content of the table
	 */
	public void refresh() {
		if (!shutdown && tableViewer!=null) {
			Display display=Display.getDefault();
			display.syncExec(
				  new Runnable() {
				    public void run(){
				    	if (!shutdown) {
				    		tableViewer.refresh();
				    	}
				    }
			  });
		}
	}
	
	/**
	 * Shut down the container and free the resources
	 */
	public void shutdownContainer() {
		shutdown=true;
	}
}
