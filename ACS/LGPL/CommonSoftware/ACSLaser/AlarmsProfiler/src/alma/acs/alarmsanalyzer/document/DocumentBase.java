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

import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.widgets.Display;

import alma.acs.alarmsanalyzer.save.TableData;

/**
 * A base class for the views used by the tool.
 * 
 * @author acaproni
 *
 */
public abstract class DocumentBase implements Runnable {
	
	/**
	 * The refresh time in msec
	 */
	private static final int REFRSH_TIME_INTERVAL = 1500;
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
	 * The thread
	 */
	private final Thread thread;
	
	/**
	 * The title of the container
	 */
	public final String containerTitle;
	
	/**
	 * The name of the columns displayed in the view
	 */
	public final String[] colNames;
	
	/**
	 * Canstructor. It starts the thread
	 * 
	 * @param title The title of the container
	 * @param The title of the columns displayed
	 */
	public DocumentBase(String title, String[] colNames) {
		if (title==null || title.isEmpty()) {
			throw new IllegalArgumentException("Invalid document title: "+title);
		}
		if (colNames==null || colNames.length==0) {
			throw new IllegalArgumentException("Invalid names of columns");
		}
		containerTitle=title;
		this.colNames=colNames;
		thread = new Thread(this);
		thread.setDaemon(true);
		thread.start();
	}
	
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
	private void refresh() {
		if (!shutdown && tableViewer!=null) {
			Display display=Display.getDefault();
			display.syncExec(
				  new Runnable() {
				    public void run(){
				    	if (!shutdown && !tableViewer.getControl().isDisposed()) {
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
		thread.interrupt();
	}

	/**
	 * Automatically refresh the view
	 */
	@Override
	public void run() {
		while (!shutdown) {
			try {
				Thread.sleep(REFRSH_TIME_INTERVAL);
			} catch (InterruptedException ie) {
				continue;
			}
			refresh();
		}
	}
	
	/**
	 * Add the text of the cells of the table
	 * 
	 * @param tData The table data to set the string of the rows
	 */
	protected abstract void setTableContent(TableData tData);
	
	/**
	 * @return The data to be saved
	 */
	public TableData getDataToSave() {
		TableData tData = new TableData(containerTitle, colNames);
		setTableContent(tData);
		return tData;
	}
}
