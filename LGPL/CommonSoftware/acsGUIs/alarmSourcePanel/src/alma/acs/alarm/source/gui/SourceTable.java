/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2006
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
package alma.acs.alarm.source.gui;

import javax.swing.JTable;

import cern.laser.source.alarmsysteminterface.FaultState;

import alma.alarmsystem.clients.source.SourceListener;

/**
 * The table showing alarms from the source channel.
 * 
 * The table receives the alarms from the NC.
 * 
 * @author acaproni
 *
 */
public class SourceTable extends JTable implements SourceListener {
	
	// Table model
	private SourceTableModel model = new SourceTableModel();
	
	
	
	public SourceTable() {
		setModel(model);
		setAutoCreateRowSorter(true);
	}
	
	/**
	 * The listener for alarms sent by sources.
	 * 
	 * @see alma.alarmsystem.clients.source.SourceListener
	 */
	public void faultStateReceived(FaultState faultState) {
		model.addFS(faultState);
	}

	/**
	 * Remove all the fault states from the table
	 */
	public void clear() {
		model.clear();
	}
	
	/**
	 * Toggle between the compact/normal view of the table
	 * @param compact
	 */
	public void compactTable(boolean compact) {
		model.showCompact(compact);
	}

	@Override
	public void sourceXMLMsgReceived(String asiMessage) {
	}
}
