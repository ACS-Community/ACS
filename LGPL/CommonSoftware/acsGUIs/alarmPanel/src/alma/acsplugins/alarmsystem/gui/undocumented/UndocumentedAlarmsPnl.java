/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2011
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
package alma.acsplugins.alarmsystem.gui.undocumented;

import java.awt.BorderLayout;

import javax.swing.JPanel;
import javax.swing.JScrollPane;

import alma.acsplugins.alarmsystem.gui.CernAlSysTabbedPane;
import alma.acsplugins.alarmsystem.gui.undocumented.table.UndocAlarmTableModel;
import alma.acsplugins.alarmsystem.gui.undocumented.table.UndocumentedAlarmTable;

/**
 * The panel for undocumented alarms
 * 
 * @author acaproni
 * @since ACS 10.0.0
 */
public class UndocumentedAlarmsPnl extends JPanel {

	/**
	 * The scroll pane of the table
	 */
	private JScrollPane tableScrollPane = new JScrollPane(
			JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
			JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
	
	/**
	 * The model of the table of undocumented alarms
	 */
	private final UndocAlarmTableModel undocModel;
	
	/**
	 * The table of undocumented alarms
	 */
	private final UndocumentedAlarmTable undocAlarmTable;
	
	/**
	 * The status line at he bottom
	 */
	private final UndocStatusLinePnl statusPnl;
	
	/**
	 * Constructor
	 * 
	 * @param tabbedPane The tabbed pane to hide/show the undocumented alarm tab
	 */
	public UndocumentedAlarmsPnl(CernAlSysTabbedPane tabbedPane) {
		undocModel = new UndocAlarmTableModel(tabbedPane);
		statusPnl = new UndocStatusLinePnl(undocModel);
		undocAlarmTable = new UndocumentedAlarmTable(undocModel);
		tableScrollPane.setViewportView(undocAlarmTable);
		setLayout(new BorderLayout());
		add(tableScrollPane,BorderLayout.CENTER);
		add(statusPnl,BorderLayout.SOUTH);
	}

	/**
	 * Getter
	 * @return
	 */
	public UndocAlarmTableModel getUndocModel() {
		return undocModel;
	}
	
	/**
	 * Stop threads and frees resources
	 */
	public void stop() {
		statusPnl.stop();
	}
}
