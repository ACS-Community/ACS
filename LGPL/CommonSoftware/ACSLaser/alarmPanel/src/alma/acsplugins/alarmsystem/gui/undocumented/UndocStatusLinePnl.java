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
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTextField;

import alma.acsplugins.alarmsystem.gui.undocumented.table.UndocAlarmTableModel;

/**
 * The status line for the panel with undocumented alarms
 * 
 * @author acaproni
 *
 */
public class UndocStatusLinePnl extends JPanel implements ActionListener, Runnable {
	
	/**
	 * The booolean to terminate the thread refreshing the status line
	 */
	private volatile boolean terminateThread=false;
	
	/**
	 * This thread
	 */
	private Thread thread=null;
	
	/**
	 * The button to remove all the entries in the table
	 */
	private final JButton removeAllBtn=new JButton("Remove all alarms");
	
	/**
	 * The button to remove all the entries in the table
	 */
	private final JButton removeClearedBtn=new JButton("Remove cleared alarms");
	
	/**
	 * The line with the state of the undocumented alarms table
	 */
	private final JTextField statusTF = new JTextField(25);
	
	/**
	 * The model to clear alarms
	 */
	private final UndocAlarmTableModel model;
	
	/**
	 * Constructor
	 */
	public UndocStatusLinePnl(UndocAlarmTableModel model) {
		if (model==null) {
			throw new IllegalArgumentException("The model can't be null");
		}
		this.model=model;
		setLayout(new BorderLayout());
		JPanel btnPnl = new JPanel(new FlowLayout(FlowLayout.LEFT));
		btnPnl.add(removeAllBtn);
		removeAllBtn.addActionListener(this);
		btnPnl.add(removeClearedBtn);
		removeClearedBtn.addActionListener(this);
		add(btnPnl,BorderLayout.WEST);
		add(statusTF,BorderLayout.EAST);
		statusTF.setEditable(false);
		start();
	}
	
	/**
	 * Refresh the content of the label
	 * 
	 * @param totAlarms Number of alarms in table
	 * @param activeAlarms Number of active alarms in the table
	 */
	public void refresh(int totAlarms, int activeAlarms) {
		StringBuilder str=new StringBuilder();
		str.append(totAlarms);
		str.append(" alarms in table (");
		str.append(activeAlarms);
		str.append(" active and ");
		str.append(totAlarms-activeAlarms);
		str.append(" inactive)");
		statusTF.setText(str.toString());
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==removeAllBtn) {
			model.clearAll();
		} else if (e.getSource()==removeClearedBtn) {
			model.clearInactiveAlarms();
		}
	}
	
	/**
	 * Start the thread
	 */
	private void start() {
		terminateThread=false;
		thread = new Thread(this);
		thread.setDaemon(true);
		thread.start();
	}
	
	/**
	 * Stop the thread and free the resources
	 */
	public void stop() {
		terminateThread=true;
		thread.interrupt();
	}

	@Override
	public void run() {
		while (!terminateThread) {
			try {
				Thread.sleep(2000);
			} catch (InterruptedException ie) {
				continue;
			}
			if (!isVisible()) {
				// Does not refresh if the panel is not visible
				continue;
			}
			int totAl=model.getRowCount();
			int activeAl=model.getNumOfActiveAlarms();
			refresh(totAl, activeAl);
		}
	}
}
