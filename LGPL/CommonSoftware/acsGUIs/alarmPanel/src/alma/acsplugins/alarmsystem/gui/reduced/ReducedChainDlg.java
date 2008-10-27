/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2007
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
package alma.acsplugins.alarmsystem.gui.reduced;

import java.awt.BorderLayout;
import java.awt.Dialog;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.table.TableColumnModel;

import cern.laser.client.data.Alarm;

import alma.acsplugins.alarmsystem.gui.table.AlarmGUIType;
import alma.acsplugins.alarmsystem.gui.table.AlarmTable;
import alma.acsplugins.alarmsystem.gui.table.AlarmTableModel;
import alma.acsplugins.alarmsystem.gui.table.AlarmTableModel.AlarmTableColumn;
import alma.alarmsystem.clients.CategoryClient;

/**
 * The dialog showing all the alarms involved in a reduction.
 * <P>
 * The dialog has a table, one row for each alarm.
 * The alarms shown in this dialog are a snapshot of those involved in a reduction.
 * 
 * @author acaproni
 *
 */
public class ReducedChainDlg extends JDialog implements ActionListener {
	
	/**
	 * The button to close the dialog
	 */
	private final JButton closeBtn = new JButton("Close");
	
	/**
	 * The button to refresh the content of the table
	 */
	private final JButton refreshBtn = new JButton("Refresh");
	
	/**
	 * The table of alarms
	 */
	private final AlarmTable table;
	
	/**
	 * The model
	 */
	private final AlarmTableModel model;
	
	/**
	 * The {@link CategoryClient} to get the children of the root
	 * alarm from the alarm service
	 */
	private final CategoryClient categoryClient;
	
	/**
	 * The rot alarm, whose children are displayed in the table
	 */
	private Alarm alarm;
	
	/**
	 * Constructor
	 * 
	 * @param client The {@link CategoryClient} to get the children of the alarm
	 * 				to show in the table
	 * @param rootAlarm The root alarm whose children appear in the table
	 */
	public ReducedChainDlg(CategoryClient client, Alarm rootAlarm) {
		if (client==null) {
			throw new IllegalArgumentException("The category client can't be null");
		}
		if (rootAlarm==null) {
			throw new IllegalArgumentException("The alarm can't be null");
		}
		categoryClient=client;
		alarm=rootAlarm;
		model = new AlarmTableModel(rootPane,false);
		table = new AlarmTable(model);
		initialize();
		refreshTableContent();
	}
	
	/**
	 * Initialize the GUI
	 */
	private void initialize() {
		setIconImage(new ImageIcon(AlarmGUIType.class.getResource(AlarmGUIType.iconFolder+"arrow_in.png")).getImage());
		setModalityType(Dialog.ModalityType.MODELESS);
		setDefaultCloseOperation(DISPOSE_ON_CLOSE);
		
		rootPane.setLayout(new BorderLayout());
		
		JScrollPane tableScrollPane = new JScrollPane(
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		tableScrollPane.setViewportView(table);
		
		// Set the column of the table
		//
		// The table shows all the columns but the flag
		AlarmTableColumn[] visibleCols = new AlarmTableColumn[AlarmTableColumn.values().length-1];
		int pos=0;
		for (AlarmTableColumn col: AlarmTableColumn.values()) {
			if (col!=AlarmTableColumn.ICON) {
				visibleCols[pos++]=col;
			}
		}
		table.showColumns(visibleCols);
		
		rootPane.add(tableScrollPane,BorderLayout.CENTER);
		
		JPanel buttonPnl = new JPanel();
		buttonPnl.add(refreshBtn);
		refreshBtn.addActionListener(this);
		refreshBtn.setEnabled(false);
		buttonPnl.add(closeBtn);
		closeBtn.addActionListener(this);
		rootPane.add(buttonPnl,BorderLayout.SOUTH);
		
		pack();
		setVisible(true);
	}
	
	/**
	 * Close the dialog and frees its resources
	 */
	public void close() {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				setVisible(false);
				dispose();
			}
		});
	}

	/**
	 * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==closeBtn) {
			close();
		} else if (e.getSource()==refreshBtn) {
			refreshTableContent();
		}
	}
	
	/**
	 * Refresh the content of the table by getting the children of the
	 * root alarm from the {@link CategoryClient}.
	 */
	private void refreshTableContent() {
		Thread refreshThread = new Thread() {
			public void run() {
				refreshBtn.setEnabled(true);
				model.clear();
				setTitle("Reduction chain of ["+alarm.getAlarmId()+"]");
				getAlarmChain(alarm);
				model.fireTableDataChanged();
			}
		};
		refreshThread.setDaemon(true);
		refreshThread.setName("ReducedchainDlg.refreshTableContent "+alarm.getAlarmId());
		refreshThread.start();
	}
	
	/**
	 * Get the chain of reduction of the given alarm.
	 * <P>
	 * <i>Implementation note</i>: this method is recursive and therefore could lead
	 * to an out of memory if the chain is very deep.
	 * 
	 * @param al The alarm to get reduced nodes
	 */
	private void getAlarmChain(Alarm al) {
		model.onAlarm(al);
		if (al!=null) {
			Alarm[] alarms = null;
			try {
				if (al.isNodeParent()) {
					alarms = categoryClient.getChildren(al.getAlarmId(), true);
				} else {
					alarms = categoryClient.getChildren(al.getAlarmId(), true);
				}
			} catch (Throwable error) {
				System.err.println("Error getting the children of "+alarm.getAlarmId());
				error.printStackTrace(System.err);
				JOptionPane.showMessageDialog(table, 
						"Error getting children of "+alarm.getAlarmId(),
						"Error getting alarms",
						JOptionPane.ERROR_MESSAGE);
				return;
			}
			if (alarms !=null) {
				for (Alarm child: alarms) {
					getAlarmChain(child);
				}
			}
		}
	}
	
	/**
	 * Set a new alarm as root of the displayed chain of a reduction.
	 * <P>
	 * By setting a new root alarm, the content of the dialog is replaced
	 * by a new chain of reduction having the passed alarm as root.
	 *  
	 * @param rootAlarm The new alarm root of a reduction chain
	 */
	public void setRootAlarm(Alarm rootAlarm) {
		if (rootAlarm==null) {
			throw new IllegalArgumentException("The alarm can't be null");
		}
		alarm=rootAlarm;
		refreshTableContent();
	}
}
