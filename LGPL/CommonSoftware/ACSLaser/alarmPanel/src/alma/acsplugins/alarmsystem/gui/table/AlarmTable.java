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

/** 
 * @author  aaproni
 * @version $Id: AlarmTable.java,v 1.8 2008/10/24 09:06:14 acaproni Exp $
 * @since    
 */

package alma.acsplugins.alarmsystem.gui.table;

import java.awt.Component;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.io.FileOutputStream;
import java.text.SimpleDateFormat;
import java.util.Date;

import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JTable;
import javax.swing.JComponent;
import javax.swing.SwingUtilities;
import javax.swing.ToolTipManager;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import javax.swing.table.TableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableRowSorter;

import alma.acs.util.IsoDateFormat;
import alma.acsplugins.alarmsystem.gui.table.AlarmTableModel.AlarmTableColumn;

import cern.laser.client.data.Alarm;

/**
 * 
 * The table of alarms
 *
 */
public class AlarmTable extends JTable implements ActionListener {
	
	/**
	 * The mouse adapter receiving mouse events generated
	 * over the table of the alarms
	 * 
	 * @author acaproni
	 *
	 */
	private class AlarmTableMouseAdapter extends MouseAdapter {
		
		// The last selected alarm
		//
		// It is set when the user presses over a row (i,e. selects an alarm)
		public Alarm selectedAlarm;
		/**
		 * @see MouseListener
		 */
		public void mouseClicked(MouseEvent e) {
			alarmSelected(e);
			showPopup(e);
		}

		/**
		 * @see MouseListener
		 */
		public void mousePressed(MouseEvent e) {
			alarmSelected(e);
			showPopup(e);
		}

		/**
		 * @see MouseListener
		 */
		public void mouseReleased(MouseEvent e) {
			alarmSelected(e);
			showPopup(e);
		}
		
		/**
		 * The user selected a row, i.e. an alarm.
		 * alarmSected notifies the model that the icon showd be removed
		 * 
		 * @param e The event to get the selected row from
		 */
		private void alarmSelected(MouseEvent e) {
			int row=rowAtPoint(new Point(e.getX(),+e.getY()));
			AlarmTable.this.model.alarmSelected(getRowSorter().convertRowIndexToModel(row));	
		}
		
		/**
		 * Show the popup menu
		 * 
		 * @param e The mouse event that triggered the pop
		 */
		private void showPopup(MouseEvent e) {
			if (!e.isPopupTrigger()) {
				return;
			}
			int row=rowAtPoint(new Point(e.getX(),+e.getY()));
			selectedAlarm = AlarmTable.this.model.getRowAlarm(getRowSorter().convertRowIndexToModel(row));
			class ShowPopup extends Thread {
				MouseEvent e;
				public ShowPopup(MouseEvent e) {
					this.e=e;
				}
				public void run() {
					ackMI.setEnabled(!selectedAlarm.getStatus().isActive());
					popupM.show(AlarmTable.this,e.getX(),e.getY());
				}
			}
			SwingUtilities.invokeLater(new ShowPopup(e));	
		}
	}
	
	/**
	 * The mouse adapter receiving mouse events generated
	 * over the header of the table of the alarms
	 * 
	 * @author acaproni
	 *
	 */
	private class AlarmHeaderMouseAdapter extends MouseAdapter implements ActionListener {
		
		// The popup
		private JPopupMenu headerPopup = new JPopupMenu("Header");
		private JCheckBoxMenuItem[] menuItems;
		
		/**
		 * Constructor
		 */
		public AlarmHeaderMouseAdapter() {
			// Build the popup menu
			//
			// Each item has the same order of the AlarmTableColumn
			menuItems = new JCheckBoxMenuItem[AlarmTableColumn.values().length];
			int t=0;
			for (AlarmTableColumn col: AlarmTableColumn.values()) {
				menuItems[t]= new JCheckBoxMenuItem(col.title);
				menuItems[t].addActionListener(this);
				headerPopup.add(menuItems[t++]);
			}
		}

		/**
		 * @see MouseListener
		 */
		public void mouseClicked(MouseEvent e) {
			showPopup(e);
		}

		/**
		 * @see MouseListener
		 */
		public void mousePressed(MouseEvent e) {
			showPopup(e);
		}

		/**
		 * @see MouseListener
		 */
		public void mouseReleased(MouseEvent e) {
			showPopup(e);
		}
		
		/**
		 * Show the popup menu
		 * 
		 * @param e The mouse event that triggered the pop
		 */
		private void showPopup(MouseEvent e) {
			if (!e.isPopupTrigger()) {
				return;
			}
			class ShowHeaderPopup extends Thread {
				MouseEvent e;
				public ShowHeaderPopup(MouseEvent e) {
					this.e=e;
				}
				public void run() {
					ratioMenu();
					headerPopup.show(AlarmTable.this,e.getX(),e.getY());
				}
			}
			SwingUtilities.invokeLater(new ShowHeaderPopup(e));	
		}
		
		/**
		 * Set/unset all the checkbox depending on the
		 * visible/hidden columns
		 */
		private void ratioMenu() {
			TableColumnModel colModel = getColumnModel();
			int pos=0;
			for (AlarmTableColumn col: AlarmTableColumn.values()) {
				try {
					colModel.getColumnIndex(col);
					menuItems[pos].setSelected(true);
				} catch (IllegalArgumentException iae) {
					menuItems[pos].setSelected(false);
				}
				pos++;
			}
		}

		/**
		 * @see ActionListener
		 */
		public void actionPerformed(ActionEvent e) {
			// Look for the source of this event
			JCheckBoxMenuItem source=null;
			TableColumn column=null;
			for (int t=0; t<menuItems.length; t++) {
				if (e.getSource()==menuItems[t]) {
					source=menuItems[t];
					column=columns[t];
					break;
				}
			}
			if (source==null) {
				System.out.println("Unknown source of event: "+e.getSource());
				return;
			}
			class AddRemoveCol extends Thread {
				public TableColumn col;
				public boolean toAdd;
				public void run() {
					TableColumnModel colModel = getColumnModel();
					if (toAdd) {
						colModel.addColumn(col);
					} else {
						colModel.removeColumn(col);
					}
				}
			}
			AddRemoveCol thread = new AddRemoveCol();
			thread.col=column;
			thread.toAdd=source.isSelected();
			SwingUtilities.invokeLater(thread);
		}
		
	}
	
	// The model of the table
	private AlarmTableModel model;
	
	// The sorter for sorting the rows of the table
	private TableRowSorter<TableModel> sorter;
	
	// The cols of the table
	private TableColumn[] columns;
	
	// The alarm adapter that recives events from the mouse
	private AlarmTableMouseAdapter mouseAdapter = new AlarmTableMouseAdapter();
	
	// The clipboard
	private ClipboardHelper clipboard = new ClipboardHelper();
	
	// The popup menu shown when the user presses the right mouse button over a row
	private JPopupMenu popupM = new JPopupMenu("Alarm");
	private JMenuItem ackMI = new JMenuItem("Acknowledge");
	private JMenuItem saveMI = new JMenuItem("Save...");
	private JMenuItem clipMI = new JMenuItem("To clipboard");
	
	// The label returned as renderer when no flag is shown in the 
	// first column of the table
	private JLabel emptyLbl = new JLabel();
	
	/**
	 * Constructor 
	 * @param model The model for this table
	 */
	public AlarmTable(AlarmTableModel model) {
		super(model);
		if (model==null) {
			throw new IllegalArgumentException("Invalid null model in constructor");
		}
		this.model=model;
		initialize();
	}
	
	/**
	 * Init the GUI
	 */
	private void initialize() {
		this.setCellSelectionEnabled(false);
		setRowSelectionAllowed(false);
		this.setOpaque(false);
		sorter = new TableRowSorter<TableModel>(model);
		this.setRowSorter(sorter);
		sorter.setMaxSortKeys(2);
		sorter.setSortsOnUpdates(true);
		
		// Remove all the columns not visible at startup
		TableColumnModel colModel = getColumnModel();
		columns = new TableColumn[colModel.getColumnCount()];
		for (int t=0; t<columns.length; t++) {
			columns[t]=colModel.getColumn(t);
			columns[t].setIdentifier(AlarmTableColumn.values()[t]);
			if (columns[t].getIdentifier()==AlarmTableColumn.ICON || columns[t].getIdentifier()==AlarmTableColumn.REDUCTION) {
				columns[t].setWidth(20);
				columns[t].setResizable(false);
				columns[t].setPreferredWidth(20);
				columns[t].setMaxWidth(20);
				columns[t].setMinWidth(20);
			}
		}
		for (AlarmTableColumn col: AlarmTableColumn.values()) {
			if (!col.visibleAtStartup) {
				colModel.removeColumn(columns[col.ordinal()]);
			} 
		}
		addMouseListener(mouseAdapter);
		
		getTableHeader().addMouseListener(new AlarmHeaderMouseAdapter());
		
		buildPopupMenu();
		
		// Set the tooltip
		ToolTipManager ttm = ToolTipManager.sharedInstance();
		ttm.setDismissDelay(Integer.MAX_VALUE);
		ttm.setLightWeightPopupEnabled(true);
	}
	
	/**
	 * Build the popup menu
	 */
	private void buildPopupMenu() {
		popupM.add(ackMI);
		popupM.add(saveMI);
		popupM.add(clipMI);
		popupM.pack();
		
		ackMI.addActionListener(this);
		saveMI.addActionListener(this);
		clipMI.addActionListener(this);
	}
	
	/**
	 * @see JTable
	 */
	public Component prepareRenderer(TableCellRenderer renderer, int rowIndex, int vColIndex) {
		TableColumn col = getColumnModel().getColumn(vColIndex);
		if (col.getIdentifier().equals(AlarmTableColumn.ICON)) {
			if (model.isRowAlarmNew(sorter.convertRowIndexToModel(rowIndex))) {
				Alarm alarm = model.getRowAlarm(sorter.convertRowIndexToModel(rowIndex));
				return AlarmGUIType.fromAlarm(alarm).flagRenderer;
			} else {
				return emptyLbl;
			}
		} else if (col.getIdentifier().equals(AlarmTableColumn.REDUCTION))  {
			AlarmTableEntry entry = model.getRowEntry(sorter.convertRowIndexToModel(rowIndex));
			if (entry.isReduced()) {
				return AlarmGUIType.reductionRenderer;
			} else {
				return emptyLbl;
			}
		}
		Component c = super.prepareRenderer(renderer, rowIndex, vColIndex);
		Alarm alarm = model.getRowAlarm(sorter.convertRowIndexToModel(rowIndex));
		colorizeCell(c, alarm);
	
		if (c instanceof JComponent) {
			JComponent jc = (JComponent) c;
			if (((AlarmTableModel)model).getCellContent(sorter.convertRowIndexToModel(rowIndex), vColIndex)==null) {
				jc.setToolTipText(null);
			} else { 
				jc.setToolTipText("<HTML>"+((AlarmTableModel)model).getCellContent(sorter.convertRowIndexToModel(rowIndex), vColIndex));
			}
		}
		return c;
	}
	
	/**
	 * Set the background and the foreground of the component depending
	 * on the priority and the state of the passed alarm
	 * 
	 * @param c The component to color
	 * @param priority The alarm to set the color
	 */
	private void colorizeCell(Component c, Alarm alarm ) {
		AlarmGUIType alarmType = AlarmGUIType.fromAlarm(alarm);
		c.setForeground(alarmType.foreg);
		c.setBackground(alarmType.backg);
	}

	/**
	 * @see ActionListener
	 */
	public void actionPerformed(ActionEvent e) {
		if (e.getSource()==saveMI) {
			saveAlarm(mouseAdapter.selectedAlarm);
		} else if (e.getSource()==clipMI) {
			clipboard.setClipboardContents(mouseAdapter.selectedAlarm.toString());
		} else if (e.getSource()==ackMI) {
			model.acknowledge(mouseAdapter.selectedAlarm);
		}
	}
	
	/**
	 * Save the alarm in a file
	 * 
	 * @param The alarm to save in a plain text file
	 */
	public void saveAlarm(Alarm alarm) {
		// Get the user dir property
		JFileChooser fileChooser = new JFileChooser();
		if (fileChooser.showSaveDialog(this)!=JFileChooser.APPROVE_OPTION) {
			return;
		}
		// Build the text to write
		SimpleDateFormat dateFormat = new IsoDateFormat();
		StringBuilder str = new StringBuilder(alarm.toString());
		str.append("\n\n");
		str.append("Saved at ");
		str.append(dateFormat.format(new Date(System.currentTimeMillis())));
		str.append("\n\n");
		// Save the file
		File outF = fileChooser.getSelectedFile();
		FileOutputStream fOutS;
		try {
			fOutS = new FileOutputStream(outF,false);
			fOutS.write(str.toString().getBytes());
			fOutS.flush();
			fOutS.close();
		} catch (Exception e) {
			JOptionPane.showInternalMessageDialog(this, e.getMessage(), "Error saving", JOptionPane.ERROR_MESSAGE);
		}
	}
}