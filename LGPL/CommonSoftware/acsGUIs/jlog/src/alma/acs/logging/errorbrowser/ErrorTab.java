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
package alma.acs.logging.errorbrowser;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTable;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import alma.acs.logging.table.LogEntryTableModelBase;
import alma.acs.logging.table.renderer.DateRenderer;
import alma.acs.logging.table.renderer.EntryTypeRenderer;
import alma.acs.logging.table.renderer.InfoRenderer;

import com.cosylab.logging.LoggingClient;
import com.cosylab.logging.client.DetailedLogTable;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogField;
import com.cosylab.logging.settings.FieldChooserDialog;

/**
 * The component in each error tab of the error browser dialog
 * 
 * @author acaproni
 *
 */
public class ErrorTab extends JSplitPane implements ActionListener {
	
	/**
	 * The worker thread to look for logs belonging to the error trace
	 */
	private Engine engine;
	
	/**
	 * The table with the details of the log
	 */
	private DetailedLogTable detailedLogTable = new DetailedLogTable();
	
	/**
	 * The dialog to show the cols to display
	 */
	private FieldChooserDialog fieldChooser=null;

	/**
	 * The table of logs
	 */
	private JTable table = new JTable();
	
	/**
	 * The table model
	 */
	private LogEntryTableModelBase model;
	
	/**
	 * The popup menu to show the field chooser dialog
	 */
	private JPopupMenu popmenu;
	
	/**
	 * The columns shown by the table for each field of the log
	 * i.e. the array does not contain the column showing if there
	 * are additional data
	 */
	private TableColumn[] columns = new TableColumn[LogField.values().length];

	/**
	 * Constructor
	 * 
	 * @param sourceModel The model used by the engine to look for logs
	 * @param stackID The <code>STACKID</code> of the logs in this error trace
	 */
	public ErrorTab(LogEntryTableModelBase sourceModel, String stackID, LoggingClient client) throws Exception {
		super(JSplitPane.HORIZONTAL_SPLIT);
		
		model = new LogEntryTableModelBase(client);
		table.setModel(model);
		initialize();
		
		// Instantiate the engine that push logs into the table
		engine = new Engine(sourceModel,stackID,model);
	}
	
	/**
	 * Init the GUI
	 */
	private void initialize() {
		// Init the table
		table.createDefaultColumnsFromModel();
		table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
		table.setAutoCreateRowSorter(true);
		
		TableColumnModel tcm = table.getColumnModel(); 
		TableColumn tc;
		
		// Setup the first col 
		tc = tcm.getColumn(0);
		tc.setCellRenderer(new InfoRenderer());
		tc.setWidth(18);
		tc.setMaxWidth(18);
		tc.setResizable(false);
		
		tc = tcm.getColumn(LogField.ENTRYTYPE.ordinal() + 1);
		tc.setCellRenderer(new EntryTypeRenderer(false));

		tc = tcm.getColumn(LogField.TIMESTAMP.ordinal() + 1);
		tc.setCellRenderer(new DateRenderer(true));
		for (int t=1; t<tcm.getColumnCount(); t++) {
			tc=tcm.getColumn(t);
			tc.setMinWidth(50);
			tc.setPreferredWidth(100);
			tc.setResizable(true);
			tc.setIdentifier(LogField.values()[t-1]);
			columns[t-1]=tc;
		}
		
		// Set the visible columns
		boolean[] visCols = new boolean[LogField.values().length];
		for (int t=0; t<visCols.length; t++) {
			visCols[t]=false;
		}
		visCols[LogField.TIMESTAMP.ordinal()]=true;
		visCols[LogField.ENTRYTYPE.ordinal()]=true;
		visCols[LogField.SOURCEOBJECT.ordinal()]=true;
		visCols[LogField.LOGMESSAGE.ordinal()]=true;
		visCols[LogField.STACKLEVEL.ordinal()]=true;
		setupTableCols(visCols);
		
		// Add the tabs
		setRightComponent(new JScrollPane(detailedLogTable));
		setLeftComponent(new JScrollPane(table));
		setDividerLocation(table.getPreferredSize().width);
		
		// Init the popup menu
		JMenuItem menuItem=new JMenuItem("Select columns...");
		menuItem.addActionListener(this);
		popmenu = new JPopupMenu();
		popmenu.add(menuItem);
		
		// Add the listener to the table header
		table.getTableHeader().addMouseListener(new MouseAdapter() {
			public void mousePressed(MouseEvent e) {
				if (e.isPopupTrigger()) {
					popmenu.show(ErrorTab.this.table, e.getX(), e.getY());
				}
			}
		});
		
		// Add the listener to the table
		table.addMouseListener(new MouseAdapter() {
			public void mousePressed(MouseEvent e) {
				if (!e.isPopupTrigger()) {
					// Get the index of the row and the column below
					// the mouse pointer
					int row = ErrorTab.this.table.rowAtPoint(e.getPoint());
					
					ILogEntry log=model.getVisibleLogEntry(table.convertRowIndexToModel(row));
					ErrorTab.this.detailedLogTable.setupContent(log);
				}
			}
		});
	}
	
	/**
	 * Release all the resource
	 */
	public void close() {
		setVisible(false);
		if (engine!=null) {
			engine.close();
			model.close(false);
		}
		if (fieldChooser!=null) {
			fieldChooser.dispose();
			fieldChooser=null;
		}
	}
	
	/**
	 * Show the dialog to set the visible columns
	 * <P>
	 * The first time this class is called, the dialog is created.
	 */
	private void showFieldChooser() {
		if (fieldChooser==null) {
			fieldChooser=new FieldChooserDialog(ErrorTab.this);
		}
		
		String[] colNames = new String[LogField.values().length];
		boolean colVisible[] = new boolean[colNames.length];
		for (int t=0; t<colNames.length; t++) {
			colNames[t]=LogField.values()[t].getName();
			try {
				TableColumn tc = table.getColumn(LogField.values()[t]);
				colVisible[t]=true;
			} catch (IllegalArgumentException e) {
				colVisible[t]=false;
			}
		}
		fieldChooser.setupFields(colNames, colVisible);
		fieldChooser.setVisible(true);
		
		boolean newFields[] = fieldChooser.getChecked();
		setupTableCols(newFields);	
	}
	
	/**
	 * Show or hide the the columns of the table.
	 * <P>
	 * The array of boolean has one entry for each column that is
	 * <code>true</code> if the column must be shown or <code>false</code> if
	 * such a column must be hidden.
	 * <BR> The methods checks if a column is already shown (or hidden) before adding
	 * (or removing) a new column to the table.
	 * <P>
	 * The first column, i.e. the column showing if a log has additional data,
	 * is not considered here and is not part of the parameter. 
	 * 
	 * @param cols The columns to show/hide
	 */
	private void setupTableCols(boolean[] cols) {
		if (cols==null || cols.length!=LogField.values().length) {
			throw new IllegalArgumentException("(nvalid parameter");
		}
		for (int t=0; t<cols.length; t++) {
			
			// Check if the column is shown by the table
			TableColumn tc =null;
			try {
				tc = table.getColumn(LogField.values()[t]);
			} catch (IllegalArgumentException e) {}
			
			
			if (cols[t]) {
				// show a column
				if (tc==null) {
					table.getColumnModel().addColumn(columns[t]);
				}
			} else {
				// remove a column
				if (tc!=null) {
					table.getColumnModel().removeColumn(tc);
				}
			}
		}
	}
	
	/**
	 * Override <code>setVisible</code> to hide the field chooser dialog
	 * when the user closes the error browser dialog.
	 */
	@Override
	public void setVisible(boolean b) {
		if (b==false && fieldChooser!=null) {
			fieldChooser.setVisible(b);
		}
		super.setVisible(b);
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		showFieldChooser();
	}
	
}
