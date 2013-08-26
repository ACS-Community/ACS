/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
/*
 * Created on Oct 27, 2005 by mschilli
 */
package alma.acs.vmtools;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Properties;
import java.util.StringTokenizer;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.event.TableModelEvent;
import javax.swing.event.TableModelListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableModel;



/**
 * Allows to configure System Properties at runtime.
 *
 * @author mschilli
 */
public class SysPropEditor extends JPanel {

	

	//
	// =============  Stand-Alone Launch==================
	//
	
	
	public static void main(String[] args) {
		SysPropEditor inst = new SysPropEditor();
		SysPropEditor.openFrame(inst);
	}
	
	public static JFrame openFrame (final SysPropEditor inst) {

		// frame
		final JFrame f = new JFrame(SysPropEditor.class.getName());
		f.setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
		f.addWindowListener(new WindowAdapter(){
			@Override
			public void windowClosing(WindowEvent evt){
				int answer = JOptionPane.showConfirmDialog(f, "Really close?", "Close Window", JOptionPane.YES_NO_OPTION);
				if (answer == JOptionPane.YES_OPTION) {
					f.setVisible(false);
					f.dispose();
				}
			}
		});
		f.getContentPane().add(inst);

		f.pack();
		f.setVisible(true);
		
		return f;
	}
	

	//
	// =============  Instance Implementation ==================
	//
	
	protected MapTableModel model;
	
	
	public SysPropEditor() {
		super(new BorderLayout());
		
		model = new MapTableModel(new String[]{"name", "value"});
		populateModel(new String[]{});
		
		model.addTableModelListener(new MyTableModelListener());
		
		JTable table = new JTable(model);
		
		add(new JScrollPane(table), BorderLayout.CENTER);
		add(new Controls(), BorderLayout.SOUTH);
		
	}


	// ================================================
	// API
	// ================================================

	public void populateModel (String[] nameFilter) {
		
		// get all sysprop names
		Properties m = System.getProperties();
		ArrayList<String> names = new ArrayList<String>(m.size());
		// filter names
		Enumeration en = m.propertyNames();
		while (en.hasMoreElements()) {
			String name = (String)en.nextElement();
			if (passesThroughFilter(nameFilter, name)) {
				names.add(name);
			}
		}
		// sort names
		Collections.sort(names);
		// iterate over sorted names and read values
		String[][] data = new String[names.size()][2];
		Iterator it = names.iterator();
		int i=0;
		while (it.hasNext()) {
			// store names and values in data-array
			String name = (String)it.next();
			data[i][0] = name;
			data[i][1] = m.getProperty(name);
			i++;
		}
		
		model.setData(data);
	}

	
	// ================================================
	// Internal
	// ================================================

	
	
	private boolean passesThroughFilter(String[] nameFilter, String name) {
		for (int i = 0; i < nameFilter.length; i++) {
			if (name.startsWith(nameFilter[i])) {
				return false;
			}
		}
		return true;
	}
	
	
	// ================================================
	// Inner Types
	// ================================================

	public class MapTableModel extends AbstractTableModel {

		protected String[] columnNames;
		protected Object[][] data;

		public MapTableModel(String[] columnNames) {
			this.columnNames = columnNames;
		}

		public void setData(Object[][] data) {
			this.data = data;
			super.fireTableDataChanged();
		}
		
		// --- Table Model API ---
		
		public int getColumnCount () {
			return columnNames.length;
		}
		
		@Override
		public String getColumnName(int column) {
			return columnNames[column];
		}

		public int getRowCount () {
			return data.length;
		}

		public Object getValueAt (int row, int column) {
			return data[row][column];
		}

		@Override
		public boolean isCellEditable (int row, int column) {
			return (column == 1); 
		}
		
		 @Override
		public void setValueAt (Object value, int row, int column) {
		    String cellValue = value.toString();
		    data[row][column] = cellValue;
		    fireTableCellUpdated (row, column);
		}		
	}
	
	/** Modify the System Properties when the user edited the model */
	class MyTableModelListener implements TableModelListener {
		public void tableChanged (TableModelEvent e) {
			
			if (e.getType() == TableModelEvent.UPDATE) {
				if (e.getFirstRow() == e.getLastRow()) {
					
					TableModel src = (TableModel)e.getSource();
					int row = e.getFirstRow();
					String name = (String)src.getValueAt(row, 0);
					String value = (String)src.getValueAt(row, 1);
					System.setProperty(name, value);
				}
			}
		}
	}
	
	class Controls extends JPanel {
		JTextField txtNameFilter;
		Controls() {
			super(new BorderLayout());
			
			this.add(new JLabel("Don't show: "), BorderLayout.WEST);
			this.add(txtNameFilter = new JTextField(), BorderLayout.CENTER);
			
			JButton r = new JButton("Refresh");
			r.addActionListener(new ActionListener() {
				public void actionPerformed (ActionEvent evt) {
					StringTokenizer t = new StringTokenizer(txtNameFilter.getText(), ";, ", false);
					String[] nameFilter = (String[])Collections.list(t).toArray(new String[]{});
					populateModel(nameFilter);
				}
			});
			this.add(r, BorderLayout.EAST);
			
			txtNameFilter.setText("sun, awt, file, os, java.awt, java.vm, java.vendor, java.spec");
		}
	}

	
	
}