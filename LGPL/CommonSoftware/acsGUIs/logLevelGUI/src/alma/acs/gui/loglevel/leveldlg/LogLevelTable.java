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
package alma.acs.gui.loglevel.leveldlg;

import java.awt.Component;
import java.awt.Point;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.DefaultCellEditor;
import javax.swing.JComboBox;
import javax.swing.JTable;
import javax.swing.RowSorter;
import javax.swing.SortOrder;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalArgumentEx;
import alma.acs.gui.loglevel.leveldlg.LogLevelModel.Column;
import alma.acs.logging.level.AcsLogLevelDefinition;

import com.cosylab.logging.client.EntryTypeIcon;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.settings.LogTypeRenderer;

/**
 * The table showing the log levels
 * 
 * @author acaproni
 *
 */
@SuppressWarnings("serial")
public class LogLevelTable extends JTable {
	
	// The renderer for the global and local log types
	private LogTypeCellRenderer renderer = new LogTypeCellRenderer();
	
	// The editor for log levels
	private JComboBox editor;
	public LogTypeRenderer editorRenderer= new LogTypeRenderer();
	
	/**
	 * Constructor
	 * 
	 * @param model The model of this table
	 */
	public LogLevelTable(LogLevelModel model) {
		super();
		if (model==null) {
			throw new IllegalArgumentException("Invalid null table model in constructor");
		}
		
		String[] descs = new String[LogTypeHelper.values().length];
		for (int t=0; t<LogTypeHelper.values().length; t++) {
			descs[t]=LogTypeHelper.values()[t].logEntryType;
		}
		editor=new JComboBox(descs);
		
		setModel(model);
		
		// Set the row sorter
		setAutoCreateRowSorter(true);
		List<RowSorter.SortKey> sortKeys = new ArrayList<RowSorter.SortKey>();
		sortKeys.add(new RowSorter.SortKey(0,SortOrder.ASCENDING));
		getRowSorter().setSortKeys(sortKeys);
		
		editor.setSelectedIndex(0);
		editor.setEditable(false);
		editor.setMaximumRowCount(LogTypeHelper.values().length);
		editor.setRenderer(editorRenderer);

		TableCellEditor ce = new DefaultCellEditor(editor) {
			@Override
			public Component getTableCellEditorComponent(JTable table,
					Object value, boolean isSelected, int row, int column) {
				JComboBox cb = (JComboBox)super.getTableCellEditorComponent(table, 
						value, isSelected, row, column);
				int v = (Integer)table.getValueAt(row, column);
				int index = 0;
				try {
					index = LogTypeHelper.fromAcsCoreLevel(AcsLogLevelDefinition.fromInteger(v)).ordinal();
				} catch (AcsJIllegalArgumentEx e) {
					// not expected
					System.err.println("Invalid ACS log level: "+v);
					e.printStackTrace();
				}
				cb.setSelectedIndex(index);
				return cb;
			}
		};
		
		columnModel.getColumn(Column.LOCAL.ordinal()).setCellEditor(ce);
		columnModel.getColumn(Column.GLOBAL.ordinal()).setCellEditor(ce);
		for (int t=0; t<Column.getColumnCont(); t++) {
			columnModel.getColumn(t).setMinWidth(50);
		}
		
		setRowMargin(2);
		setRowHeight(EntryTypeIcon.getIconsVSize()+5+getRowMargin());
		
		setRowSelectionAllowed(false);
		
	}
	
	/**
	 * Return the renderer for a given cell
	 */
	public TableCellRenderer getCellRenderer(int row, int column) {
		int modelColIdx = convertColumnIndexToModel(column);
		if (modelColIdx==LogLevelModel.Column.GLOBAL.ordinal() || modelColIdx==LogLevelModel.Column.LOCAL.ordinal()) {
			return renderer;
		}
		return super.getCellRenderer(row, column);
	}
	
	/**
	 * Set the tootip for the name of the logger
	 */
	public String getToolTipText(MouseEvent e) {
		String tip=null;
		Point p = e.getPoint();
        int rowIndex = rowAtPoint(p);
        int colIndex = columnAtPoint(p);
        int realColumnIndex = convertColumnIndexToModel(colIndex);
        int realRowIndex=convertRowIndexToModel(rowIndex);
        if (realColumnIndex==0) {
        	tip=getModel().getValueAt(realRowIndex, 0).toString();
        }
		
		return tip;
	}
	
}
