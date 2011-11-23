/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) COSYLAB - Control System Laboratory, 2011
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
 * Created on Dec 18, 2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package com.cosylab.logging.client;

import java.awt.Component;
import java.awt.FontMetrics;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Vector;

import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellRenderer;

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.LogField;

import alma.acs.logging.table.LogTooltipHelper;
import alma.acs.logging.table.renderer.EntryTypeRenderer;
import alma.acs.util.IsoDateFormat;

/**
 * The table used to represent data in the right panel of the
 * main window
 * 
 * @author acaproni
 *
 */
public class DetailedLogTable extends JTable
{	
	
	/**
	 * The string shown when the dialog is not displaying a log
	 */
	private final String NOT_AVAILABLE="";
	
	public class MultilineRenderer extends JTextArea implements
			TableCellRenderer {
		
		public MultilineRenderer() {
			super (100,20);
			setWrapStyleWord(true);
			setLineWrap(true);
		}

		public Component getTableCellRendererComponent(
				//
				JTable table, Object obj, boolean isSelected, boolean hasFocus,
				int row, int column) {

			setText(obj.toString());
			int newSize = normalRowHeight*formatText(obj.toString());
			if (DetailedLogTable.this.getRowHeight(row)!=newSize) {
				DetailedLogTable.this.setRowHeight(row, newSize);
			}
			FontMetrics fm = this.getFontMetrics(getFont());	
			return this;
		}

		/**
		 * Calculate the number of lines to display the string in the
		 * renderer.
		 * <P>
		 * Setting the preferred size worked with jdk 1.3 but
		 * now does not and I did not find anything on the net so I have
		 * to find a solution by myself.
		 * <P>
		 * The calculation is not 100% perfect but seems to work pretty well.
		 * In future we should think of changing this strategy with something
		 * better..
		 * 
		 * @param str The string to display into the renderer
		 * @return The number of lines (probably) needed to display the
		 *         string into the text area. 
		 */
		private int formatText(String str) {
			if (str==null || str.length()==0) {
				return 1;
			}
			FontMetrics fm = this.getFontMetrics(getFont());
			int pixels = fm.stringWidth(str);
			if (pixels<getSize().width) {
				return 1;
			}
			return pixels/getPreferredSize().width+1;
		}

	}

	
	/**
	 * The model for this table.
	 * 
	 * @author acaproni
	 * 
	 */
	public class DetailedTableModel extends AbstractTableModel {
		/**
		 * An object of <code>LogTypeHelper</code> is needed in order to have 
		 * <code>EntryTypeRenderer</code> working properly.
		 */
		public LogTypeHelper logType=null;
		
		@Override
		public int getColumnCount()
		{
			return 2;
		}
		
		@Override
		public int getRowCount()
		{
			return rowsNum;
		}
		
		@Override
		public Object getValueAt(int row, int col)
		{
			if (col==1 && row==LogField.ENTRYTYPE.ordinal()) {
				return logType;
			}
			return nameValue[row][col];
		}
	}
	
	/**
	 * The table model
	 */
	private DetailedTableModel dataModel = new DetailedTableModel();
	
	/**
	 * The rows in the table
	 */
	private int rowsNum;
	
	/**
	 * The height of a row with only one line of text
	 */
	private int normalRowHeight;
	
	/**
	 * The pairs <name,value> i.e. all the values displayed in the table.
	 * <P>
	 * The first column contains the title of each cell in HTML format;
	 * the second column contains the value displayed in the right column of the
	 * table in plain text.
	 * The renderes will take such a string and format to be properly displayed,
	 * with the exception of the log type that is stored into the
	 * <code>DetailedTableModel</code> as a <code>LogTypeHelper</code>.
	 */
	private String[][] nameValue;
	
	/**
	 * The multiline renderer for the log message
	 */
	private MultilineRenderer logMessageRenderer = new MultilineRenderer();
	
	private EntryTypeRenderer entryTypeRenderer = new EntryTypeRenderer(true);

	/**
	 * Build a table using the data in the log entry
	 * 
	 * @param log The logEntry with the data to display 
	 */
	public DetailedLogTable() {
		super();
		setModel(dataModel);
		setRowSelectionAllowed(false);
        setColumnSelectionAllowed(false);
        setCellSelectionEnabled(false);
        setAutoResizeMode(JTable.AUTO_RESIZE_LAST_COLUMN);
        
		getColumnModel().getColumn(0).setHeaderValue("LogField");
		getColumnModel().getColumn(1).setHeaderValue("Value");
		
		getColumnModel().getColumn(0).setMinWidth(110);
		getColumnModel().getColumn(0).setMaxWidth(250);
		getColumnModel().getColumn(1).setMinWidth(250);
		setEmptyContent();
		normalRowHeight=getRowHeight();
	}
	
	/**
	 * Fill the table with an empty content (no log selected)
	 *
	 */
	private void setEmptyContent() {
		rowsNum = LogField.values().length;
		nameValue = new String[rowsNum][2];
		for (int i=0; i<LogField.values().length; i++) {
			nameValue[i][0]= "<HTML><B>"+LogField.values()[i].getName()+"</B>";
			nameValue[i][1]=NOT_AVAILABLE;
		}
		
		dataModel.fireTableDataChanged();
	}
	
	/**
	 * Fill the table with the fields of the given log.
	 * 
	 * @param log The log whose content is shown in the table
	 */
	public void setupContent(ILogEntry log) {
		if (log==null) {
			setEmptyContent();
			return;
		}
		
		// The number of rows in the table is given by the number of fields
		// of each LogEntry plus the number of the "data" elements for the selected log
		Vector<ILogEntry.AdditionalData> additionalData = log.getAdditionalData();
		rowsNum = LogField.values().length;
		if (additionalData!=null) {
			rowsNum+=additionalData.size();
		}
		if (rowsNum > 0) {
			nameValue = new String[rowsNum][2];
			for (int i=0; i<LogField.values().length; i++) {
				LogField field = LogField.values()[i];
				nameValue[i][0]= "<HTML><B>"+field.getName()+"</B>";
				Object obj = log.getField(field);
				if (obj!=null) {
					if (field==LogField.ENTRYTYPE) {
						nameValue[i][1]=obj.toString();
						dataModel.logType=(LogTypeHelper)obj;
					} else if (field==LogField.TIMESTAMP) {
						SimpleDateFormat df = new IsoDateFormat();
						Date dt = new Date((Long)obj);
						StringBuffer dateSB = new StringBuffer();
						java.text.FieldPosition pos = new java.text.FieldPosition(0);
						df.format(dt,dateSB,pos);
						nameValue[i][1]=dateSB.toString();
					} else {
						nameValue[i][1]= obj.toString();
					}
				} else {
					nameValue[i][1]="";
				}
			}
			for (int i=LogField.values().length; i < rowsNum; i++) {
				nameValue[i][0] = "<HTML><B>Additional</B> <I>"+additionalData.get(i-LogField.values().length).name+"</I>";
				String temp = new String ((String)additionalData.get(i-LogField.values().length).value);
				nameValue[i][1] = temp;
			}
			
			dataModel.fireTableDataChanged();
		}
	}
	
	/**
	 * Sets a tool tip on all the cells. It pops up when the value is not fully displayed while 
	 * the mouse scrolls over it.
	 *  
	 * @see javax.swing.JTable#prepareRenderer(TableCellRenderer, int, int)
	 */
	public Component prepareRenderer(TableCellRenderer renderer, int rowIndex, int vColIndex)
	{
		Component c = super.prepareRenderer(renderer, rowIndex, vColIndex);

		if (vColIndex==1) {
			LogTooltipHelper.setToolTip((JComponent)c,nameValue[rowIndex][vColIndex].toString(),96);
		}
		return c;
	}

	/* (non-Javadoc)
	 * @see javax.swing.JTable#getCellRenderer(int, int)
	 */
	@Override
	public TableCellRenderer getCellRenderer(int row, int column) {
		if (column==0) {
			return super.getCellRenderer(row, column);
		} else {
			if (row!=LogField.ENTRYTYPE.ordinal()) {
				return logMessageRenderer;
			} else {
				return entryTypeRenderer;
			}
		}
	}

}
