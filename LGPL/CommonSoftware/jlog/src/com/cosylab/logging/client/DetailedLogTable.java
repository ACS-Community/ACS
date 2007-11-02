/*
 * Created on Dec 18, 2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package com.cosylab.logging.client;

import java.awt.Component;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Vector;

import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableModel;

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.ILogEntry.Field;

/**
 * The table used to represent data in the right panel of the
 * main window
 * 
 * @author acaproni
 *
 */
public class DetailedLogTable extends JTable
{
	int rowsNum; // The rows in the table
	String[][] nameValue; // The pairs <name,value>
	private String NOT_AVAILABLE="N/A";
	
	private TableModel dataModel = new AbstractTableModel() { 
		
		public int getColumnCount()
		{
			return 2;
		}
		public int getRowCount()
		{
			return rowsNum;
		}
		public Object getValueAt(int row, int col)
		{
			return nameValue[row][col];
		}
	};

	/**
	 * Build a table using the dats in the log entry
	 * 
	 * @param log The logEntry with the datas to display 
	 */
	public DetailedLogTable() {
		super();
		setModel(dataModel);
		setRowSelectionAllowed(false);
        setColumnSelectionAllowed(false);
        setCellSelectionEnabled(false);
        setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
        
		getColumnModel().getColumn(0).setHeaderValue("Field");
		getColumnModel().getColumn(1).setHeaderValue("Value");
		
		getColumnModel().getColumn(0).setMinWidth(110);
		getColumnModel().getColumn(1).setMinWidth(10000);
		setEmptyContent();
	}
	
	/**
	 * Fill the table with an empty content (no log selected)
	 *
	 */
	private void setEmptyContent() {
		rowsNum = Field.values().length;
		nameValue = new String[rowsNum][2];
		for (int i=0; i<Field.values().length; i++) {
			nameValue[i][0]= "<HTML><B>"+Field.values()[i].getName()+"</B>";
			nameValue[i][1]=NOT_AVAILABLE;
		}
		
		getTableHeader().resizeAndRepaint();
		resizeAndRepaint();
	}
	
	/**
	 * Fill the table with the fields of the given log
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
		rowsNum = Field.values().length;
		if (additionalData!=null) {
			rowsNum+=additionalData.size();
		}
		if (rowsNum > 0) {
			nameValue = new String[rowsNum][2];
			for (int i=0; i<Field.values().length; i++) {
				Field field = Field.values()[i];
				nameValue[i][0]= "<HTML><B>"+field.getName()+"</B>";
				Object obj = log.getField(field);
				if (obj!=null) {
					if (field==Field.ENTRYTYPE) {
						nameValue[i][1]=obj.toString()+" ("+LogTypeHelper.getLogTypeDescription((Integer)obj)+")";
					} else if (field==Field.TIMESTAMP) {
						SimpleDateFormat df = new SimpleDateFormat(ILogEntry.TIME_FORMAT);
						Date dt = (Date)obj;
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
			for (int i=Field.values().length; i < rowsNum; i++) {
				nameValue[i][0] = "<HTML><B>Additional</B> <I>"+additionalData.get(i-Field.values().length).getName()+"</I>";
				nameValue[i][1] = additionalData.get(i-Field.values().length).getValue();
			}

			// Change the name of the columns
			getColumnModel().getColumn(0).setHeaderValue("Field");
			getColumnModel().getColumn(1).setHeaderValue("Value");

			// Force the header to resize and repaint itself
			
			getTableHeader().resizeAndRepaint();
			resizeAndRepaint();
		}
	}
	
	/**
	 * Sets a tool tip on all the cells. It pops up when the value is not fully displayed while the mose 
	 * scrolls over it. 
	 * @see javax.swing.JTable#prepareRenderer(TableCellRenderer, int, int)
	 */
	public Component prepareRenderer(TableCellRenderer renderer, int rowIndex, int vColIndex)
	{
		Component c = super.prepareRenderer(renderer, rowIndex, vColIndex);

		int columnWidth = getColumnModel().getColumn(vColIndex).getWidth() / 6;
		setToolTip((JComponent)c,nameValue[rowIndex][vColIndex].toString(),columnWidth);
		return c;
	}
	
	/**
	 * Format the string before setting the tooltip for the given component
	 * The tooltip is shown only if the text is not visible (i.e. the num.
	 * of displayed chars for the column containing the text is less then
	 * the given text).
	 * 
	 * To show the string as multine it is transformed in HTML so
	 * \n are replaced by <BR>/ To show strings containing HTML and/or
	 * XML the <PRE> tag is used (for this reason existing < and > in
	 * the original string are replaced by &lt; and &lgt;)
	 * 
	 * @param c The component to set the tooltip 
	 * @param text The string to display in the tooltip
	 * @param colWidth The width of the column
	 * 
	 * @return
	 */
	private void setToolTip(JComponent  c, String text, int colWidth) {
		if (text==null)	{
			c.setToolTipText(null);
		} else if (text.length() <colWidth) {
			c.setToolTipText(null);
		} else {
			// I am going to print the string in HTML format: new lines become <BR>
			text=text.replaceAll("<","&lt;");
			text=text.replaceAll(">","&gt;");
			// Eventually, set the tooltip
			c.setToolTipText("<HTML><PRE>"+text+"</PRE></HTML>");
		}
	}

}
