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
package com.cosylab.cdb.browser;

import javax.swing.table.*;
import java.util.HashMap;
import javax.swing.*;

class CDBTableModel extends AbstractTableModel
{
    Object [][]data;
    String []colNames;
    HashMap resetValue = new HashMap();

    /**
     *  Constructs a table model.
     *  @param data the data of the table
     *  @param colNames names of the table columns
     */
    CDBTableModel(Object[][]data, String[]colNames){
	this.data = data;
	this.colNames = colNames;
    }

    /**
     *  Returns the number of rows in the model.
     */
     public int getRowCount(){
	return data.length;
    }   

    /**
     *  Returns the number of columns in the model.
     */
    public int getColumnCount(){
	return colNames.length;
    }
    
    /**
     *  Returns the name of the column at columnIndex.
     *  @param col the index of the column 
     */
    public String getColumnName(int col){
	return colNames[col];
    }
    /**
     *  Returns the most specific superclass for all the cell values in the column.
     *  @param col the index of the column 
     */
    public Class getColumnClass(int col){
	return getValueAt(0,col).getClass();
    }

    /**
     *   Returns true if the cell at rowIndex and columnIndex is editable.
     *  @param row the row whose value to be queried.
     *  @param col the column whose value to be queried .
     */
    public boolean isCellEditable(int row, int col){
	if(col < 1){
	    return false;
	}
	return true;
    }

    /**
     *  Returns the value for the cell at columnIndex and rowIndex.
     *  @param row the row whose value is to be queried
     *  @param col the column whose value is to be queried
     */
    public Object getValueAt(int row, int col){
	return (String)data[row][col];
    }

    /**
     *  Sets the value in the cell at columnIndex and rowIndex to aValue.
     *  @param value the new value
     *  @param row the row whose value is to be changed
     *  @param col the column whose value is to be changed
     */
    public void setValueAt(Object value, int row, int col){
	data[row][col] = (String)value;
	fireTableCellUpdated(row,col);
    }

    public void storeVal(){
	int row = CDBLogic.selectedTable.getSelectedRow();
	int col = CDBLogic.selectedTable.getSelectedColumn();

	if(resetValue.get(new Integer(row)) == null){
	    
	    //Browser.getInstance().display("storing value: " + getValueAt(row,col),true);
	    resetValue.put(new Integer(row), (String)data[row][col]);
	}
    }

    public void resetValues(){
	for(int i = 0; i < getRowCount(); i++){
 	    if(resetValue.get(new Integer(i)) != null){
 		setValueAt((String)resetValue.get(new Integer(i)),i,1);
	    }
 	}
	resetValue.clear();
    }
}
