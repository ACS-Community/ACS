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

import javax.swing.*;
import javax.swing.table.*;
import javax.swing.text.*;
import java.awt.*;

class CDBTable extends JTable 
{
    boolean [] rowChanged;

    CDBTable(Object [][]data, String [] colNames){
	super(data,colNames);
	
	this.tableHeader.setEnabled(false);
	this.tableHeader.setReorderingAllowed(false);
	
	rowChanged = new boolean[data.length];

	//  Set cell renderer for the ATTRIBUTE VALUES column.
	//setDefaultRenderer(getColumnClass(1), new CDBTableCellRenderer());

	//
	setUpStringEditor();
    }

    public void setUpStringEditor(){
	final StringField stringField = new StringField("",3);//, this);
	DefaultCellEditor stringEditor = new DefaultCellEditor(stringField){
		public Object getCellEditorValue(){
		    return (String)stringField.getValue();
		}
	    };
	setDefaultEditor(String.class, stringEditor);
    }

    public void emptyArray(){
	for(int i = 0; i < rowChanged.length; i ++){
	    rowChanged[i] = false;
	}
    }


    // Table cell rederer class
    class CDBTableCellRenderer extends JLabel implements TableCellRenderer
    {
	Color background = Color.white;
	Color foreground = Color.black;
	Color selectedForeground = Color.red;

	CDBTableCellRenderer(){
	    //setOpaque(true);
	}

	public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int col){
	    setFont(table.getFont());
	    setText((String)value);

	    //if(Browser.getInstance().reset.isEnabled() && isForegroundRed(row,col)){
	    //setForeground(selectedForeground);
	    //setBackground(background);
	    //}
	    //else{
	    //setForeground(foreground);
	    //setBackground(background);
	    //}
	    
	    return this;
	}
    }

    class StringField extends JTextField
    {
	int counter = 0;

	StringField(String value, int col){
	    super(col);
	    setValue(value);
	}

	public void setValue(String val){
	    setText(val);
	}

	public String getValue(){
	    return getText();
	}

	protected Document createDefaultModel(){
	    return new WholeStringDocument();
	}

	protected class WholeStringDocument extends PlainDocument
	{
	    public void insertString(int offs, String str, AttributeSet a) throws BadLocationException{
		int r = getSelectedRow();
		int c = getSelectedColumn();

		//Browser.getInstance().display("int offs: " + offs + ". String str: " + str, true );

		if(rowChanged[r]==false){
		    Browser.getInstance().display("MESSAGE: Row " + (r+1) + " has changed. Storing its original value \"" + 
						  (String)getValueAt(r,c) + "\".\t[Node: " + Browser.getInstance().getPath() + "]",true);
		    
		    rowChanged [r] = true;
		    CDBLogic.CDBTree.setEnabled(false);
		    CDBLogic.selectedTableModel.storeVal();
		    CDBLogic.tableChanged = true;

		    if(CDBLogic.selectedTabbedPane.isEnabledAt(CDBLogic.xmlIndex)){
			CDBLogic.XMLIndexEnabled = true;
			CDBLogic.selectedTabbedPane.setEnabledAt(CDBLogic.xmlIndex,false);
		    }
		    Browser.getInstance().enableButtons(true);
		}

		char [] source = str.toCharArray();
		char [] result = new char[source.length];
		int j = 0;

		for(int i = 0; i < result.length; i++){
		    result[j++] = source[i];
		}

		super.insertString(offs,new String(result,0,j),a);
	    }
	}
    }
}
