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
package alma.demo.dyncomp;

/*
 * Created on Oct 29, 2003
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
 
import javax.swing.table.AbstractTableModel;
import javax.swing.JButton;

 /**
 * This is a concrete class definition to represent the data in the table
 * of the activated dynamic components
 *
 *  @author Alessandro Caproni, 2003, Nov 
 */
public class MyTableModel extends AbstractTableModel 
{
	/** The number of row and columns in the table */
	private final int ROWNUM=32;
	private final int COLNUM=3;
	
	/** The name of the columns 
	 * 
	 */
	final String colNames[] = { 
		"Dynamic component",
		"cUrl",
		""
		 };
	
	/** The data of each cell
	 * 
	 * The third column always contains the Release button
	 */
	Object tableData[][];
	
	/** The construtor
	 * 
	 *
	 */
	public MyTableModel() {
		tableData = new Object[ROWNUM][COLNUM];
		for (int r=0; r<ROWNUM; r++)
			for (int j=0; j<COLNUM; j++)
				tableData[r][j]=null;
	}
	
	/** Return the value of the cell
	*
	* @param row The row of the cell
	* @param colum The column of the cell
	*
	* @return The value of the cell
	*/
	public Object getValueAt(int row, int column) {
		return tableData[row][column];
	}
	
	/** Return the number of rows of the table
	*
	* @return The number of rows of the table
	*/
	public int getRowCount() { 
		return ROWNUM;
	}
	
	/** Return the number of columns of the table
	*
	* @return The number of columns of the table
	*/
	public int getColumnCount() {
		return COLNUM;
	}
	
	/** Append a new activated component with the given name
	 * 
	 * In this very very very first version the component are added 
	 * at the end without any care
	 * 
	 * @param name The name of the newly activated component
	 * @param cUrl The CORBA url of the activated component
	 */
	public void append(String name,String cUrl) {
		// Scans the table to find the first free cell in the table
		int freePos=0;
		while (freePos<ROWNUM && tableData[freePos][0]!=null) freePos++;
		if (freePos<ROWNUM) {
			// Set the name at column 0
			setValueAt((Object)name,freePos,0);
			// Set the cUrl at col 1
			setValueAt((Object)cUrl,freePos,1);
			// Set the button at column 2
			JButton btn =new JButton("Release "+name);
			setValueAt((Object)btn,freePos,2);
			btn.setVisible(true);
		}
	}
	
	/** Return the class of the elements stored in a column
	*
	* @param c The number of the column
	*
	* @return The Class of the objects stored in the cells of the column
	*/
	public Class getColumnClass(int c) {
		if (c==0 || c==1) try {
			return Class.forName("java.lang.String");
		} catch (ClassNotFoundException cnfe) { System.err.println(cnfe.toString()); return null; }
		else if (c==2) try {
			return Class.forName("javax.swing.JButton");
		}  catch (ClassNotFoundException cnfe) {  System.err.println(cnfe.toString()); return null; }
		return null;
	}


	/** Check if an element with the given name already exists in the table
	*
	* @param name the name of the item to search
	*
	* @return true if the item is found
	*/
	public boolean exist(String name) {
		for (int t=0; t<ROWNUM; t++) {
			if (tableData[t][0]!=null)
				if (name.compareTo((String)tableData[t][0])==0) return true;
		}
		return false;
	}

	/** Delete the component with the given name
	*
	* @paramn url The name of the component
	*/
	public void deleteEntry(String url) {
		int t;
		for (t=0; t<ROWNUM; t++) {
			if (tableData[t][1]!=null)
				if (url.compareTo((String)tableData[t][1])==0) break;
		}
		if (t<ROWNUM) {
			setValueAt(null,t,0);
			setValueAt(null,t,1);
			setValueAt(null,t,2);
		}
	}

	/** Sort the items of the table
	*/
	public void sort() {
		int a=0;
		int b;
		while (a<ROWNUM-2) {
			b=a+1;
			while (b<ROWNUM-1 && tableData[b][0]==null) b++;
			if (b<ROWNUM-1) {
				setValueAt((Object)tableData[b][0],a,0);
				setValueAt((Object)tableData[b][1],a,1);
				setValueAt((Object)tableData[b][2],a,2);
				setValueAt(null,b,0);
				setValueAt(null,b,1);
				setValueAt(null,b,2);
				a++;
			} else {
				// No more items found
				break;
			}
		}
		// The last one is always null
		setValueAt(null,ROWNUM-1,0);
		setValueAt(null,ROWNUM-1,1);
		setValueAt(null,ROWNUM-1,2);
	}
	
	/** Add a component to the table
	 * 
	 * @param name The name of the component
	 * @param row The row is ignored because each new element is appended at the end
	 * @param col The col parameter is ignored because the name always is inserted in the first column
	 */
	public void setValueAt(Object obj, int row, int col) {
		tableData[row][col]=obj;
		fireTableCellUpdated(row,col);
	}
	
	/** Return the name of the column
	 * 
	 * @param col The number of the column
	 */
	public String getColumnName(int col) {
		return colNames[col];
	} 
	 
	/** Check if a cell is editable.
	 * In this case all the cells are not editable
	 *
	 * @return true if the cell is editable
	 */
	public boolean isCellEditable(int row, int col) {
		if (tableData[row][col]==null) return false;
		if (col==0 || col==1) return false;
		else return true;
	}

    /**
     * Return the URL of a component given its name
     *
     * @param name The name of the component
     * @return The cURL of the component or null if a component with the specified name doe not exist
     */
    public String getURL(String name) {
	for (int t=0; t<ROWNUM; t++){
	    if (tableData[t][0]!=null) {
		if (((String)(tableData[t][0])).compareTo(name)==0) {
			return (String)tableData[t][1];
		}
	    }
	}
	return null;
    }

}

