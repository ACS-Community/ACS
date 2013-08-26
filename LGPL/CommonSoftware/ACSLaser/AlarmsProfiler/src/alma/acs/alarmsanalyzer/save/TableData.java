/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2010
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
package alma.acs.alarmsanalyzer.save;

import java.util.Vector;

/**
 * The data of a table of a view
 * 
 * The data to save on files are a collection of the data shown in each container,
 * basically the data in the table at the moment the save is executed.
 * <P>
 * Each data is composed of 
 * <UL>
 * 	<LI>a title (like the title of the view)
 *  <LI>the title of each column
 *  <LI>a list of the data of each row
 * </UL>
 * 
 * @author acaproni
 */
public class TableData {
	
	/**
	 * The title of the table
	 */
	public final String tableTile;
	
	/**
	 * The titles of each col. of the table
	 */
	public final String[] header;
	
	/**
	 * The rows of the table
	 */
	private final Vector<String[]> rows = new Vector<String[]>();
	
	/**
	 * Constructor
	 * 
	 * @param title The title of the table
	 * @param hdrTitles The header title of each column of the table
	 */
	public TableData(String title, String[] hdrTitles) {
		if (title==null || title.length()==0) {
			throw new IllegalArgumentException("Invalid title");
		}
		System.out.println(title);
		if (hdrTitles==null || hdrTitles.length==0) {
			throw new IllegalArgumentException("Invalid header");
		}
		this.tableTile=title;
		this.header=hdrTitles;
	}
	
	/**
	 * Add the vale of each column of a row.
	 * <P>
	 * The length of the array must be the same of the header title.
	 * 
	 * @param data The value of the columns of the row
	 */
	public synchronized void addRowData(String[] data) {
		if (data==null || data.length!=header.length) {
			throw new IllegalArgumentException("Invalid data row");
		}
		rows.add(data);
	}
	
	/**
	 * 
	 * @return The number of rows in the vector
	 */
	public synchronized int getRowsNumber() {
		return rows.size();
	}
	
	/**
	 * Return the strings of the passed row index
	 * 
	 * @param r The number of the row (zero based)
	 * @return The values to show in the row r
	 */
	public synchronized String[] getRow(int r) {
		if (r<0) {
			throw new IllegalArgumentException("Illegal row number");
		}
		if (r>=rows.size()) {
			throw new IndexOutOfBoundsException("Invalid row "+r);
		}
		return rows.get(r);
	}
}
