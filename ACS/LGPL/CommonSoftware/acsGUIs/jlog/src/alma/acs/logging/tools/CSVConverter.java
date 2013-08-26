/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2009
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 200, All rights reserved
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
package alma.acs.logging.tools;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Vector;

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.LogField;

import alma.acs.util.IsoDateFormat;

/**
 * Objects of this class produce a CSV string from a given log.
 * The CSV adhere to the definition in RFC4180.
 * <P>
 *  It is possible to select the columns of the log to export and their
 *  positions by setting a string.
 *  <P>
 *  A method generate the header in CSV format
 *  <P>
 *  All the fields appear inside double quotes.
 *  If a field contains double quotes they are escaped by double quotes.
 *  It is also possible not to enclose the fields by double quotes but in that 
 *  case the content of the field is changed because some character are not
 *  allowed.
 *  <P>
 *  It is possible to define a different separator instead of a comma, like
 *  for example the TAB.
 *  
 * @author acaproni
 *
 */
public class CSVConverter extends LogConverter {
	
	/**
	 * The separator, usually a ','
	 */
	private char separator=',';
	
	/**
	 * If <code>true</code> each field is enclosed in double quotes 
	 * otherwise the double quotes do not enclose the fields but
	 * some character in the fields will be replaced because 
	 * are not allowed (like the double quotes for example)
	 */
	private boolean useDoubleQuotes = true;
	
	/**
	 * Constructor 
	 * 
	 * @param cols A string describing the field of the log and their
	 *             position in the output
	 */
	public CSVConverter(String cols) {
		super(cols);
	}
	
	/**
	 * Constructor.
	 * <P>
	 * The converted string contains all the fields just once
	 * and the additional data.
	 * 
	 */
	public CSVConverter() {
		super();
	}
	
	/**
	 * Constructor
	 * 
	 * @param cols A string describing the field of the log and their
	 *             position in the output
	 * @param separator A character to use as fields separator
	 * @param doubleQuotes If <code>true</code> the fields are enclosed by double quotes
	 */
	public CSVConverter(String cols, char separator, boolean doubleQuotes) {
		super(cols);
		useDoubleQuotes=doubleQuotes;
		this.separator=separator;
	}
	
	/**
	 * Set the separator
	 * 
	 * @param sep The new separator char
	 */
	public void setSeparator(char sep) {
		separator=sep;
	}
	
	/**
	 * Put or remove the double quotes around the fields
	 * 
	 * @param enclose If <code>true</code> the fields are enclosed by double quotes
	 */
	public void encloseByDoubleQuotes(boolean enclose) {
		useDoubleQuotes=enclose;
	}
	
	/**
	 * Generate the header for the CSV file
	 * (it is optional and can appear in the first line of the file)
	 * 
	 * @return The CSV string representing the header in CSV format
	 *         with CR/LF at the end of the line
	 */
	public String getHeader() {
		StringBuilder str = new StringBuilder();
		for (int t=0; t<colIndex.length(); t++) {
			Character c= Character.toUpperCase(colIndex.charAt(t));
			int index;
			if ((c>='0' && c<='9') || (c>='A' && c<='F')) {
				index=Integer.parseInt(c.toString(),16);
			} else {
				index=16; // DATA
			}
			if (t>0) {
				str.append(separator);
			}
			appendField(LogField.values()[index].getName(),str);
		}
		str.append('\n');
		return str.toString();
	}
	
	/**
	 * Convert a log in a CSV string 
	 * 
	 * @param log The log to convert
	 * @return The CSV string representing the log
	 */
	public String convert(ILogEntry log) {
		if (log==null) {
			throw new IllegalArgumentException("Impossible to convert a null log");
		}
		StringBuilder str = new StringBuilder();
		SimpleDateFormat df = new IsoDateFormat();
		for (int t=0; t<colIndex.length(); t++) {
			if (t>0) {
				str.append(separator);
			}
			Character c= Character.toUpperCase(colIndex.charAt(t));
			if (
					(c>='0' && c<='9') || 
					(c>='A' && c<=Character.toUpperCase(LogField.values()[LogField.values().length-1].id))) {
				LogField field=LogField.fromID(c);
				Object obj = log.getField(field);
				if (obj==null) {
					appendField(null,str);
				} else  if (field==LogField.TIMESTAMP) {
					// Write the date in the right format
					Date dt=new Date(((Long)obj).longValue());
					StringBuffer dateSB = new StringBuffer();
					java.text.FieldPosition pos = new java.text.FieldPosition(0);
					df.format(dt,dateSB,pos);
					appendField(dateSB.toString(),str);
				} else if (field==LogField.ENTRYTYPE) {
					appendField(LogTypeHelper.fromLogTypeDescription(obj.toString()).logEntryType,str);
				} else {
					appendField(obj.toString(),str);
				}
			} else {
				// DATA
				if (log.hasDatas()) {
					appendField(formatData(log.getAdditionalData()),str);
				} else {
					appendField(null,str);
				}
			}
		}
		str.append('\n');
		return str.toString();
	}
	
	/**
	 * Append a field to the string (str)
	 * The field is into double quotes.
	 * If double quotes exist into the string, they are escaped.
	 * 
	 * @param fld The field to append
	 * @param str The string builder where the field is appended
	 */
	private void appendField(String fld, StringBuilder str) {
		if (useDoubleQuotes) {
			if (fld==null || fld.length()==0) {
				str.append('"');
				str.append('"');
				return;
			}
			str.append('"');
			str.append(fld.replaceAll("\"","\"\""));
			str.append('"');
		} else {
			if (fld==null || fld.length()==0) {
				return;
			}
			String temp = fld.replace('"','\'');
			temp=temp.replace(",","_");
			str.append(temp.replace('\n',' '));
		}
	}
	
	/**
	 * Format the additional data in a string to be
	 * appended in the CSV.
	 * The produced string is not converted in CSV but contains all
	 * the entries of the additional data
	 * 
	 * The format is the following:
	 * [name1 ==> val1] [name2 ==> val2] ....
	 * 
	 * @param datas The additional data of a log
	 * @return A string with the additional data
	 */
	private String formatData(Vector<ILogEntry.AdditionalData> datas) {
		StringBuilder temp = new StringBuilder();
		boolean first=true;
		for (ILogEntry.AdditionalData data: datas) {
			if (!first) {
				temp.append(' ');
			} else {
				first=false;
			}
			temp.append('[');
			temp.append(data.name);
			temp.append(" ==> ");
			temp.append(data.value);
			temp.append(']');
		}
		return temp.toString();
	}
	
}
