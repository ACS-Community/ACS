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

import java.util.Collection;

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogField;

/**
 * The base class of the converters of the logs.
 * <P>
 * It is possible:
 * <ul>
 * 	<LI>to select the field to write in the converted log
 *  <LI>repeat the same field several time
 *  <LI>define the order of the fields in the converted string
 * </ul>
 * <P>
 * <B>Note</B> that the format of the XML is not customizable
 * i.e. the selected columns and their order will be ignored while
 * converting.
 * 
 * @author acaproni
 * @since ACS 8.1.0
 */
public abstract class LogConverter {
	
	/**
	 * Additional data is not a field of the log so its 
	 * value is specified here as a constant char
	 */
	public final char ADDITIONAL_DATA_ID='j';
	
	/**
	 * The string representing the columns and their order.
	 * <P>
	 * colIndex is composed of chars identifying the column
	 * and their position. 
	 * Each char is is a LogField.id or ADDITIONAL_DATA_ID.
	 * <P>
	 * For example if <code>colIndex=="0f1j"</code> then the converted string 
	 * contains entries with like that: 
	 * timestamp, message, log type, addData 1, addData2...addDataN
	 * because 
	 * 	LogField.TIMESTAMP.id=0, 
	 * 	LogFiled.LOGMESSAGE=f, 
	 * 	LogField.ENTRYTYPE.id=1 and 
	 * 	ADDITIONAL_DATA_ID=j.
	 * <P>
	 * <B>Note that this is ignored while converting to XML!</B>
	 * 
	 * @see {@link LogField}
	 * @see LogConverter.colIndexVector
	 */
	protected String colIndex;
	
	/**
	 * Default constructor: the converted string will contain
	 * all the fields.
	 */
	public LogConverter() {
		setCols(null);
	}
	
	/**
	 * Constructor
	 * 
	 * @param cols A string describing the columns and their position
	 * 				in the converted String
	 */
	public LogConverter(String cols) {
		setCols(cols);
	}
	
	/**
	 * Constructor
	 * <P>
	 * This constructor only allows to have additional data appened
	 * at the end of the other fields.
	 * 
	 * @param fields A collection describing the columns and their position
	 * 				in the converted String
	 * @param additionlData <code>true</code> if the additional data must be
	 * 			appended in the last position of the converted string
	 */
	public LogConverter(Collection<LogField> fields, boolean additionlData) {
		setCols(fields,additionlData);
	}
	
	/**
	 * Set the columns to write in the converted string.
	 * 
	 * 
	 * @param columns A string of columns; if <code>null</code>
	 * 				or empty then colIndexs contain all
	 * 				the fields plus the additional data
	 * @see colIndex
	 */
	public void setCols(String columns) {
		if (columns==null || columns.isEmpty()) {
			colIndex="";
			for (LogField f: LogField.values()) {
				colIndex=colIndex+f.id;
			}
			colIndex=colIndex+ADDITIONAL_DATA_ID;
			colIndex=colIndex.toUpperCase();
			return;
		}
		// Check if all the chars represent a valid log field
		char[] ids=columns.toCharArray();
		for (char id: ids) {
			if (id==ADDITIONAL_DATA_ID) {
				continue;
			}
			LogField.fromID(id);
		}
		colIndex=columns.toUpperCase();
	}
	
	/**
	 * Set the columns to write in the converted string
	 * 
	 * @param columns A vector of LogField representing the field to
	 * 					write in the converted string
	 * @param additionlData <code>true</code> if the additional data must be
	 * 			appended in the last position of the converted string
	 * 
	 * @see colIndexVector
	 */
	public void setCols(Collection<LogField> columns, boolean additionlData) {
		if ((columns==null || columns.isEmpty())&& !additionlData) {
			throw new IllegalArgumentException("The columns can't be null nor empty");
		}
		String cols="";
		for (LogField f: columns) {
			cols=cols+f.id;
		}
		if (additionlData) {
			cols=cols+ADDITIONAL_DATA_ID;
		}
		setCols(cols);
	}
	
	/**
	 * Convert the log to a string of the given format
	 * 
	 * @param log The log to convert to a String
	 * @return An ASCII string with the selected format
	 */
	public abstract String convert(ILogEntry log);
	
	/**
	 * Generate the header for the given converter.
	 * <P>
	 * If no header is needed, the heder return an empty string.
	 * @return
	 */
	public abstract String getHeader();
}
