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
package alma.acs.logging.engine.io;

import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * This class implements add some functionalities
 * to the standard StringBuffer like searching
 * for the TAGs and closing TAGS.
 * The purpose of this class is to support the loading of logs
 * from a file.
 * 
 * @author acaproni
 *
 */
public class LogStringBuffer {
	// The array that implements the buffer
	private StringBuilder buffer = new StringBuilder();
	
	/**
	 * The constructor
	 * 
	 * @param size The size of the rotating buffer
	 */
	public LogStringBuffer() {
		clear();
	}
	
	/**
	 * Clear the buffer
	 *
	 */
	public void clear() {
		if (buffer.length()>0) {
			buffer.delete(0,buffer.length());
		}
	}
	
	/**
	 * Append a char to the end of the array
	 * All the other chars are shifted back of one position, 
	 * and the first char is lost
	 * 
	 * @param ch The char to append
	 */
	public void append(char ch) {
		buffer.append(ch);
	}
	
	/**
	 * Remove any spurious in the buffer before a starting tag
	 *  
	 * @param tag The opening tag
	 */
	public void trim(String tag) {
		int pos=buffer.indexOf("<"+tag); 
		if (pos==0) {
			return;
		} else {
			buffer.delete(0,pos-1);
		}
	}
	
	/**
	 * @return a String representation of the buffer
	 */
	public String toString() {
		return buffer.toString();
	}
	
	/**
	 * @return The index of the type of the log found
	 *         as opening  TAG
	 *         -1: if the buffer contains no opening TAG
	 */
	private LogTypeHelper getOpeningTagIndex() {
		for (LogTypeHelper log: LogTypeHelper.values()) {
			if (buffer.indexOf("<"+log.logEntryType)!=-1) {
				return log;
			}
		}
		return null;
	}
	
	/**
	 * @return True is the buffer contains an opening tag
	 */
	public boolean hasOpeningTag() {
		return getOpeningTagIndex()!=null;
	}
	
	/**
	 * Return the opening TAG in the buffer, if any
	 * 
	 * @return The opening TAG in the buffer (for example "Debug")
	 *         Return an empty string if there is no opening TAG in the buffer
	 */
	public String getOpeningTag() {
		LogTypeHelper logType = getOpeningTagIndex();
		if (logType==null) {
			return "";
		} else {
			return logType.logEntryType;
		}
	}
	
	/**
	 * Look for a closing TAG
	 * 
	 * @param tag The name of the closing tag (for example "Info")
	 * @return true if the buffer contains the closing TAG
	 */
	public boolean hasClosingTag(String tag) {
		String closingTag= "</"+tag+">";
		int posClosingTag = buffer.indexOf(closingTag);
		boolean ret = posClosingTag !=-1;
		if (tag.compareTo(LogTypeHelper.TRACE.logEntryType)==0) {
			// Trace could terminate with "/>" or </Trace>
			ret = ret || buffer.indexOf("/>")!=-1;
		}
		if (ret==false) {
			return ret;
		}
		// Check if the closing tag is inside a CDATA section
		int posCDATA = buffer.lastIndexOf("<![CDATA[");
		if (posCDATA==-1) {
			// No CDATA section
			return true;
		}
		int posCloseCDATA = buffer.lastIndexOf("]]>");
		// The last check ensure that the closing tag is outside of the CDATA
		return posClosingTag>posCloseCDATA;
	}
	
	/**
	 * Returns the index within this string of the first occurrence of the specified substring.
	 * 
	 * @param str Any string
	 * @return if the string argument occurs as a substring within this object, then the index 
	 *          of the first character of the first such substring is returned; 
	 *          if it does not occur as a substring, -1 is returned.
	 * 
	 * @see java.lang.StringBuffer
	 */
	public int indexOf(String str) {
		return buffer.indexOf(str);
	}
}
