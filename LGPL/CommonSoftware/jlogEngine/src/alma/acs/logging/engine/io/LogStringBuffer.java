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
 * <code>LogStringBuffer</code> is a buffer of chars supporting the loading of logs
 * from a file.
 * <P>
 * The buffer contains the chars read from the file and passed through the <code>append()</code>.
 * For each new chars, <code>append()</code> checks if a closing or an ending XML tag of a log is in the buffer.
 * <P>
 * When a whole XML representing a log is in the buffer, it is returned to the caller and the 
 * buffer cleared to be ready to look for the next XML.
 * <BR>
 * The size of the buffer is initially set to a default value and it is doubled whenever
 * the algorithm needs more room in the array of char.
 * <P>
 * This functionality was initially implemented encapsulating a <code>StringBuilder</code> property
 * but it was too slow compared to basic chars manipulation.
 *  
 * @author acaproni
 *
 */
public class LogStringBuffer {
	/**
	 * The array of chars where the chars read from the input file are written.
	 * <P>
	 * The size passed in the construct is doubled when the length is not enough by calling
	 * <code>doubleBuffer()</code>.
	 * <BR>
	 * While reading logs from a file, each log is temporarily written in this array do it 
	 * begins with an XML opening tag and terminates with the closing tag.
	 * Any extra char read at the beginning of an opening tag is skipped.
	 */
	private char[] buffer = new char[1024];
	
	/**
	 * The position where a new char must be written in the array <code>buffer</code>.
	 * <P>
	 * It represents also the length of the string in the buffer.
	 */
	private int bufferPos=0;
	
	/**
	 * The position of the starting XML tag
	 * <P>
	 * The value of this property initially is -1 and is set to be equal to the position of the
	 * '<' char of the starting tag until a valid tag is found
	 *
	 */
	private int startTagPos;
	
	/**
	 * The XML tags of the log types (including the leading '<') for example
	 * <Trace and <Info
	 * <P>
	 * The first dimension is the position of the tag in the array of <code>LogTypeHelper</code>,
	 * the second dimension is the length of the tag
	 */
	private char[][] xmlOpeningTags;
	
	/**
	 * This property, initialized in the constructor, contains the size of the shortest start tag
	 * and is used to reduce the number of checks while looking for the starting tag
	 */
	private final int minStartXMLTagSize;
	
	/**
	 * The closing XML tags of the log types for example </Trace> and </Info>
	 * <P>
	 * The first dimension is the position of the tag in the array of <code>LogTypeHelper</code>,
	 * the second dimension is the length of the tag
	 */
	private char[][] xmlClosingTags;
	
	/**
	 * The starting of a CDATA section
	 */
	private char[] cdataStart = (new String("<![CDATA[")).toCharArray();
	
	/**
	 * The end of a CDATA section
	 */
	private char[] cdataEnd= (new String("]]>")).toCharArray();
	
	/**
	 * The position of the starting of the cdata section
	 */
	private int cdataStartPos;
	
	/**
	 * The position of the ending of the cdata section 
	 */
	private int cdataEndPos;
	
	/**
	 * The starting XML tag (it is <code>null</code> when the start tag has not yet 
	 * been found in the buffer
	 */
	private LogTypeHelper startTag;
	
	/**
	 * The constructor
	 * 
	 * @param size The size of the rotating buffer
	 */
	public LogStringBuffer() {
		clear();
		// Build the xml tags
		int minSz = Integer.MAX_VALUE;
		xmlOpeningTags = new char[LogTypeHelper.values().length][];
		for (int t=0; t<LogTypeHelper.values().length; t++) {
			xmlOpeningTags[t]=("<"+LogTypeHelper.values()[t].logEntryType).toCharArray();
			if (xmlOpeningTags[t].length<minSz) {
				minSz=xmlOpeningTags[t].length;
			}
		}
		minStartXMLTagSize=minSz;
		// The closing tag has one item more for the trace: "/>"
		xmlClosingTags = new char[LogTypeHelper.values().length+1][];
		for (int t=0; t<LogTypeHelper.values().length; t++) {
			xmlClosingTags[t]=("</"+LogTypeHelper.values()[t].logEntryType+">").toCharArray();
		}
		xmlClosingTags[xmlClosingTags.length-1]="/>".toCharArray();
	}
	
	/**
	 * Clear the buffer
	 *
	 */
	private void clear() {
		bufferPos=0;
		startTag=null;
		startTagPos=cdataStartPos=cdataEndPos=-1;
	}
	
	/**
	 * Append a char to the end of the buffer.
	 * <P>
	 * This method checks if the buffer contains the XML representing a log entry.
	 * The <code>str</code> parameter is used to return the string with a log and
	 * <I>must</I> be empty.
	 * The method does not perform any check about the content of the passed 
	 * <code>StringBuilder</code> so it is the caller that must ensure the correctness.
	 * <P>
	 * <B>Note</B>: the content of the <code>StringBuilder</code> is changed only
	 * and only if there is a whole log in the buffer.
	 * 
	 * @param ch The char to append
	 * @param str A string representing the new log; 
	 *            the content of this parameter is changed if and only if there is 
	 *            a log in the buffer
	 */
	public void append(char ch, StringBuilder str) {
		if (bufferPos==buffer.length) {
			buffer=doubleBuffer(buffer);
		}
		buffer[bufferPos++]=ch;
		if (ch!='<' && ch!='>' && ch!='[') {
			// The buffer is processed only if a special char is found: <, >, or [
			return;
		}
		
		if (ch=='<' && startTag==null) {
			startTagPos=bufferPos-1;
			return;
		}
		if (startTagPos>-1 && startTag==null && bufferPos-startTagPos>=minStartXMLTagSize) {
			startTag=lookForStartTag(buffer, startTagPos);
			if (ch!='>') {
				return;
			}
		}
		if (ch=='[' && cdataStartPos==-1) {
			if (compareFromLastChar(buffer, bufferPos-1, cdataStart)) {
				cdataStartPos=bufferPos-1;
				return;
			}
		}
		if (ch=='>' && cdataStartPos!=-1 && cdataEndPos==-1) {
			if (compareFromLastChar(buffer, bufferPos-1, cdataEnd)) {
				cdataEndPos=bufferPos-1;
				return;
			}
		}
		if (ch=='>' && startTag!=null) {
			// Check if we are inside a CDATA
			if (cdataStartPos!=-1 && cdataEndPos<cdataStartPos) {
				return;
			}
			// Check if there is a closing XML tag 
			if (
					compareFromLastChar(buffer, bufferPos-1, xmlClosingTags[startTag.ordinal()]) || 
					(startTag==LogTypeHelper.TRACE && compareFromLastChar(buffer, bufferPos-1, xmlClosingTags[xmlClosingTags.length-1]))) {
				// A log is in the buffer
				str.append(buffer,startTagPos, bufferPos-startTagPos);
				clear();
			}
		}
	}
	
	/**
	 * @return a String representation of the buffer
	 */
	public String toString() {
		return new String(buffer).substring(0, bufferPos);
	}
	
	/**
	 * Doubles the array.
	 * <P>
	 * It creates a new array having the size double as the size of the passed array.
	 * <BR>
	 * The content of the first array is copied in the newly created one.
	 * 
	 * @param originalBuffer The buffer to copy in the new array of a double size 
	 */
	private char[] doubleBuffer(char[] originalBuffer) {
		if (originalBuffer==null) {
			throw new IllegalArgumentException("Invalid null array of char");
		}
		char ret[] = new char[originalBuffer.length*2];
		System.arraycopy(originalBuffer, 0, ret, 0, originalBuffer.length);
		return ret;
	}
	
	/**
	 * Compare the <code>src</code> array starting at position <code>srcPos</code> with 
	 * the <code>dest</code> array.
	 * <P>
	 * The method checks if all the chars starting at <code>srcPos</code> position of the first array
	 * are equal to the chars of the second array.
	 * <BR>
	 * The comparison lasts for all the chars of the <code>dest</code> array i.e. this method compares
	 * the chars in <code>src[newPos, newPos+dest.length]</code> with those in <code>dest[0,dest.length]</code>.
	 * 
	 * @param src The first array to compare
	 * @param srcPos The position inside <code>src</code> to start the comparison at
	 * @param dest The array to compare
	 * @return <code>true</code> If the chars of the <code>src</code> starting at <code>srcPos</code> are
	 *                           equal to the chars of <code>dest</code>.
	 */
	private boolean compare(char[] src, int srcPos, char[]dest) {
		if (bufferPos-srcPos<dest.length) {
			return false;
		}
		for (int c=0; c<dest.length; c++) {
			if (srcPos+c>=src.length || src[srcPos+c]!=dest[c]) {
				return false;
			}
		}
		return true;
	}
	
	/**
	 * Compare the content of 2 array of chars starting from the position
	 * of the last char of the first array.
	 * <P>
	 * The method checks is <code>src</code> and <code>dest</code> are equal.
	 * The check is done in reverse order, starting from the position of the last 
	 * char of the first array, <code>src</code>.
	 * <BR>
	 * The comparison lasts for all the chars of the <code>dest</code> array i.e. this method compares
	 * the chars in <code>src[lastCharPos-dest.length,lastCharPos]</code> with those in <code>dest[0,dest.length]</code>.
	 * 
	 * @param src The first array to compare
	 * @param lastCharPos The position of the last char of <code>src</code> to compare
	 * @param dest The array to compare
	 * @return  <code>true</code> If the chars of the <code>src</code> starting at <code>lastCharPos-dest.length</code> 
	 *                            are equal to the chars of <code>dest</code>.
	 */
	private boolean compareFromLastChar(char src[], int lastCharPos, char[] dest) {
		if (lastCharPos-dest.length+1<0) {
			return false;
		}
		return compare(src, lastCharPos-dest.length+1, dest);
	}
	
	/**
	 * Look for an XML tag in the passed string, starting at the given position.
	 *  
	 * @param str The string to check
	 * @param pos The position in the <code>str</code> to look for the tag
	 * @return The <code>LogTypehelper</code> corresponding to the XML tag
	 *         <code>null</code> if not XML tag is found at the given position 
	 */
	private LogTypeHelper lookForStartTag(char[] str, int pos) {
		for (int t=0; t<xmlOpeningTags.length; t++) {
			if (compare(str,pos,xmlOpeningTags[t])) {
				return LogTypeHelper.values()[t];
			}
		}
		return null;
	}
	
	public void dump() {
		System.out.println("VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV");
		System.out.println("startTag: "+startTag+"\t at "+startTagPos);
		System.out.println("cdata starts at "+cdataStartPos+", ends at "+cdataEndPos);
		System.out.println("Buffer (size: "+buffer.length+", pos:"+bufferPos+"): "+(new String(buffer).substring(0, bufferPos)));
		System.out.println("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^");
	}
}
