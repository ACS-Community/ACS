/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2014 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.acs.util.stringqueue;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;

/** 
 * A specialized class of {@link DefaultQueueFileHandlerImpl} to be used to store
 * XML strings in the file of the queue by appending the XML header and footer at the beginning and at the
 * end of each file of the cache.
 * The constructor allow the set the TAG of the items contained in the files.
 * <BR>
 * Objects of this class delegate to {@link DefaultQueueFileHandlerImpl} but for the {@link #getNewFile()}
 * and {@link #fileProcessed(java.io.File, String, String)}.
 * The former writes the XML header at the beginning of the file before
 * returning the fiole to the caller. The latter append the XML footer at the end of the file.
 * 
 * @author  acaproni
 * @since   2014.6
 */
public class DefaultXmlQueueFileHandlerImpl extends DefaultQueueFileHandlerImpl {
	
	/**
	 * Standard XML header
	 */
	public static final String standardXmlHdr="<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
	
	/**
	 * The header for XML 
	 */
	public static final String xmlHeader= "\n<Header Name=\"File written by DefaultXmlQueueFileHandler\" />\n";
	
	/**
	 * The XML TAG of the items in each file of the cache
	 */
	private final String xmlTag;
	
	/**
	 * The XML tag for the items contained in the XML.
	 * THis tag encloses the items written by the queue i.e. there is one opening TAG
	 * and one closing tag in each file of the queue.
	 * 
	 * @param xmlTag The tag of the items in each XML file of the queue
	 * @see DefaultQueueFileHandlerImpl
	 */
	public DefaultXmlQueueFileHandlerImpl(String xmlTag) {
		super();
		if (xmlTag==null || xmlTag.isEmpty()) {
			throw new IllegalArgumentException("The XML TAG can't be null nor empty");
		}
		this.xmlTag=xmlTag;
	}

	/**
	 * @param maxFileSize The max size of the files of the cache.
	 * @param prefix The prefix of the name of the cache files
	 * @param xmlTag The tag of the items in each XML file of the queue
	 * @see DefaultQueueFileHandlerImpl
	 */
	public DefaultXmlQueueFileHandlerImpl(long maxFileSize, String prefix, String xmlTag) {
		super(maxFileSize, prefix);
		if (xmlTag==null || xmlTag.isEmpty()) {
			throw new IllegalArgumentException("The XML TAG can't be null nor empty");
		}
		this.xmlTag=xmlTag;
	}

	/**
	 * @param maxFileSize The max size of the files of the cache.
	 * @param xmlTag The tag of the items in each XML file of the queue
	 * @see DefaultQueueFileHandlerImpl
	 */
	public DefaultXmlQueueFileHandlerImpl(long maxFileSize, String xmlTag) {
		super(maxFileSize);
		if (xmlTag==null || xmlTag.isEmpty()) {
			throw new IllegalArgumentException("The XML TAG can't be null nor empty");
		}
		this.xmlTag=xmlTag;
	}

	/**
	 * @param prefix The prefix of the name of the cache files
	 * @param xmlTag The tag of the items in each XML file of the queue
	 * @see DefaultQueueFileHandlerImpl
	 */
	public DefaultXmlQueueFileHandlerImpl(String prefix, String xmlTag) {
		super(prefix);
		if (xmlTag==null || xmlTag.isEmpty()) {
			throw new IllegalArgumentException("The XML TAG can't be null nor empty");
		}
		this.xmlTag=xmlTag;
	}

	/**
	 * Append the XML footer to the passed file before calling 
	 * {@link DefaultQueueFileHandlerImpl#fileProcessed(File, String, String)}
	 */
	@Override
	public void fileProcessed(File filePointer, String minTime, String maxTime) {
		try {
			RandomAccessFile rf = new RandomAccessFile(filePointer.getAbsolutePath(), "rw");
			rf.seek(rf.length());
			rf.writeBytes( "\n</"+xmlTag+">");
			rf.close();
		} catch (Throwable t) {
			// A error happening writing the footer will be ignored (but still we want to print it out)
			System.err.println("Error writing the footer: "+t.getMessage());
			t.printStackTrace(System.err);
		}
		super.fileProcessed(filePointer, minTime, maxTime);
	}

	/**
	 * Get the file from {@link DefaultQueueFileHandlerImpl#getNewFile()} and write the XML header
	 * at the beginning of the file before returning to the caller.
	 * 
	 * @see {@link DefaultQueueFileHandlerImpl#getNewFile()}
	 */
	@Override
	public File getNewFile() throws IOException {
		File temp= super.getNewFile();
		RandomAccessFile rf = new RandomAccessFile(temp.getAbsolutePath(), "rw");
		rf.seek(0);
		rf.writeBytes(standardXmlHdr+"<"+xmlTag+">"+xmlHeader);
		rf.close();
		return temp;
	}
	
}
