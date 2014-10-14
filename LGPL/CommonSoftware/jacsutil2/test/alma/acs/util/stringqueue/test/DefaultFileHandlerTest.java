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
package alma.acs.util.stringqueue.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.RandomAccessFile;
import java.util.Vector;

import alma.acs.util.stringqueue.DefaultQueueFileHandlerImpl;
import alma.acs.util.stringqueue.DefaultXmlQueueFileHandlerImpl;
import alma.acs.util.stringqueue.TimestampedStringQueue;
import junit.framework.TestCase;

/** 
 * The default file handlers are implicitly tested by the other tests
 * of this package. 
 * This class focuses on the content of the files written by the queue
 * on disk.
 * 
 * @author  acaproni
 * @since   2014.6
 */
public class DefaultFileHandlerTest extends TestCase {
	
	/**
	 * Extends DefaultQueueFileHandlerImpl for testing purposes.
	 * 
	 * @author acaproni
	 *
	 */
	public class MyFileHandler extends DefaultQueueFileHandlerImpl {
		
		/**
		 * The number of created files
		 */
		public int createdFiles=0;
		
		/**
		 * The number of processed files
		 */
		public int processedFiles=0;
		
		/**
		 * Constructor
		 * @see DefaultQueueFileHandlerImpl#DefaultQueueFileHandlerImpl(long)
		 */
		public MyFileHandler(long maxSize) {
			super(maxSize);
		}

		@Override
		public void fileProcessed(File filePointer, String minTime,	String maxTime) {
			assertNotNull(filePointer);
			System.out.println("DefaultQueueFileHandlerImpl.fileProcessed(File , String , String ) invoked");
			processedFiles++;
			if (processedFiles==1) {
				// Check the length of the file
				assertTrue(filePointer.getName()+" is shorter then expected "+filePointer.length(),filePointer.length()>=MAX_FILE_SIZE);
			}
			super.fileProcessed(filePointer, minTime, maxTime);
			// The file should have been deleted now
			assertFalse("The file should have been deleted",filePointer.exists());
		}

		@Override
		public File getNewFile() throws IOException {
			File temp=super.getNewFile();
			assertNotNull(temp);
			createdFiles++;
			System.out.println("DefaultQueueFileHandlerImpl.getNewFile() invoked "+temp.getName());
			return temp;
		}
		
	}
	
	/**
	 * Extends DefaultQueueFileHandlerImpl for testing purposes.
	 * 
	 * @author acaproni
	 *
	 */
	class MyXmlFileHandler extends DefaultXmlQueueFileHandlerImpl {
		
		/**
		 * The number of created files
		 */
		public int createdFiles=0;
		
		/**
		 * The number of processed files
		 */
		public int processedFiles=0;
		
		/**
		 * Constructor
		 * @see DefaultXmlQueueFileHandlerImpl#DefaultXmlQueueFileHandlerImpl(long, String)
		 */
		public MyXmlFileHandler(long maxSize, String tag) {
			super(maxSize,tag);
		}
		
		/**
		 * Check if the file contains the XML tags
		 * 
		 * @param filePointer
		 */
		private void checkFile(File filePointer) {
			try {
				
				FileInputStream fis = new FileInputStream(filePointer);
				BufferedReader br = new BufferedReader(new InputStreamReader(fis));
				
				// Check if the file begins with XML TAGs
				String xmlStartTag=br.readLine();
				assertEquals(DefaultXmlQueueFileHandlerImpl.standardXmlHdr.trim(), xmlStartTag);
				String myXmlTag=br.readLine();
				assertEquals("<"+xmlTag+">", myXmlTag);
				String xmlHdr=br.readLine();
				assertEquals(DefaultXmlQueueFileHandlerImpl.xmlHeader.trim(), xmlHdr);
				
				// Cannot check if the terminate tag is added at the end of the file
				// because super.fileProcessed deletes the file before exiting!
				
				// Done with the file
				br.close();
				br = null;
				fis = null;
			} catch (Throwable t) {
				System.err.println("Exception caught checning the file: "+t.getMessage());
				t.printStackTrace(System.err);
			}
		}
		
		@Override
		public void fileProcessed(File filePointer, String minTime,	String maxTime) {
			assertNotNull(filePointer);
			System.out.println("DefaultQueueFileHandlerImpl.fileProcessed(File , String , String ) invoked");
			processedFiles++;
			if (processedFiles==1) {
				// Check the length of the file
				assertTrue(filePointer.getName()+" is shorter then expected "+filePointer.length(),filePointer.length()>=MAX_FILE_SIZE);
			}
			checkFile(filePointer);
			super.fileProcessed(filePointer, minTime, maxTime);
			// The file should have been deleted now
			assertFalse("The file should have been deleted",filePointer.exists());
		}

		@Override
		public File getNewFile() throws IOException {
			File temp=super.getNewFile();
			assertNotNull(temp);
			createdFiles++;
			System.out.println("DefaultXmlQueueFileHandlerImpl.getNewFile() invoked");
			return temp;
		}
	}
	
	/**
	 * Max length of each file of the cache for this test
	 */
	private static final int MAX_FILE_SIZE=16384;
	
	/**
	 * The Tag to write in the XML file
	 */
	private static final String xmlTag="TestTAG";
	
	/**
	 * The string to identifie the timestamp in the strings pushed in
	 * the queue
	 */
	private final String timestampIdentifier="DefaultFileHandlerTest-";
	
	/**
	 * Constructor
	 */
	public DefaultFileHandlerTest() {
		super("DefaultFileHandlerTest");
	}
	
	/**
	 * Generate the strings to put in the cache.
	 * 
	 * The number of strings to put in the vector depends by the passed
	 * parameter. The sum of the length of all the strings in the
	 * vector is equal or greater to the passed parameter.
	 * In this way it is possible to check if the cache creates/deletes a
	 * file in the right moment.
	 *  
	 * @param size The length of all the strings in the vector
	 * @return A vector of strings
	 * 
	 */
	private Vector<String> generateStrings(int size) {
		Vector<String> strings = new Vector<String>();
		int currentSz=0;
		long t=0;
		while (currentSz<size) {
			String str = timestampIdentifier+"2005-12-02T13:45:02.761\" "+(t++);
			currentSz+=str.length();
			strings.add(str);
		}
		return strings;
	}

	@Override
	protected void setUp() throws Exception {
		super.setUp();
	}

	@Override
	protected void tearDown() throws Exception {
		super.tearDown();
	}
	
	/**
	 * Push strings in cache to generate 2 files.
	 * <P>
	 * The checks are done by {@link MyFileHandler}
	 * 
	 * @throws Exception
	 */
	public void testDefaultQueueFileHandlerImpl() throws Exception {
		// We want the cache to generate 2 files
		Vector<String> strToPush=generateStrings(MAX_FILE_SIZE+2048);
		MyFileHandler fHandler = new MyFileHandler(MAX_FILE_SIZE);
		TimestampedStringQueue queue=new TimestampedStringQueue(fHandler,timestampIdentifier);
		queue.start();
		
		int t=0;
		while (queue.getActiveFilesSize()<2) {
			queue.push(strToPush.get(t++));
			assertEquals("Inconsistent number of files in cache",fHandler.createdFiles, queue.getActiveFilesSize());
		}
		queue.close(true);
		
	}
	
	/**
	 * Push strings in cache to generate 2 files.
	 * <P>
	 * The checks are done by {@link MyXmlFileHandler}
	 * 
	 * @throws Exception
	 */
	public void testDefaultXmlQueueFileHandlerImpl() throws Exception {
		// We want the cache to generate 2 files
		Vector<String> strToPush=generateStrings(MAX_FILE_SIZE+2048);
		MyXmlFileHandler fHandler = new MyXmlFileHandler(MAX_FILE_SIZE,xmlTag);
		TimestampedStringQueue queue=new TimestampedStringQueue(fHandler,timestampIdentifier);
		queue.start();
		
		int t=0;
		while (queue.getActiveFilesSize()<2) {
			queue.push(strToPush.get(t++));
			assertEquals("Inconsistent number of files in cache",fHandler.createdFiles, queue.getActiveFilesSize());
		}
		queue.close(true);
		
	}
	
}
