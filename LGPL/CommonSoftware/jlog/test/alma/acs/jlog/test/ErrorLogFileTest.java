/*
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2008 
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
package alma.acs.jlog.test;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;

import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.logging.dialogs.error.ErrorLogFile;

public class ErrorLogFileTest extends ComponentClientTestCase {
	
	/**
	 * The object to test
	 */
	private ErrorLogFile file;
	
	/**
	 * The timeout
	 */
	private final int TIMEOUT = 15;
	
	/**
	 * Each test writes this string plus an integer.
	 * 
	 * In this way it is possible to check the written file.
	 */
	private final String str= "BaseString";

	/**
	 * Constructor
	 * 
	 * @throws Exception
	 */
	public ErrorLogFileTest() throws Exception {
		super(ErrorLogFileTest.class.getName());
	}

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		file = new ErrorLogFile(TIMEOUT, "test.",".log", null, true, false);
		assertNotNull(file);
	}

	/**
	 * Test the append method
	 */
	public void testAppend() throws Exception {
		
		for (int t=0; t<1000; t++) {
			file.append(str+Integer.valueOf(t).toString()+"\n");
		}
		assertTrue(checkFile(file.getFileName(), 1000,0));
	}
	
	/**
	 * Test the append method with a timeout
	 */
	public void testAppendTimeout() throws Exception {
		for (int t=0; t<1000; t++) {
			file.append(str+Integer.valueOf(t).toString()+"\n");
		}
		try {
			Thread.sleep(2000*TIMEOUT);
		} catch (Exception e) {}
		for (int t=1000; t<2000; t++) {
			file.append(str+Integer.valueOf(t).toString()+"\n");
		}
		try {
			Thread.sleep(2000*TIMEOUT);
		} catch (Exception e) {}
		for (int t=2000; t<3000; t++) {
			file.append(str+Integer.valueOf(t).toString()+"\n");
		}
		assertTrue(checkFile(file.getFileName(), 3000,0));
	}
	
	/**
	 * Check the content of the file with the passed name.
	 * The correctness is ensured if the file exists, is readable
	 * and it is composed of passed number of entries. 
	 * 
	 * @param name The name of the file
	 * @param numOfEntries The number of entries in the file
	 * @param firstEntryNum The number of the first entry in the file
	 * @return <code>true</code> If the content of the file is as expected
	 */
	private boolean checkFile(String name, int numOfEntries, int firstEntryNum) throws Exception {
		File f = new File(name);
		assertTrue(f.isFile());
		assertTrue(f.canRead());
		
		FileInputStream inF = new FileInputStream(name);
		assertNotNull(inF);
		
		StringBuilder readStrBuilder = new StringBuilder();
		
		byte[] buffer = new byte[1024];
		int bytesRead=-1;
		do {
			bytesRead = inF.read(buffer);
			if (bytesRead>0) {
				readStrBuilder.append(new String(buffer,0,bytesRead));
			}
		} while (bytesRead!=-1);
		// Check the number of lines in the file
		String[] strings = readStrBuilder.toString().split("\n");
		assertEquals(numOfEntries, strings.length);
		// Check the format of the strings
		for (int t=0; t<numOfEntries; t++) {
			String entry = str+(firstEntryNum+t);
			assertEquals(entry, strings[t]);
		}
		return true;
	}
	
	/**
	 * Test the copy method of the <code>ErrorLogFile</code>
	 * 
	 * @throws Exception
	 */
	public void testCopy() throws Exception {
		File f = File.createTempFile("test.", ".copy.log",new File("."));
		f.deleteOnExit();
		FileOutputStream copyFile = new FileOutputStream(f);
		
		for (int t=0; t<1000; t++) {
			file.append(str+Integer.valueOf(t).toString()+"\n");
		}
		
		file.copy(copyFile);
		
		copyFile.close();
		
		assertTrue(checkFile(f.getAbsolutePath(), 1000,0));
	}
	
	/**
	 * Test the clear method of the <code>ErrorLogFile</code>
	 * 
	 * @throws Exception
	 */
	public void testClear() throws Exception {
		// Add some strings to the file
		String oldName = file.getFileName();
		for (int t=0; t<1000; t++) {
			file.append(str+Integer.valueOf(t).toString()+"\n");
		}
		file.clear();
		for (int t=1000; t<2000; t++) {
			file.append(str+Integer.valueOf(t).toString()+"\n");
		}
		String newName = file.getFileName();
		assertTrue(checkFile(file.getFileName(), 1000,1000));
		assertNotSame(newName+" and "+oldName+" should differ after clearing",newName, oldName);
	}
}
