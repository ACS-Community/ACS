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
package alma.acs.jlog.test;

import java.io.File;
import java.io.FileWriter;
import java.util.Collection;
import java.util.Iterator;
import java.util.Properties;
import java.util.Vector;

import com.cosylab.logging.engine.ACS.ACSRemoteErrorListener;
import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogEntryXML;
import com.cosylab.logging.engine.log.ILogEntry.AdditionalData;
import com.cosylab.logging.engine.log.LogField;

import junit.framework.TestCase;
import alma.acs.logging.engine.io.IOHelper;
import alma.acs.logging.engine.io.IOPorgressListener;
import alma.acs.logging.engine.parser.ACSLogParser;
import alma.acs.logging.engine.parser.ACSLogParserFactory;

/**
 * A class testing the load and save facilities
 * <P>
 * As a general rule, each test of this class
 * <OL>
 * 	<LI>generate some logs with the help of <code>CacheUtils</code>,
 * 	<LI>save and load the logs.
 * 	<LI>check if the logs written and those read are equals. 
 * </OL>
 * 
 * @author acaproni
 *
 */
public class LoadSaveTest extends TestCase implements IOPorgressListener, ACSRemoteLogListener, ACSRemoteErrorListener {
	
	// Some interesting logs to check 
	// See COMP-2369 for log1
	private final String log1 ="<Trace TimeStamp=\"2008-04-14T17:53:22.445\" SourceObject=\"CONTROL/ACC/cppContainer-GL\" "+
	"File=\"Unavailable\" Line=\"0\" Routine=\"\" Host=\"gas01\" Process=\"CONTROL/ACC/cppContainer\" "+
	"Context=\"\" Thread=\"CONTROL/Array002/DELAYSERVERServiceThread\">"+
	"<![CDATA[AlarmSupplier::publishEvent()\n"+
    "About to send XML of:\n"+
    "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n"+
    "<ASI-message xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" backup=\"false\" version=\"0.9\" xsi:type=\"ASI-message\">\n"+
	"<source-name>ALARM_SYSTEM_SOURCES</source-name>\n"+
	"<source-hostname>gas01</source-hostname>\n"+
	"<source-timestamp seconds=\"1208195602\" microseconds=\"433703\"/>\n"+
	"<fault-states>\n"+
	"<fault-state family=\"DelayServer\" member=\"CONTROL/Array002\" code=\"1\">\n"+
	"<descriptor>TERMINATE</descriptor>\n"+
	"<user-timestamp seconds=\"1208195602\" microseconds=\"433103\"/>\n"+
	"</fault-state>\n"+
	"</fault-states>\n"+
	"</ASI-message>\n"+
	"]]></Trace>\n"; 
		
	private final String log2 = "<Warning TimeStamp=\"2005-12-13T15:09:12.041\" SourceObject=\"ARCHIVE_MASTER_COMP\" " +
	"File=\"alma.ACS.MasterComponentImpl.MasterComponentImplBase\"  "+
	"Line=\"164\" Routine=\"doTransition\" Host=\"gas\" Process=\"LoggerName: alma.component.ARCHIVE_MASTER_COMP\" "+
	"Thread=\"RequestProcessor-10\" StackId=\"unknown\" StackLevel=\"0\" LogId=\"140\">"+
	"<![CDATA[Illegal event.]]><Data Name=\"LoggedException\">alma.acs.genfw.runtime.sm.AcsStateIllegalEventException: illegal event 'initPass1' in state 'ONLINE'.\n"+
	"at alma.ACS.MasterComponentImpl.statemachine.AlmaSubsystemContext.illegalEvent(AlmaSubsystemContext.java:248)\n"+
    "at alma.ACS.MasterComponentImpl.statemachine.AvailableSubStateAbstract.initPass1(AvailableSubStateAbstract.java:30)\n"+
    "at alma.ACS.MasterComponentImpl.statemachine.AvailableState.initPass1(AvailableState.java:60)\n"+
    "at alma.ACS.MasterComponentImpl.statemachine.AlmaSubsystemContext.initPass1(AlmaSubsystemContext.java:135)\n"+
    "at alma.ACS.MasterComponentImpl.MasterComponentImplBase.doTransition(MasterComponentImplBase.java:134)\n"+
    "at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)\n"+
    "at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:39)\n"+
    "at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:25)\n"+
    "at java.lang.reflect.Method.invoke(Method.java:324)\n"+
    "at alma.acs.container.ContainerSealant.invoke(ContainerSealant.java:134)\n"+
    "at $Proxy0.doTransition(Unknown Source)\n"+
    "at alma.archive.ArchiveSubsystemMasterIFPOATie.doTransition(ArchiveSubsystemMasterIFPOATie.java:63)\n"+
    "at alma.archive.ArchiveSubsystemMasterIFPOA._invoke(ArchiveSubsystemMasterIFPOA.java:76)\n"+
    "at org.jacorb.poa.RequestProcessor.invokeOperation(Unknown Source)\n"+
    "at org.jacorb.poa.RequestProcessor.process(Unknown Source)\n"+
    "at org.jacorb.poa.RequestProcessor.run(Unknown Source)\n"+
    "</Data></Warning>";
	
	private String[] specialLogs = { log1, log2 };
	
	// The number of bytes read and written
	private volatile long bytesRead, bytesWritten;
	
	/**
	 * The logs to read and write
	 */
	private Collection<ILogEntry> logs;
	
	/**
	 * The logs read form a file
	 * <P>
	 * This collection is filled by <code>logEntryReceived()</code>.
	 */
	private Vector<ILogEntry> logsRead;
	
	/**
	 * The number of logs read since the beginning of a load
	 */
	private int numOfLogsRead;
	
	/**
	 * The number of logs read since the beginning of a load
	 */
	private int numOfLogsWritten;
	
	/**
	 * The number of logs in <code>logs</code> to load/save
	 */
	private static final int NUMBER_OF_LOGS=1500;
	
	/**
	 * The directory where the logs are read and written.
	 * <P>
	 * It comes form the <code>user.dir</code> property. 
	 */
	private String folder;
	private static final String USER_DIR = "user.dir";
	
	/**
	 * The name of the file used for testing
	 */
	private String fileName;
	
	/**
	 * The name of the file used for testing compressed load and save
	 */
	private String gzipFileName;
	
	/**
	 * @see alma.acs.logging.engine.io.IOPorgressListener#bytesRead(long)
	 */
	@Override
	public void bytesRead(long bytes) {
		bytesRead=bytes;
	}

	/**
	 * @see alma.acs.logging.engine.io.IOPorgressListener#bytesWritten(long)
	 */
	@Override
	public void bytesWritten(long bytes) {
		bytesWritten=bytes;
	}

	/**
	 * @see junit.framework.TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		bytesRead=0;
		bytesWritten=0;
		numOfLogsRead=0;
		logs=CacheUtils.generateLogs(NUMBER_OF_LOGS);
		assertEquals(NUMBER_OF_LOGS, logs.size());
		// Read the folder name and generate the file name
		Properties props = System.getProperties();
		assertNotNull(props);
		folder = props.getProperty(USER_DIR);
		assertNotNull(folder);
		File folderFile = new File(folder);
		String folderPath = folderFile.getAbsolutePath();
		fileName = folderPath+"/logs.xml";
		gzipFileName=fileName+".gz";
		// Check if folder is  a writable directory
		assertTrue(folderFile.isDirectory());
		assertTrue(folderFile.canWrite());
		
		logsRead = new Vector<ILogEntry>();
	}

	/**
	 * @see junit.framework.TestCase#tearDown()
	 */
	@Override
	protected void tearDown() throws Exception {
		super.tearDown();
		// Delete the file
		File f = new File(fileName);
		f.deleteOnExit();
		// Delete the compressed file
		File compressed = new File(fileName+".gz");
		if (compressed.exists()) {
			compressed.deleteOnExit();
		}
		logs.clear();
		logs=null;
		logsRead.clear();
		logsRead=null;
	}
	
	/**
	 * Load and save a collection of logs in uncompressed files
	 * <P>
	 * The save is performed by passing the name of the file
	 *  
	 * @throws Exception
	 */
	public void testSaveLoad() throws Exception {
		IOHelper ioHelper = new IOHelper();
		assertNotNull(ioHelper);
		
		long assumedLen=0;
		for (ILogEntry log: logs) {
			char[] chars = (log.toXMLString()+"\n").toCharArray();
			assumedLen+=chars.length;
		}
		
		// Save the logs on file
		ioHelper.saveLogs(fileName, logs, this, false,false);
		assertEquals(assumedLen, bytesWritten);
		assertEquals(logs.size(), numOfLogsWritten);
		
		// Read the logs
		ioHelper.loadLogs(fileName, this, null, this, this,false);
		assertEquals(logs.size(),numOfLogsRead);
		assertTrue(bytesRead>assumedLen); // bytes read includes the XML header
	
		// Compare the 2 collections
		assertEquals(logs.size(), logsRead.size());
		
		int t=0;
		for (ILogEntry log: logs) {
			String logXML = log.toXMLString();
			String logReadXML = logsRead.get(t++).toXMLString();
			assertEquals(logXML, logReadXML);
		}
	}
	
	/**
	 * Load and save a collection of logs in uncompressed files
	 * <P>
	 * The save is performed by passing the name of the file
	 *  
	 * @throws Exception
	 */
	public void testSaveLoadCompressed() throws Exception {
		IOHelper ioHelper = new IOHelper();
		assertNotNull(ioHelper);
		
		// Save the logs on file
		ioHelper.saveLogs(gzipFileName, logs, this, false,true);
		assertEquals(logs.size(), numOfLogsWritten);
		
		// Read the logs
		ioHelper.loadLogs(gzipFileName, this, null, this, this,true);
		assertEquals(logs.size(),numOfLogsRead);
	
		// Compare the 2 collections
		assertEquals(logs.size(), logsRead.size());

		// Compare the 2 collections
		int t=0;
		for (ILogEntry log: logs) {
			String logXML = log.toXMLString();
			String logReadXML = logsRead.get(t++).toXMLString();
			assertEquals(logXML, logReadXML);
		}
	}

	/**
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteErrorListener#errorReceived(java.lang.String)
	 */
	@Override
	public void errorReceived(String xml) {
		System.err.println("ERROR: "+xml);
	}

	/**
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteLogListener#logEntryReceived(com.cosylab.logging.engine.log.ILogEntry)
	 */
	@Override
	public void logEntryReceived(ILogEntry logEntry) {
		logsRead.add(logEntry);
	}

	/**
	 * @see alma.acs.logging.engine.io.IOPorgressListener#logsRead(int)
	 */
	@Override
	public void logsRead(int numOfLogs) {
		numOfLogsRead=numOfLogs;
	}
	
	/**
	 * @see alma.acs.logging.engine.io.IOPorgressListener#logsWritten(int)
	 */
	@Override
	public void logsWritten(int numOfLogs) {
		numOfLogsWritten=numOfLogs;
	}
	
	/**
	 * Load a set of special logs that could be interesting.
	 * <P>
	 * Initially this method opens an XML file and writes into it the special logs without using
	 * <code>IOHelper</code>.
	 * Then it tries to load the file containing the special logs
	 * 
	 * 
	 * @throws Exception
	 */
	public void testLoadingSpecialLogs() throws Exception {
		FileWriter f = new FileWriter(fileName,false);
		f.write("<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n<Log>\n<Header Name=\"NameForXmlDocument\" Type=\"LOGFILE\" />\n");
		for (String xml: specialLogs) {
			f.write(xml);
		}
		f.write("</Log>");
		f.flush();
		f.close();
		
		IOHelper ioHelper = new IOHelper();
		assertNotNull(ioHelper);
		
		ioHelper.loadLogs(fileName, this, null, this, this,false);
		// Check if the number of records is the same
		assertEquals(specialLogs.length, logsRead.size());
	}
	
	/**
	 * Test the append switch while saving logs by saving the same
	 * collection twice in the same file but with the append switch set to
	 * <code>true</code>.
	 * @throws Exception
	 */
	public void testAppend() throws Exception {
		IOHelper ioHelper = new IOHelper();
		assertNotNull(ioHelper);
		
		// First save with no append
		ioHelper.saveLogs(fileName, logs, this, false,false);
		// Second save with append
		ioHelper.saveLogs(fileName, logs, this, true,false);
		
		// Load the logs
		ioHelper.loadLogs(fileName, this, null, this, this,false);
		assertEquals(2*logs.size(), logsRead.size());
		assertEquals(2*logs.size(), numOfLogsRead);
	}
	
	/**
	 * Check the load and save of logs by passing an <code>Iterator</code>
	 * 
	 * @throws Exception
	 */
	public void testSaveLoadIterator() throws Exception {
		IOHelper ioHelper = new IOHelper();
		assertNotNull(ioHelper);
		
		Iterator<ILogEntry> iterator = logs.iterator();
		
		long assumedLen=0;
		for (ILogEntry log: logs) {
			char[] chars = (log.toXMLString()+"\n").toCharArray();
			assumedLen+=chars.length;
		}
		
		// Save the logs on file
		ioHelper.saveLogs(fileName, iterator, this, false,false);
		assertEquals(assumedLen, bytesWritten);
		assertEquals(logs.size(), numOfLogsWritten);
		
		// Read the logs
		ioHelper.loadLogs(fileName, this, null, this, this,false);
		assertEquals(logs.size(),numOfLogsRead);
	}
	
	/**
	 * Save and Load the special logs then check their fields.
	 * <P>
	 * This test is performed comparing the fields of the special logs one by one
	 * instead of comparing the XMLs.
	 * The test does the following steps:
	 * <OL>
	 * 	<LI>build vector of logs to save from the XMLs in <code>specialLogs</code>
	 *  <LI>save the vector of logs with an <code>IOHelper</code> object
	 *  <LI>load the logs from file
	 *  <LI>compare each field of the logs read from the file with those in <code>specialLogs</code>
	 *  <LI>compare the additional data names and values
	 * </OL>
	 * <P>
	 * The test implicitly checks the conversion between XML and ILogEntry too. 
	 */
	public void testSaveLoadFields() throws Exception {
		ACSLogParser parser = ACSLogParserFactory.getParser();
		assertNotNull(parser);
		//Build the logs from the XML
		Vector<ILogEntry> logsToCheck = new Vector<ILogEntry>();
		for (int t=0; t<specialLogs.length; t++) {
			ILogEntry log =parser.parse(specialLogs[t]);
			assertNotNull(log);
			logsToCheck.add(log);
		}
		
		// Save the logs on disk
		IOHelper ioHelper = new IOHelper();
		assertNotNull(ioHelper);
		ioHelper.saveLogs(fileName, logsToCheck, this, false,false);
		assertEquals(logsToCheck.size(), numOfLogsWritten);
		
		// Load the logs from disk
		ioHelper.loadLogs(fileName, this, null, this, this,false);
		assertEquals(logsRead.size(),logsToCheck.size());
		
		// Iterate over the logs comparing each field
		for (int t=0; t<logsToCheck.size(); t++) {
			ILogEntry originalLog = logsToCheck.elementAt(t);
			assertNotNull(originalLog);
			ILogEntry savedLog = logsRead.elementAt(t);
			assertNotNull(savedLog);
			
			// Check the fields
			for (LogField f: LogField.values()) {
				Object original= originalLog.getField(f);
				Object saved=savedLog.getField(f);
				assertEquals("Fields "+f+" differ",original, saved);
			}
			// Check additional data
			assertEquals(originalLog.hasDatas(), savedLog.hasDatas());
			if (originalLog.hasDatas()) {
				Vector<AdditionalData> originalData = originalLog.getAdditionalData();
				assertNotNull(originalData);
				Vector<AdditionalData> savedData = savedLog.getAdditionalData();
				assertNotNull(savedData);
				assertEquals(originalData.size(), savedData.size());
				for (int count=0; count<originalData.size(); count++) {
					AdditionalData originalDataItem = originalData.elementAt(count);
					assertNotNull(originalDataItem);
					AdditionalData savedDataItem = savedData.elementAt(count);
					assertNotNull(savedDataItem);
					
					assertEquals("Data names differ", originalDataItem.name, savedDataItem.name);
					assertEquals("Data values differ", originalDataItem.value, savedDataItem.value);
					
				}
			}
		}
	}
}
