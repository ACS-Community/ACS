package com.cosylab.logging.engine.ACS;

import java.util.Vector;

import junit.framework.TestCase;

import com.cosylab.logging.engine.log.LogEntryXML;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.ILogEntry.Field;

import com.cosylab.logging.engine.ACS.ACSLogParserDOM;

public class ACSLogParserTest extends TestCase {

	private ACSLogParser parser;

	private final String xmlLogInfo1 = 
		"<Info TimeStamp=\"2006-03-28T00:26:29.238\" File=\"maciContainerImpl.cpp\" Line=\"1454\" " + 
		"Routine=\"maci::ContainerImpl::initThread\" Host=\"gas\" Process=\"maciContainer\" Thread=\"ARCHIVE_BULKSENDER::actionThread\" " + 
		"Context=\"\" SourceObject=\"ARCHIVE_BULKSENDER::actionThread\">" + 
		"<![CDATA[Thread name: 'ARCHIVE_BULKSENDER::actionThread']]>" + 
		"</Info>";
	
	private final String xmlLogInfo2 = 
		"<Info TimeStamp=\"2006-03-28T00:26:29.239\" File=\"maciContainerImpl.cpp\" Line=\"1454\" " + 
		"Routine=\"maci::ContainerImpl::initThread\" Host=\"gas\" Process=\"maciContainer\" Thread=\"ARCHIVE_BULKSENDER::monitorThread\" " + 
		"Context=\"\" SourceObject=\"ARCHIVE_BULKSENDER::monitorThread\">" + 
		"<![CDATA[Thread name: 'ARCHIVE_BULKSENDER::monitorThread']]>" + 
		"</Info>";
	
	private final String xmlLogWarningWithException = 
		"<Warning TimeStamp=\"2006-03-28T00:26:46.149\" " + 
		"File=\"alma.acs.container.ContainerSealant\" Line=\"184\" Routine=\"invoke\" Host=\"gas\" " + 
		"Process=\"LoggerName: alma.acs.container.ARCHIVE/ACC/javaContainer\" SourceObject=\"ARCHIVE/ACC/javaContainer\" " + 
		"Thread=\"RequestProcessor-15\" StackId=\"unknown\" StackLevel=\"0\" LogId=\"298\">" + 
		"<![CDATA[checked exception was thrown in functional method 'ARCHIVE_CONNECTION/alma.xmlstore.Operational#retrieve':]]>" +
		"<Data Name=\"LoggedException\"><![CDATA[alma.xmlstore.OperationalPackage.NotFound: uid://X00000000000028aa/X00000002" + "\n" +
		"	at alma.archive.components.OperationalImpl.retrieve(OperationalImpl.java:450)" + "\n" +
		"	at alma.archive.components.OperationalImpl.retrieve(OperationalImpl.java:480)" + "\n" +
		"	at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)" + "\n" +
		"	at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:39)" + "\n" +
		"	at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:25)" + "\n" +
		"	at java.lang.reflect.Method.invoke(Method.java:585)" + "\n" +
		"	at alma.acs.container.ContainerSealant.invoke(ContainerSealant.java:155)" + "\n" +
		"	at $Proxy3.retrieve(Unknown Source)" + "\n" +
		"	at alma.xmlstore.OperationalPOATie.retrieve(OperationalPOATie.java:103)" + "\n" +
		"	at alma.xmlstore.OperationalPOA._invoke(OperationalPOA.java:316)" + "\n" +
		"	at org.jacorb.poa.RequestProcessor.invokeOperation(Unknown Source)" + "\n" +
		"	at org.jacorb.poa.RequestProcessor.process(Unknown Source)" + "\n" +	
		"	at org.jacorb.poa.RequestProcessor.run(Unknown Source)" + "\n" +
		"]]></Data></Warning>";


	
	protected void setUp() throws Exception {
		super.setUp();
		parser = new ACSLogParserDOM();
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	
	/**
	 * Parses one log record from XML and verifies a few fields, 
	 * including the exception details that are attached as additional data.
	 * 
	 * @throws Exception
	 */
	public void testParseLogRecord() throws Exception {
		ILogEntry log = parser.parse(xmlLogWarningWithException);
		
		// verify some fields
		assertEquals("wrong typename string", "Warning", LogTypeHelper.getLogTypeDescription((Integer)log.getField(Field.ENTRYTYPE)));
		assertEquals("wrong type code", new Integer(LogTypeHelper.ENTRYTYPE_WARNING), log.getField(Field.ENTRYTYPE));
		
		Vector<ILogEntry.AdditionalData> datas = log.getAdditionalData();
		assertFalse("There should have been 1 piece of attached data!", datas == null || datas.size() != 1);
		Vector<ILogEntry.AdditionalData> additionalData = log.getAdditionalData();
		assertFalse("There should have been 1 piece of additional data!", additionalData == null || additionalData.size() != 1);
		ILogEntry.AdditionalData myData = additionalData.get(0);
		assertEquals("LoggedException", myData.getName());
		assertTrue(myData.getValue().startsWith("alma.xmlstore.OperationalPackage.NotFound: uid://X00000000000028aa/X00000002"));
	}
}
