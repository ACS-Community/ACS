package com.cosylab.logging.engine.ACS;

import java.util.Vector;

import junit.framework.TestCase;

import org.w3c.dom.NodeList;

import com.cosylab.logging.engine.log.LogEntryXML;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.ILogEntry.AdditionalData;

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
		parser = new ACSLogParser();
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	
	/**
	 * @throws Exception
	 */
	public void testParseLogRecord() throws Exception {
		LogEntryXML log = parser.parse(xmlLogWarningWithException);
		
		// verify some fields
		assertEquals("wrong typename string", "Warning", log.getEntryTypeAsString());
		assertEquals("wrong type code", new Integer(LogTypeHelper.ENTRYTYPE_WARNING), log.getField(LogEntryXML.FIELD_ENTRYTYPE));
		
		NodeList datas = log.getDatas();
		assertFalse("There should have been 1 piece of attached data!", datas == null || datas.getLength() != 1);
		Vector<AdditionalData> additionalData = log.getAdditionalData();
		assertFalse("There should have been 1 piece of additional data!", additionalData == null || additionalData.size() != 1);
		AdditionalData myData = additionalData.get(0);
		assertEquals("LoggedException", myData.getName());
		assertTrue(myData.getValue().startsWith("alma.xmlstore.OperationalPackage.NotFound: uid://X00000000000028aa/X00000002"));
	}
	
	
	/**
	 * This methods checks whether identical strings for a certain field of two log messages 
	 * result in one reused String in memory, or in two redundant strings.
	 * Field names are highly repetitive, which can be seen by the huge compression rate achievable for real life log files.
	 * <p>
	 * As of 2006-04-05 this test fails, and shows what great optimization potential jlog has
	 * if all Strings would be reused from a HashMap (or field-specific HashMaps) or something like that.
	 * 
	 * @throws Exception
	 */
	public void testStringReuse() throws Exception {
		final LogEntryXML record1 = parser.parse(xmlLogInfo1);
		final LogEntryXML record2 = parser.parse(xmlLogInfo2);
		
		String routineName1 = (String) record1.getField(LogEntryXML.FIELD_ROUTINE);
		String routineName2 = (String) record2.getField(LogEntryXML.FIELD_ROUTINE);
		// strings must be equal by design of this test, but are they also the same String in memory?
		assertTrue("Wrong junit test! The two parsed records should have identical routine names.", routineName1.equals(routineName2));		
		assertTrue("Bad jlog implementation: Recurring strings must be reused from a pool to minimize memory consumption!", routineName1 == routineName2);
	}
}
