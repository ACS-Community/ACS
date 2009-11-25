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
package alma.acs.jlog.test.tools;

import java.util.Vector;

import alma.acs.logging.engine.parser.ACSLogParser;
import alma.acs.logging.engine.parser.ACSLogParserFactory;
import alma.acs.logging.engine.parser.ACSLogParserFactory.ParserTypes;
import alma.acs.logging.tools.CSVConverter;
import alma.acs.logging.tools.TextConverter;
import alma.acs.logging.tools.TwikiTableConverter;
import alma.acs.logging.tools.XMLConverter;

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogField;
import com.cosylab.logging.engine.log.ILogEntry.AdditionalData;

import junit.framework.TestCase;

/**
 * Test the conversion of a log with the different converters
 * 
 * @author acaproni
 *
 */
public class ConvertersTest extends TestCase {
	
	private final String timeStamp="\"2006-03-28T00:26:46.149\"";
	private final String type="Warning";
	private final String sourceObject="\"ARCHIVE/ACC/javaContainer\"";
	private final String file="\"alma.acs.container.ContainerSealant\"";
	private final String line="\"184\"";
	private final String routine="\"invoke\"";
	private final String host="\"gas\"";
	private final String process="\"TheProcess\"";
	private final String context="\"TheContext\"";
	private final String thread="\"RequestProcessor-15\"";
	private final String logId="\"298\"";
	private final String priority="\"3\"";
	private final String uri="\"theUri\"";
	private final String stackId="\"unknown\"";
	private final String stackLevel ="\"0\"";
	private final String message="checked exception was thrown in functional method 'ARCHIVE_CONNECTION/alma.xmlstore.Operational#retrieve'";
	private final String audience="\"operator\"";
	private final String array="\"A1\"";
	private final String antenna="\"DV01\"";
	
	private final String DataName1="\"Name\"";
	private final String DataValue1="Value";
	private final String DataName2="\"Pippo\"";
	private final String DataValue2="Pluto";
	
	/**
	 * Convenience data used for testing the results
	 * <P>
	 * The position of each string is the same of the LogField
	 * i.e. LogField.TIMESTAMP.ordinal=0=logFields[0];
	 */
	String[] logFields = {
			timeStamp,
			type,
			sourceObject,
			file,
			line,
			routine,
			host,
			process,
			context,
			thread,
			logId,
			priority,
			uri,
			stackId,
			stackLevel,
			message,
			audience,
			array,
			antenna
	};
	
	/**
	 * The log used for testing
	 */
	private final String xmlLog = 
		"<"+type+
		" TimeStamp=" +timeStamp +
		" File="+file+
		" Line="+line+
		" Routine="+routine+
		" Host="+host+
		" SourceObject="+sourceObject+
		" Process="+process+
		" Context="+context+
		" Thread="+thread+
		" StackId="+stackId+
		" StackLevel="+stackLevel+
		" LogId="+logId+
		" Priority="+priority+
		" URI="+uri+
		" Audience="+audience+
		" Array="+array+
		" Antenna="+antenna+
		">" + 
		"<![CDATA["+message+"]]>" +
		"<Data Name="+DataName1+"><![CDATA["+DataValue1+"]]></Data>"+
		"<Data Name="+DataName2+"><![CDATA["+DataValue2+"]]></Data>"+
		"</"+type+">";
	
	/**
	 * The log to convert
	 */
	ILogEntry logToConvert;
	
	/**
	 * Constructor
	 */
	public ConvertersTest() {
		super(ConvertersTest.class.getName());
	}

	/**
	 * @see junit.framework.TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		ParserTypes type=ParserTypes.VTD;
		ACSLogParser parser=ACSLogParserFactory.getParser(type);
		logToConvert=parser.parse(xmlLog);
	}

	/**
	 * @see junit.framework.TestCase#tearDown()
	 */
	@Override
	protected void tearDown() throws Exception {
		super.tearDown();
	}
	
	/**
	 * Test the XML conversion
	 * 
	 * @throws Exception
	 */
	public void testXML() throws Exception {
		XMLConverter converter = new XMLConverter();
		assertNotNull(converter);
		String ret=converter.convert(logToConvert);
		assertNotNull(ret);
		ParserTypes type=ParserTypes.VTD;
		ACSLogParser parser=ACSLogParserFactory.getParser(type);
		ILogEntry log=parser.parse(ret);
		assertNotNull(log);
		for (LogField f: LogField.values()) {
			Object original=logToConvert.getField(f);
			Object converted=log.getField(f);
			assertEquals(original, converted);
		}
		Vector<AdditionalData> originalData=logToConvert.getAdditionalData();
		assertNotNull(originalData);
		Vector<AdditionalData> convertedData=log.getAdditionalData();
		assertNotNull(convertedData);
		assertEquals(originalData.size(), convertedData.size());
		for (AdditionalData ad: originalData) {
			boolean found=false;
			String name=ad.name;
			String val=ad.value;
			for (AdditionalData adConverted: convertedData) {
				if (name.equals(adConverted.name)) {
					found=true;
					assertEquals(val, adConverted.value);
					break;
				}
			}
			assertTrue("Additional data not found",found);
		}
	}
	
	/**
	 * Test the CSV converter whose converted string
	 * contains all the log fields
	 * 
	 * @throws Exception
	 */
	public void testCSVConverterAllFields() throws Exception {
		CSVConverter converter=new CSVConverter();
		assertNotNull(converter);
		String ret=converter.convert(logToConvert);
		assertNotNull(ret);
		assertFalse("The convertion returned an empty string",ret.isEmpty());
		
		// Build the string that the converted should have returned
		StringBuilder testSB=new StringBuilder();
		for (String str: logFields) {
			if (str.equals(type) || str.equals(message)) {
				testSB.append('"');
				testSB.append(str);
				testSB.append('"');
			} else {
				testSB.append(str);
			}
			testSB.append(',');
		}
		testSB.append("\"[");
		testSB.append(DataName1.replaceAll("\"", ""));
		testSB.append(" ==> ");
		testSB.append(DataValue1);
		testSB.append("] [");
		testSB.append(DataName2.replaceAll("\"", ""));
		testSB.append(" ==> ");
		testSB.append(DataValue2);
		testSB.append("]\"\n");
		assertEquals(testSB.toString(),ret);
	}
	
	/**
	 * Test the setting of a wrong columns string 
	 * i.e. a string that does contain at least one invalid
	 * {@link LogField} ID.
	 *  
	 * @throws Exception
	 */
	public void testSetWrongColumns() throws Exception {
		String cols="0123ABGXA4";
		CSVConverter converter=new CSVConverter();
		boolean exceptionOccurred=false;
		try {
			converter.setCols(cols);
		} catch (IllegalArgumentException ie) {
			exceptionOccurred=true;
		}
		assertTrue(exceptionOccurred);
	}
	
	/**
	 * Test the setting of the columns with a collection of {@link LogField}
	 * 
	 * @throws Exception
	 */
	public void testColumnsVector() throws Exception {
		Vector<LogField> cols = new Vector<LogField>();
		assertNotNull(cols);
		cols.add(LogField.URI);
		cols.add(LogField.CONTEXT);
		TextConverter converter = new TextConverter(null);
		assertNotNull(converter);
		converter.setCols(cols, false);
		String ret= converter.convert(logToConvert);
		assertNotNull(ret);
		String expected=uri.replaceAll("\"", "")+" "+context.replaceAll("\"", "")+"\n";
		assertEquals(expected, ret);
	}
	
	/**
	 * Test the setting of the separator
	 *  
	 * @throws Exception
	 */
	public void testSetSeparator() throws Exception {
		CSVConverter converter=new CSVConverter();
		assertNotNull(converter);
		converter.setSeparator('!');
		String ret=converter.convert(logToConvert);
		converter.setSeparator('#');
		String ret2=converter.convert(logToConvert);
		// ret and ret2 must be equals aparto of the separators
		assertEquals(ret.replaceAll("!", "#"), ret2);
	}
	
	/**
	 * Test the CSV conversion in case a subset of fields is chosen 
	 * @throws Exception
	 */
	public void testCVSSomeFields() throws Exception {
		String cols="";
		cols+=LogField.TIMESTAMP.id;
		cols+=LogField.LOGMESSAGE.id;
		cols+=LogField.AUDIENCE.id;
		cols+=LogField.TIMESTAMP.id;
		assertNotNull(cols);
		CSVConverter converter = new CSVConverter(cols,'$',false);
		String str=converter.convert(logToConvert);
		String[] parts=str.split("\\$");
		assertNotNull(parts);
		assertEquals(cols.length(),parts.length);
		assertEquals(timeStamp.replaceAll("\"", ""), parts[0]);
		assertEquals(message, parts[1]);
		assertEquals(audience.replaceAll("\"", ""), parts[2]);
		assertEquals(timeStamp.replaceAll("\"", ""), parts[3].subSequence(0, parts[3].length()-1));
	}
	
	/**
	 * Test the text converter.
	 * <P>
	 * The test is done on a subset of the columns: it is enough
	 * because the conversion is delegated to the already tested
	 * {@link CSVConverter}
	 * 
	 * @throws Exception
	 */
	public void testTextConverter() throws Exception {
		String cols="";
		cols+=LogField.AUDIENCE.id;
		cols+=LogField.ARRAY.id;
		cols+=LogField.SOURCEOBJECT.id;
		cols+=LogField.ANTENNA.id;
		cols+=LogField.LINE.id;
		TextConverter converter = new TextConverter(cols);
		assertNotNull(converter);
		String txt=converter.convert(logToConvert);
		String expected=audience.replaceAll("\"", "");
		expected+=" "+array.replaceAll("\"", "");
		expected+=" "+sourceObject.replaceAll("\"", "");
		expected+=" "+antenna.replaceAll("\"", "");
		expected+=" "+line.replaceAll("\"", "")+"\n";
		assertEquals(expected, txt);
	}
	
	/**
	 * Test the conversion in twiki table format
	 * <P>
	 * The test is done on a subset of the columns: it is enough
	 * because the conversion is delegated to the already tested
	 * {@link CSVConverter}
	 * 
	 * @throws Exception
	 */
	public void testTwikiTableConverter() throws Exception {
		String cols="";
		cols+=LogField.ROUTINE.id;
		cols+=LogField.HOST.id;
		cols+=LogField.THREAD.id;
		cols+=LogField.STACKLEVEL.id;
		cols+=LogField.STACKID.id;
		TwikiTableConverter converter = new TwikiTableConverter(cols);
		assertNotNull(converter);
		String txt=converter.convert(logToConvert);
		String expected="|"+routine.replaceAll("\"", "");
		expected+="|"+host.replaceAll("\"", "");
		expected+="|"+thread.replaceAll("\"", "");
		expected+="|"+stackLevel.replaceAll("\"", "");
		expected+="|"+stackId.replaceAll("\"", "")+"|\n";
		assertEquals(expected, txt);
	}
	
	/**
	 * Check the generation of the header for the different 
	 * log converters.
	 * @throws Exception
	 */
	public void testGetHeader() throws Exception {
		String cols="";
		cols+=LogField.TIMESTAMP.id;
		cols+=LogField.SOURCEOBJECT.id;
		cols+=LogField.LOGMESSAGE.id;
		cols+=LogField.ENTRYTYPE.id;
		// CSV
		CSVConverter csvConverter = new CSVConverter(cols);
		assertNotNull(csvConverter);
		String csvHdr=csvConverter.getHeader();
		assertNotNull(csvHdr);
		String expectedCsvHdr="\""+LogField.TIMESTAMP.name;
		expectedCsvHdr+="\",\""+LogField.SOURCEOBJECT.name;
		expectedCsvHdr+="\",\""+LogField.LOGMESSAGE.name;
		expectedCsvHdr+="\",\""+LogField.ENTRYTYPE.name+"\"\n";
		assertEquals(expectedCsvHdr, csvHdr);
		
		// XML
		XMLConverter xmlConverter = new XMLConverter();
		assertNotNull(xmlConverter);
		String xmlHdr=xmlConverter.getHeader();
		assertNotNull(xmlHdr);
		assertTrue(xmlHdr.isEmpty());
		
		// Text
		TextConverter txtConverter= new TextConverter(cols);
		assertNotNull(txtConverter);
		String txtHdr=txtConverter.getHeader();
		assertNotNull(txtHdr);
		String expectedTxtHdr=LogField.TIMESTAMP.name;
		expectedTxtHdr+=" "+LogField.SOURCEOBJECT.name;
		expectedTxtHdr+=" "+LogField.LOGMESSAGE.name;
		expectedTxtHdr+=" "+LogField.ENTRYTYPE.name+"\n";
		assertEquals(expectedTxtHdr, txtHdr);
		
		// Twiki table
		TwikiTableConverter ttConverter=new TwikiTableConverter(cols);
		assertNotNull(ttConverter);
		String ttHdr=ttConverter.getHeader();
		assertNotNull(ttHdr);
		String expectedTtHdr="| *"+LogField.TIMESTAMP.name;
		expectedTtHdr+="* | *"+LogField.SOURCEOBJECT.name;
		expectedTtHdr+="* | *"+LogField.LOGMESSAGE.name;
		expectedTtHdr+="* | *"+LogField.ENTRYTYPE.name+"* |\n";
		assertEquals(expectedTtHdr, ttHdr);
	}
}
