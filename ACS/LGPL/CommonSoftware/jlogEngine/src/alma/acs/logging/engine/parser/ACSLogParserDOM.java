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
package alma.acs.logging.engine.parser;

import java.io.IOException;
import java.io.StringReader;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import alma.acs.util.XmlNormalizer;

import com.cosylab.logging.engine.ACS.LogParseException;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogEntry;
import com.cosylab.logging.engine.log.LogEntryXML;

/**
 * ACSLogParserDOM is used for parsing an xml string (representing an XML element such as &lt;Info&gt;,&lt;Debug&gt; and so on) 
 * to produce a valid LogEntryXML.
 * <p> 
 * It will typically get the string from the ACSStructuredPushConsumer class where the logs get delivered 
 * from the CORBA Logging Service. 
 */
public class ACSLogParserDOM implements ACSLogParser {
//	private DocumentBuilderFactory factory = null;
	private DocumentBuilder builder = null;
	
	
	/**
	 * ACSLogParserDOM constructor comment.
	 */
	ACSLogParserDOM() throws ParserConfigurationException {
		initialize();
	}
	
	private void initialize() throws ParserConfigurationException {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();

		builder = factory.newDocumentBuilder();
	}

	
	/* (non-Javadoc)
	 * @see com.cosylab.logging.engine.ACS.ACSLogParser#parse(java.lang.String)
	 */
	public synchronized ILogEntry parse(String string) throws LogParseException {
		Document document = null;
		
		try {
			document = builder.parse(new InputSource(new StringReader(string)));
		} catch (IOException ioe) {
			// cannot get here	
			System.err.println("Exception parsing "+ioe.getMessage());
			throw new LogParseException(ioe);
		} catch (Exception e) {
			/* There was an exception parsing the log, but before giving up 
			 * we try to fix markup issues inside the text that is contained in the XML */
			document = null;
			String newLogString = XmlNormalizer.normalizeXMLEmbeddedTextOnly(string);
			try {
				document = builder.parse(new InputSource(new StringReader(newLogString)));
				System.out.println("Fatal error recovered:");
				System.out.println("\tOriginal log entry: "+string);
				System.out.println("\tCleaned log entry: "+newLogString+"\n");
			} 
			catch(IOException ex1)  {
				System.err.println("Failed to parse the following log entry:");
				System.err.println(string);
				System.err.println("with IO exception ");
				throw new LogParseException(ex1);
			}
			catch (SAXException ex2) {
				System.err.println("Failed to parse the following log entry:");
				System.err.println(string);
				System.err.println("with parser exception ");
				throw new LogParseException(ex2);
			}
		}
		return new LogEntry(new LogEntryXML(document.getFirstChild()));
	}	
}

