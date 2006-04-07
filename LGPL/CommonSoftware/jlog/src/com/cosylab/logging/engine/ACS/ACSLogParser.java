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
package com.cosylab.logging.engine.ACS;

import java.io.IOException;
import java.io.StringReader;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.DOMException;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.cosylab.logging.engine.log.LogEntryXML;

import alma.acs.util.XmlNormalizer;

/**
 * ACSLogParser is used for parsing an xml string (representing an XML element such as &lt;Info&gt;,&lt;Debug&gt; and so on) 
 * to produce a valid LogEntryXML.
 * <p> 
 * It will typically get the string from the ACSStructuredPushConsumer class where the logs get delivered 
 * from the CORBA Logging Service. 
 */
public class ACSLogParser {
//	private DocumentBuilderFactory factory = null;
	private DocumentBuilder builder = null;
	
	
	/**
	 * ACSLogParser constructor comment.
	 */
	public ACSLogParser() throws ParserConfigurationException {
		initialize();
	}
	
	private void initialize() throws ParserConfigurationException {
		DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();

		builder = factory.newDocumentBuilder();
	}

	
	/**
	 * Parses the xmlLog. This method must be synchronized to ensure that the
	 * parser parses only one log at a time.
	 */
	public synchronized LogEntryXML parse(String string) 
	throws SAXException, DOMException, IOException {
		Document document = null;
		
		try {
			document = builder.parse(new InputSource(new StringReader(string)));
		} catch (IOException ioe) {
			// cannot get here	
			System.err.println("Exception parsing "+ioe.getMessage());
			ioe.printStackTrace(System.err);
			throw ioe;
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
			} catch (SAXException ex2) {
				System.err.println("Failed to parse the following log entry:");
				System.err.println(string);
				System.err.println("with parser exception ");
				ex2.printStackTrace();
				throw ex2;
			}
		}
		return new LogEntryXML(document.getFirstChild());
	}	
}

