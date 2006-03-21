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

/**
 * ACSLogParser is used for parsing an xml string (starting with like <Info>,<Debug> and so on) 
 * to produce a valid LogEntryXML. It gets the string  
 * from inside the ACSStructuredPushConsumer class where the logs get accumulated from the 
 * CORBA Logging Service. 
 * Creation date: (11/8/2001 6:22:53 PM)
 * @author: 
 */
public class ACSLogParser {
//	private DocumentBuilderFactory factory = null;
	private DocumentBuilder builder = null;
/**
 * ACSLogParser constructor comment.
 */
public ACSLogParser() throws ParserConfigurationException {
	super();
	initialize();
}

private void initialize() throws ParserConfigurationException {
	DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
//	try {
		builder = factory.newDocumentBuilder();
/*	} catch (Throwable pce) {
		
		System.out.println("Exception in ACSLogParser::parse(): " + pce);
		return;
	}*/
}
/**
 * Used for testing purposes only.
 * @param args an array of command-line arguments
 */
public static void main(java.lang.String[] args) {
	// Insert code to start the application here.
	ACSLogParser parser = null;
	try {
		parser = new ACSLogParser();
	} catch (Exception e) {};
	LogEntryXML log = null;
	try {
		// log = parser.parse("<Info TimeStamp=\"2001-11-07T09:24:11.096\" Routine=\"msaci::ContainerImpl::init\" Host=\"disna\" Process=\"maciManager\" Thread=\"main\" Context=\"\"><Data Name=\"StupidData\">All your base are belong to us.</Data>Connected to the Centralized Logsger.</Info>");
		log = parser.parse("<Log><Header Name=\"NameForXmlDocument\" Type=\"LOGFILE\" /><Info TimeStamp=\"2002-11-07T15:13:20.641\">Error</Info></Log>");
		// log = parser.parse("<Info TimeStamp=\"2002-07-01T13:15:42.455\">	Error = 0.000000 < -1.000000 < 1000.000000</Info>");
	} catch (Exception e) {
		System.out.println("Exception in ACSLogParser::main(): " + e);
	}
	System.out.println(log);
}
/**
 * Parses the xmlLog. This method must be synchronized to ensure that the
 * parses parses only one log at a time.
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
		/* There was an exception parsing the log but before gaving up 
		 * I can try to fix some errors...
		 */
		document = null;
		String newLogString = cleanLogString(string);
		try {
			document = builder.parse(new InputSource(new StringReader(newLogString)));
			System.out.println("\nWarning: original log entry changed!");
			System.out.println("Original log entry: "+string);
			System.out.println("Cleaned log entry: "+newLogString+"\n");
		}	catch (IOException ioe) {
			// cannot get here	
			System.err.println("Exception parsing "+ioe.getMessage());
			ioe.printStackTrace(System.err);
			throw ioe;
		}
	}
	return new LogEntryXML(document.getFirstChild());
}

/**
 * Try to clean the log string by replacing chars like > with &gt; 
 * and so on 
 * 
 * @param logStr The log string
 * @return A new possybly cleaned string representing the log
 * 
 */
private String cleanLogString(String logStr) {
	System.out.println("cleaning");
	StringBuffer sb = new StringBuffer();
	// It is true if the char we are reading is into a queote "..."
	boolean intoQuote = false;
	// It is true if the char we are reading is into a CDATA section
	boolean intoCDATA =false;
	for (int t=0; t<logStr.length(); t++) {
		char c = logStr.charAt(t);
		if (c=='\"') {
			intoQuote=!intoQuote;
			sb.append(c);
			continue;
		}
		
		if (c=='>') {
			if (intoQuote) {
				sb.append("&gt;");
				continue;
			} else {
				sb.append(c);
				continue;
			}
		}
		
		if (c=='<') {
			if (intoQuote) {
				sb.append("&lt;");
				continue;
			} else {
				sb.append(c);
				continue;
			}
		}
		
		if (c=='\'') {
			if (intoQuote) {
				sb.append("&apos;");
				continue;
			} else {
				sb.append(c);
				continue;
			}
		}
		
		if (c=='&') {
			if (intoQuote) {
				sb.append("&amp;");
				continue;
			} else {
				sb.append(c);
				continue;
			}
		}
		sb.append(c);
	}
	return sb.toString();
}


}

