/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2005
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
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

package alma.acs.logging.config.converter;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.util.LinkedHashMap;
import java.util.Map;

import org.jdom.Comment;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.Namespace;
import org.jdom.input.SAXBuilder;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;
import org.xml.sax.InputSource;

/**
 * Converts an old CDB Container config XML file to the new logging config format introduced with ACS 6.
 * <p>
 * TODO-: validate output against Container.xsd
 *
 * @author hsommer
 * @deprecated  with ACS 7.0 because conversion of CDB config files for ACS 5 or lower should be over soon.
 *              To be removed with ACS 8.0
 */
public class ACS6ContainerLoggingConfigConverter {

	public static final String loggingNS = "urn:schemas-cosylab-com:LoggingConfig:1.0";
	public static final String containerNS = "urn:schemas-cosylab-com:Container:1.0";
	
	private final Map<String, String> logAttrNameMap;	
	private SAXBuilder parser;
	
	
	public ACS6ContainerLoggingConfigConverter() {
		logAttrNameMap = new LinkedHashMap<String, String>();
		logAttrNameMap.put("CentralizedLogger", "centralizedLogger");
		logAttrNameMap.put("MinCachePriority", "minLogLevel");
		logAttrNameMap.put("CacheSize", "dispatchPacketSize");
		logAttrNameMap.put("MaxCachePriority", "immediateDispatchLevel");
	}
	
	
	/**
	 * @param file
	 * @throws JDOMException
	 * @throws IOException
	 * @throws IllegalArgumentException if the file argument is not a Container config file.
	 */
	public void convertLoggingConfigFile(File containerConfigFile) throws Exception {
		
		boolean validate = false;
		Document doc = parseLoggingConfigFile(containerConfigFile, validate);
		
		Element rootElem = doc.getRootElement();
		if (!rootElem.getName().equals("Container") || !rootElem.getNamespaceURI().equals(containerNS)) {
			throw new IllegalArgumentException("File " + containerConfigFile.getAbsolutePath() + " does not have the expected 'Container' root elem.");
		}
		if (!rootElem.getNamespaceURI().equals(containerNS)) {
			throw new IllegalArgumentException("Container element in file " + containerConfigFile.getAbsolutePath() + " does not have the expected namespace " + containerNS);
		}
		
		// append new <LoggingConfig> element
		Element loggingElem = createLoggingElement(rootElem);
		rootElem.addContent(loggingElem);
		rootElem.addNamespaceDeclaration(Namespace.getNamespace("log", loggingNS));
		
		removeLoggingAttributes(rootElem);		
		
		File outFile = new File(getOutputFilename(containerConfigFile));
		outputLoggingConfigFile(doc, outFile);
	}
	
	static String getOutputFilename(File inputFile) {
		return inputFile.getAbsoluteFile() + ".forACS60";
	}
	
	void removeLoggingAttributes(Element rootElem) {
		for (String oldAttrName : logAttrNameMap.keySet()) {
			rootElem.removeAttribute(oldAttrName);
		}
	}

	
	Element createLoggingElement(Element containerElem) throws Exception {
		if (containerElem.getChild("LoggingConfig", Namespace.getNamespace(containerNS)) != null) {
			throw new Exception("<LoggingConfig/> element already exists!");
		}
		Element loggingElem = new Element("LoggingConfig", containerNS);
		for (String oldAttrName : logAttrNameMap.keySet()) {
			String newAttrName = logAttrNameMap.get(oldAttrName);
			if (containerElem.getAttributeValue(oldAttrName) != null) {
				loggingElem.setAttribute(newAttrName, containerElem.getAttributeValue(oldAttrName));
			}
		}
		
		// template for new attributes and child element
		String comment = " Some new additional attributes can be used to tweak logging, although as of ACS 6.0 some are only effective for Java logging:\n" + 
			"         minLogLevelLocal, maxLogQueueSize, flushPeriodSeconds. \n" +
			"         With a future ACS release it will be possible to configure individual loggers by adding optional child elements, as in \n" +
			"            <log:_ Name=\"frodoContainer\" minLogLevel=\"4\" minLogLevelLocal=\"2\" />  \n" + 
			"            <log:_ Name=\"MyMuteComponent\" minLogLevel=\"6\" minLogLevelLocal=\"6\" />  \n" + 
			"    ";
		loggingElem.addContent(new Comment(comment));
		
		return loggingElem;
	}
	
	
	void outputLoggingConfigFile(Document doc, File outFile) throws IOException {
		FileOutputStream outstream = new FileOutputStream(outFile);
		outputLoggingConfig(doc, outstream);
	}
	
	void outputLoggingConfig(Document doc, OutputStream outstream) throws IOException {
		Format format = Format.getPrettyFormat();
		//format.setExpandEmptyElements(true);
		
		XMLOutputter out = new XMLOutputter(format);
		
		// for the encoding problem, see http://www.jdom.org/docs/apidocs/org/jdom/output/XMLOutputter.html
		OutputStreamWriter writer = new OutputStreamWriter(outstream, format.getEncoding()); 
		
		out.output(doc, writer);
	}
	
	public Document parseLoggingConfigFile(File file, boolean validate) throws JDOMException, IOException {
		if (!file.exists() || !file.isFile()) {
			throw new IllegalArgumentException("Logging config file " + file.getAbsolutePath() + " does not exist.");
		}
		
		if (parser == null || parser.getValidation() != validate) {
			parser = createParser(validate);
		}

		Reader xmlReader = new BufferedReader(new FileReader(file));
		InputSource insrc = new InputSource(xmlReader); 
		Document doc = parser.build(insrc);
		
		return doc;
	}

	
	/**
	 * Schema validation is currently not supported. 
	 */
	public SAXBuilder createParser(boolean validate) throws JDOMException {
		// see http://www.jdom.org/docs/faq.html#a0360
		SAXBuilder builder = new SAXBuilder("org.apache.xerces.parsers.SAXParser", validate);
		builder.setFeature("http://apache.org/xml/features/validation/schema", validate);
		if (validate) {
			throw new JDOMException("schema mapping not configured for validating parser");
			// set namespace-schema pairs
			// builder.setProperty("http://apache.org/xml/properties/schema/external-schemaLocation",   
			//                     "http://www.w3.org/2001/12/soap-envelope soap-envelope.xsd" + " " +   
			//                     "http://kevinj.develop.com/weblog/weblog.xsd weblog.xsd");			
		} 
		return builder;
	}
	
	
	public static void main(String[] args) {
		if (args.length != 1) {
			System.out.println("usage: acsStartJava -endorsed " + ACS6ContainerLoggingConfigConverter.class.getName() + " filename");
			return;
		}
		File inputFile = null;
		try {
			inputFile = new File(args[0]);
			ACS6ContainerLoggingConfigConverter converter = new ACS6ContainerLoggingConfigConverter();
			converter.convertLoggingConfigFile(inputFile);
			System.out.println("Converted '" + inputFile.getName() + "' to new logging config format, see '" + 
					ACS6ContainerLoggingConfigConverter.getOutputFilename(inputFile) + "'.");
		} catch (Throwable thr) {
			System.err.println("Failed to convert " + inputFile);
			thr.printStackTrace();
		}
	}

}
