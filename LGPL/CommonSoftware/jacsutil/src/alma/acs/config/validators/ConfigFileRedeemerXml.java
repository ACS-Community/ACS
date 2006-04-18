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

package alma.acs.config.validators;

import java.io.File;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.SAXParser;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.NodeList;
import org.w3c.dom.Element;
import org.xml.sax.helpers.DefaultHandler;


/**
 * Class that can recognize XML files that are known to be not config files.
 *  
 * @author hsommer
 */
public class ConfigFileRedeemerXml extends ConfigFileRedeemer {

	protected SAXParser parser;
	protected DefaultHandler saxHandler;
//	protected XmlNamespaceContextContainer nsContext;
	protected List<XPathMatchSetup> xpathMatchSetupList;
	
	/**
	 * 
	 */
	public ConfigFileRedeemerXml() throws Exception {
		xpathMatchSetupList = new ArrayList<XPathMatchSetup>();
		configure();
		
//		nsContext = new XmlNamespaceContextContainer();
		
//		SAXParserFactory saxfac = SAXParserFactory.newInstance();
//		saxfac.setNamespaceAware(true);
//		parser = saxfac.newSAXParser();
//		
//		saxHandler = new DefaultHandler() {
//			public void startElement(String uri, String localName, String qName, Attributes atts) {
//				startSaxElement(uri, localName, qName, atts);
//			}
//		};
	}
	
	/**
	 * Configures information about known files which should not be accused of being config files.
	 */
	protected void configure() {
//		addXPathMatch(new XPathMatchSetup("/bla:Container", "bla", "urn:schemas-cosylab-com:Container:1.0", XPathConstants.NODE));		
		addXPathMatch(new XPathMatchSetup("/EntitybuilderSettings/EntitySchema", null, null, XPathConstants.NODESET));
		
		addXPathMatch(new XPathMatchSetup("/Type/@type", null, null, XPathConstants.NUMBER));
		addXPathMatch(new XPathMatchSetup("/bla:Type/@type", "bla", "Alma/ACSError", XPathConstants.NUMBER)); // error schema used to have a target namespace
		
		addXPathMatch(new XPathMatchSetup("/bla:AcsCommandCenterProject", "bla", "Alma/Acs/AcsCommandCenterProject", XPathConstants.NODE));		
	}
	
	public String[] getFileEndings() {
		return new String[] {".xml"};
	}

	
	public boolean _isNotAConfigFile(File xmlFile) {
		try {
			// parse the XML file into a DOM
			DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
			factory.setNamespaceAware(true);			
			DocumentBuilder builder = factory.newDocumentBuilder();
			Element rootElement = builder.parse(xmlFile).getDocumentElement();
			
			// create an XPath engine
			XPath xpath = XPathFactory.newInstance().newXPath();
			
			// for each XPath expression configured in method #addXPathMatch, 
			// we try if it matches anything in our DOM
			for (Iterator<XPathMatchSetup> iter = xpathMatchSetupList.iterator(); iter.hasNext();) {
				XPathMatchSetup xpathSetup = iter.next();				
				XmlNamespaceContextContainer nsContext = new XmlNamespaceContextContainer();
				nsContext.addNamespace(xpathSetup.getPrefix(), xpathSetup.getNamespace());
				xpath.setNamespaceContext(nsContext);
				Object xpathResult = xpath.evaluate(xpathSetup.getExpression(), rootElement, xpathSetup.getReturnType());
				if (xpathResult != null) {
					// NUMBER must not be NaN
					if (xpathSetup.getReturnType().equals(XPathConstants.NUMBER) && ((Double)xpathResult).isNaN()) {
//						System.out.println("result: Number " + xpathResult.getClass().getName() + xpathResult);
						continue;
					}
					// NODESET must not be empty
					else if (xpathSetup.getReturnType().equals(XPathConstants.NODESET) && ((NodeList)xpathResult).getLength() == 0) {
						continue;
					}
					System.out.println("XML file '" + xmlFile + "' is not a config file. " + 
							"The query '" + xpathSetup.getExpression() + "' found a matching " + xpathSetup.getReturnType().getLocalPart());
					return true;
				}
			}
			
			return false;
			
//			parser.parse(xmlFile, saxHandler);
//			return true;
		} catch (Exception e) {
			e.printStackTrace();
			System.err.println("xml file '" + xmlFile + "' failed to be parsed and will be treated as suspect.");
			return false;
		}
	}

//	protected void startSaxElement(String uri, String localName, String qName, Attributes atts) {
//		// todo: try to recognize the element.
//		System.out.println("XML element: " + qName);
//	}
	
	
	public static class XPathMatchSetup {
		private String expression;
		private String prefix;
		private String namespace;
		private QName returnType;
		public XPathMatchSetup(String expression, String prefix, String namespace, QName returnType) {
			this.expression = expression;
			this.namespace = namespace;
			this.prefix = prefix;
			this.returnType = returnType;
		}
		String getExpression() {
			return expression;
		}
		String getPrefix() {
			return prefix;
		}
		String getNamespace() {
			return namespace;
		}		
		QName getReturnType() {
			return returnType;
		}		
	}
	
	
	public void addXPathMatch(XPathMatchSetup xpathMatchSetup) {
		this.xpathMatchSetupList.add(xpathMatchSetup);
	}
}
