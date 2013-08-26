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
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;


/**
 * Class that can recognize XML files that are known to be not config files.
 *  
 * @author hsommer
 */
public class ConfigFileRedeemerXml extends ConfigFileRedeemer {

	protected List<XPathMatchSetup> xpathMatchSetupList;
	
	public static final String XSD_NS = "http://www.w3.org/2001/XMLSchema";
	
	/**
	 * 
	 */
	public ConfigFileRedeemerXml(Logger logger) throws Exception {
		super(logger);
		xpathMatchSetupList = new ArrayList<XPathMatchSetup>();
		configure();
	}

	
	/**
	 * Configures information about known files which should not be accused of being config files.
	 */
	protected void configure() {
		// Castor config files
		addXPathMatch(new XPathMatchSetup("/EntitybuilderSettings/EntitySchema", null, null, XPathConstants.NODESET));
		// ACS error system type/code definition files
		addXPathMatch(new XPathMatchSetup("/Type/@type", null, null, XPathConstants.NUMBER));
		addXPathMatch(new XPathMatchSetup("/bla:Type/@type", "bla", "Alma/ACSError", XPathConstants.NUMBER)); // error schema used to have a target namespace

		// UML-XMI files
		addXPathMatch(new XPathMatchSetup("/XMI/@xmi.version", null, null, XPathConstants.NUMBER));
		// APDM entities
		addXPathMatch(new XPathMatchSetup("/oprp:ObsProposal", "oprp", "Alma/ObsPrep/ObsProposal", XPathConstants.NODE)); 
		addXPathMatch(new XPathMatchSetup("/oprj:ObsProject", "oprj", "Alma/ObsPrep/ObsProject", XPathConstants.NODE)); 
		addXPathMatch(new XPathMatchSetup("/sbl:SchedBlock", "sbl", "Alma/ObsPrep/SchedBlock", XPathConstants.NODE)); 		
		addXPathMatch(new XPathMatchSetup("/xsd:schema[@targetNamespace='Alma/ObsPrep/ObsProposal']", "xsd", XSD_NS, XPathConstants.NODE));
		addXPathMatch(new XPathMatchSetup("/xsd:schema[@targetNamespace='Alma/ObsPrep/ObsProject']", "xsd", XSD_NS, XPathConstants.NODE));
		addXPathMatch(new XPathMatchSetup("/xsd:schema[@targetNamespace='Alma/ObsPrep/ObsReview']", "xsd", XSD_NS, XPathConstants.NODE));
		addXPathMatch(new XPathMatchSetup("/xsd:schema[@targetNamespace='Alma/ObsPrep/SchedBlock']", "xsd", XSD_NS, XPathConstants.NODE));
		addXPathMatch(new XPathMatchSetup("/xsd:schema[@targetNamespace='Alma/ObsPrep/ProjectStatus']", "xsd", XSD_NS, XPathConstants.NODE));
		addXPathMatch(new XPathMatchSetup("/xsd:schema[@targetNamespace='Alma/ValueTypes']", "xsd", XSD_NS, XPathConstants.NODE));

		// OAW mappings
		addXPathMatch(new XPathMatchSetup("/MetaMap", null, null, XPathConstants.NODE));
		addXPathMatch(new XPathMatchSetup("/MetaModel", null, null, XPathConstants.NODE));
		
		addXPathMatch(new XPathMatchSetup("/bla:AcsCommandCenterProject", "bla", "Alma/Acs/AcsCommandCenterProject", XPathConstants.NODE));		
		addXPathMatch(new XPathMatchSetup("/bla:AcsCommandCenterTools", "bla", "Alma/Acs/AcsCommandCenterTools", XPathConstants.NODE));		
		addXPathMatch(new XPathMatchSetup("/bla:cosydoc", "bla", "urn:schemas-cosylab-com:Document", XPathConstants.NODE));
		
		// known ACS schema files
		addXPathMatch(new XPathMatchSetup("/xsd:schema/xsd:simpleType[@name='ErrorType']", "xsd", XSD_NS, XPathConstants.NODE));
		addXPathMatch(new XPathMatchSetup("/xsd:schema/xsd:element[@name='AcsCommandCenterProject']", "xsd", XSD_NS, XPathConstants.NODE));
		addXPathMatch(new XPathMatchSetup("/xsd:schema/xsd:element[@name='AcsCommandCenterTools']", "xsd", XSD_NS, XPathConstants.NODE));
		addXPathMatch(new XPathMatchSetup("/xsd:schema/xsd:complexType[@name='EventChannel']", "xsd", XSD_NS, XPathConstants.NODE));
		addXPathMatch(new XPathMatchSetup("/xsd:schema/xsd:element[@name='SimulatedComponent']", "xsd", XSD_NS, XPathConstants.NODE));
		addXPathMatch(new XPathMatchSetup("/xsd:schema/xsd:complexType[@name='EntityT']", "xsd", XSD_NS, XPathConstants.NODE));
		addXPathMatch(new XPathMatchSetup("/xsd:schema/xsd:element[@name='IdentifierRange']", "xsd", XSD_NS, XPathConstants.NODE));
		addXPathMatch(new XPathMatchSetup("/xsd:schema/xsd:element[@name='ComponentHelperInfo']", "xsd", XSD_NS, XPathConstants.NODE));
		addXPathMatch(new XPathMatchSetup("/xsd:schema/xsd:element[@name='EntitybuilderSettings']", "xsd", XSD_NS, XPathConstants.NODE));
		addXPathMatch(new XPathMatchSetup("/xsd:schema/xsd:complexType[@name='ComponentInfo']", "xsd", XSD_NS, XPathConstants.NODE));
		addXPathMatch(new XPathMatchSetup("/xsd:schema/xsd:complexType[@name='Container']", "xsd", XSD_NS, XPathConstants.NODE));
		addXPathMatch(new XPathMatchSetup("/xsd:schema/xsd:complexType[@name='Manager']", "xsd", XSD_NS, XPathConstants.NODE));
		addXPathMatch(new XPathMatchSetup("/xsd:schema/xsd:element[@name='Log']", "xsd", XSD_NS, XPathConstants.NODE));
		addXPathMatch(new XPathMatchSetup("/xsd:schema/xsd:element[@name='paramset']", "xsd", XSD_NS, XPathConstants.NODE));
		addXPathMatch(new XPathMatchSetup("/xsd:schema/xsd:element[@name='pset']", "xsd", XSD_NS, XPathConstants.NODE));
		
		// Ant build files (incl. Eclipse external build files)
		addXPathMatch(new XPathMatchSetup("/project/@basedir", null, null, XPathConstants.STRING));
		addXPathMatch(new XPathMatchSetup("/project/target/javadoc", null, null, XPathConstants.NODESET));
				
		// addXPathMatch(new XPathMatchSetup("/bla:Container", "bla", "urn:schemas-cosylab-com:Container:1.0", XPathConstants.NODE));		
		
	}
	
	public String[] getFileEndings() {
		return new String[] {".xml", ".xsd", ".xslt"};
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
					// STRING must not be empty
					else if (xpathSetup.getReturnType().equals(XPathConstants.STRING) && ((String)xpathResult).trim().length() == 0) {
						continue;
					}
					else if (xpathSetup.getReturnType().equals(XPathConstants.NODE)) {
//						Node node = (Node) xpathResult;
//						System.out.println("*** got a node: " + node.toString());
					}
					System.out.println("XML file '" + xmlFile + "' is not a config file. " + 
							"The query '" + xpathSetup.getExpression() + "' found a matching " + xpathSetup.getReturnType().getLocalPart());
					return true;
				}
			}
			
			return false;
			
		}
		catch (Exception e) {
			logger.log(Level.WARNING, "xml file '" + xmlFile + "' failed to be parsed and will be treated as suspect.", e);
			return false;
		}
	}

	
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
