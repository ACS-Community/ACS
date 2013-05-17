/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.cdbChecker;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.xml.sax.EntityResolver;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import com.sun.xml.internal.xsom.XSComplexType;
import com.sun.xml.internal.xsom.XSContentType;
import com.sun.xml.internal.xsom.XSElementDecl;
import com.sun.xml.internal.xsom.XSModelGroup;
import com.sun.xml.internal.xsom.XSParticle;
import com.sun.xml.internal.xsom.XSSchema;
import com.sun.xml.internal.xsom.XSSchemaSet;
import com.sun.xml.internal.xsom.XSTerm;
import com.sun.xml.internal.xsom.parser.XSOMParser;

/**
 * Checks for proper inheritance from CharacteristicComponent schema,
 * see http://jira.alma.cl/browse/COMP-3752.
 * <p>
 * For schema parsing we use the <code>com.sun.xml.internal.xsom</code> classes.
 * If Oracle removes them, we have to resort to some other library, e.g.
 * com.sun.org.apache.xerces.internal.xs.XSLoader, http://ws.apache.org/commons/XmlSchema/index.html,
 * http://www.eclipse.org/modeling/mdt/?project=xsd#xsd
 * <p>
 * Note that there is a bug in XSOM (checked up to jdk1.7.0_07) that prevents us from first parsing all XSD files
 * and then validating the baci standards. Thus we parse and validate each schema file separately,
 * which brings about unnecessary repeated parsing of imported schemas.
 * The problem is that XSOM does not skip parsing an XSD file when that XSD was already parsed as an imported schema before,
 * and the other way around. It does however skip an imported XSD that was imported already by a previous schema.
 * What's broken is the skipping-coordination between imported XSDs and directly parsed XSDs.
 * The error would be a SAXParseException about "...type... is already defined".
 * 
 * @author hsommer
 */
public class BaciSchemaChecker
{
	private final File xsdFile;
	private final Logger logger;
	private final XSSchemaSet schemaSet;
	private XSComplexType baciPropertyBaseType;
	private XSComplexType baciCharacteristicComponentType;
	
	public BaciSchemaChecker(File xsdFile, EntityResolver resolver, Logger logger) throws SAXException, MalformedURLException {
		this.xsdFile = xsdFile;
		this.logger = logger;
		XSOMParser parser = new XSOMParser();
		parser.setErrorHandler(new ParserErrorHandler());
		parser.setEntityResolver(resolver);

		// we use the URL format just so that in debug logs it is the same format
		// as the xsd system-id given to the schema resolver.
		URL url = xsdFile.toURI().toURL();
		logger.fine("About to parse " + url);
		parser.parse(url);

		schemaSet = parser.getResult();
		
		// use this to explore include/import relationships, if needed
//		Set<SchemaDocument> xsdDocs = parser.getDocuments();
//		String msg = "Total parsed XSDs: ";
//		for (SchemaDocument schemaDocument : xsdDocs) {
//			msg += schemaDocument.getSystemId() + ", ";
//		}
//		System.out.println(msg);

//		String msg = "Parsed schema file '" + xsdFile.getAbsolutePath() + "' using XSOMParser and got the following schema NSs: ";
//		if (schemaSet != null) {
//			for (XSSchema schema : schemaSet.getSchemas()) {
//				msg += schema.getTargetNamespace() + ", ";
//			}
//		}
//		logger.info(msg);
	}
	
	/**
	 * This method should be used only for unit testing.
	 */
	XSSchemaSet getSchemaSet() {
		return schemaSet;
	}

	public static class BaciPropertyLocator {
		public BaciPropertyLocator(String fileUri, String elementName, String propertyName, String propertyTypeName) {
			this.fileUri = fileUri;
			this.elementName = elementName;
			this.propertyName = propertyName;
			this.propertyTypeName = propertyTypeName;
		}
		public String fileUri;
		public String elementName;
		public String propertyName;
		public String propertyTypeName;
		public String toString() {
			return "Property " + elementName + "#" + propertyName + " (type=" + propertyTypeName + ") in " + fileUri;
		}
	}
	
	
	public List<BaciPropertyLocator> findBaciPropsOutsideCharacteristicComp() {
		
		List<BaciPropertyLocator> ret = new ArrayList<BaciPropertyLocator>();
		if (schemaSet == null) {
			return ret;
		}
		
		final String baciNsUri = "urn:schemas-cosylab-com:BACI:1.0";
		XSSchema baciSchema = schemaSet.getSchema(baciNsUri);
		
		if (baciSchema != null) {
			extractBaciTypes(baciSchema);
			for (XSSchema schema : schemaSet.getSchemas()) {
				logger.finer("Checking XSD file " + schema.getLocator().getSystemId());
				for (XSElementDecl elem : schema.getElementDecls().values()) {
					logger.finer("Checking XSD element " + elem.getName() + ", NS=" + elem.getTargetNamespace());
					if (elem.getType().isComplexType()) {
						XSComplexType elemType = elem.getType().asComplexType();
						XSParticle particle = elemType.getContentType().asParticle();
						checkElementsRecursively(elem, particle, ret);
					}
				}
			}
		}
		else {
			logger.finer("No baci types used in " + xsdFile.toURI());
		}
		return ret;
	}
	
	/** 
	 * Recursion over elements and complex types, inspired from
	 * http://stackoverflow.com/questions/10320814/how-to-get-max-depth-of-a-xsd-using-xsom-dom-jaxb?rq=1.
	 */
	private void checkElementsRecursively(XSElementDecl parentElem, XSParticle xsp, List<BaciPropertyLocator> badProperties) {
		if (xsp != null) {
			XSTerm term = xsp.getTerm();
			if (term.isElementDecl()) {
				String elementName = term.asElementDecl().getName();
				XSComplexType xscmp = (term.asElementDecl()).getType().asComplexType();
//				logger.fine("getElementsRecursively: parent=" + parentElem.getName() + "; elementDecl=" + elementName + "; complexType=" + xscmp);
				if (xscmp != null) {
					if (isBaciPropertyType(xscmp)) {
						if (isCharacteristicComponent(parentElem)) {
							logger.finer("OK: Baci property " + parentElem.getName() + "#" + elementName);
						}
						else {
							// the bad case: a baci property sits in a parent element that does not derive from CharacteristicComponent
							String file = parentElem.getLocator().getSystemId();
							String typeName = ( xscmp.isGlobal() ? xscmp.getName() : xscmp.getBaseType().getName() );
							BaciPropertyLocator propLoc = new BaciPropertyLocator(file, parentElem.getName(), elementName, typeName);
							badProperties.add(propLoc);
						}
					}
					else {
						XSContentType xscont = xscmp.getContentType();
						XSParticle particle = xscont.asParticle();
						checkElementsRecursively(term.asElementDecl(), particle, badProperties);
					}
				}
			} 
			else if (term.isModelGroup()) {
				XSModelGroup model = term.asModelGroup();
				XSParticle[] parr = model.getChildren();
				for (XSParticle partemp : parr) {
					checkElementsRecursively(parentElem, partemp, badProperties);
				}
			}
		}
	}
	
	/**
	 * Extract those types from BACI.xsd that will later be used in the validation.
	 * Must be called at least once before {@link #isBaciPropertyType(XSComplexType)} can be called.
	 */
	void extractBaciTypes(XSSchema baciSchema) {
		if (baciPropertyBaseType == null) {
			baciPropertyBaseType = baciSchema.getComplexType("Property");
		}
		if (baciCharacteristicComponentType == null) {
			baciCharacteristicComponentType = baciSchema.getComplexType("CharacteristicComponent");
		}
	}
	
	boolean isBaciPropertyType(XSComplexType type) {
		if (baciPropertyBaseType != null) {
			return ( type.isDerivedFrom(baciPropertyBaseType) );
		}
		else {
			throw new IllegalStateException("Must first find the 'Property' complex type (see field #baciPropertyBaseType)!");
		}
	}

	boolean isCharacteristicComponent(XSElementDecl parentElem) {
		if (baciCharacteristicComponentType != null) {
			XSComplexType xscmp = parentElem.getType().asComplexType();
			return ( xscmp.isDerivedFrom(baciCharacteristicComponentType) );
		}
		else {
			throw new IllegalStateException("Must first find the 'Property' complex type (see field #baciCharacteristicComponentType)!");
		}
	}

	
	private class ParserErrorHandler implements ErrorHandler {

		@Override
		public void warning(SAXParseException exception) {
			logger.log(Level.WARNING, "XSOM parse warning", exception);
		}

		@Override
		public void error(SAXParseException exception) {
			logger.log(Level.WARNING, "recoverable XSOM parse error: " + exception.toString());
		}

		@Override
		public void fatalError(SAXParseException exception) {
			logger.log(Level.SEVERE, "fatal XSOM parse error", exception);
		}
	}
}
