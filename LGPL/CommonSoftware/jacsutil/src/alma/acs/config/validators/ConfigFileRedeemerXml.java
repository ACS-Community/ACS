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

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.helpers.DefaultHandler;

/**
 * Class that can recognize XML files that are known to be not config files.
 *  
 * @author hsommer
 */
public class ConfigFileRedeemerXml extends ConfigFileRedeemer {

	private SAXParser parser;
	private DefaultHandler saxHandler;
	
	/**
	 * 
	 */
	public ConfigFileRedeemerXml() throws Exception {
		parser = SAXParserFactory.newInstance().newSAXParser();
		saxHandler = new DefaultHandler() {
			public void startElement (String uri, String localName, String qName, Attributes atts) {
				// todo: try to recognize the element.
				System.out.println("XML element: " + qName);
			}
		};
	}
	
	public String[] getFileEndings() {
		return new String[] {".xml"};
	}

	
	public boolean _isNotAConfigFile(File file) {
		try {
			parser.parse(file, saxHandler);
			return true;
		} catch (Exception e) {
			e.printStackTrace();
			System.err.println("xml file '" + file + "' failed to be parsed and will be treated as suspect.");
			return false;
		}
	}


}
