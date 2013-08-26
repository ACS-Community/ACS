/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
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
package alma.acs.eclipse.utils.pluginbuilder;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

/**
 * Write the <code>plugin.xml</code> file.
 * <P>
 * The generated plugin has not extensions not extension points
 * so {@link PluginXmlWriter} generates an empty plugin.xml file.
 * 
 * @author acaproni
 *
 */
public class PluginXmlWriter {
	
	/**
	 * The XML header
	 */
	public static final String xmlHeader="<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
	
	/**
	 * The eclipse TAG
	 */
	public static final String eclipseTAG="<?eclipse version=\"3.2\"?>\n";
	
	/**
	 * The opening plugin TAG
	 */
	public static final String openingPluginTAG="<plugin>\n";
	
	/**
	 * The opening plugin TAG
	 */
	public static final String closingPluginTAG="</plugin>\n\n";
	
	/**
	 * The file for writing
	 */
	private final FileWriter outFile;
	
	/**
	 * The name of the XML definition file
	 */
	public static final String pluginXMLFileName="plugin.xml";

	/**
	 * Constructor.
	 * 
	 * @param outFile The writable file for writing plugin.xml info into
	 * @throws IOException In case of error creating the file for writing
	 */
	public PluginXmlWriter(File outFileFolder) throws IOException {
		if (outFileFolder==null || !outFileFolder.canWrite()) {
			throw new IllegalArgumentException("The folder cant be null and must be writable");
		}
		this.outFile=new FileWriter(outFileFolder+File.separator+pluginXMLFileName,false);
	}
	
	/**
	 * Write the XML content into the file
	 * 
	 * @throws IOException In case of errors writing into the file
	 */
	public void write() throws IOException {
		outFile.write(xmlHeader);
		outFile.write(eclipseTAG);
		outFile.write(openingPluginTAG);
		outFile.write(closingPluginTAG);
		outFile.flush();
		outFile.close();
	}
}
