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
package alma.tools.entitybuilder;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;

import org.exolab.castor.xml.MarshalException;
import org.exolab.castor.xml.Marshaller;
import org.exolab.castor.xml.Unmarshaller;
import org.exolab.castor.xml.ValidationException;

import alma.xmljbind.test.obsproject.ObsProject;
import alma.xmljbind.test.obsproposal.ObsProposal;

/**
 * Test helper class that manages sample XML files, the unmarshaling to Castor classes, and marshalling back to files.
 * 
 * @author hsommer created Mar 1, 2005 12:02:37 PM
 */
public class XmlInOut
{

	private File module_dir;

	// todo: more flexible
	private static final String pathToXmlSamples = "test/data/xml_instances_fake/";

	public XmlInOut() {
		String baseDirPath = System.getProperty("module.dir");
		if (baseDirPath != null) {
			module_dir = new File(baseDirPath);
		} else {
			module_dir = new File(System.getProperty("user.dir"));
		}
	}

	public ObsProposal unmarshalObsProposal(String xmlFileName) 
			throws FileNotFoundException, MarshalException, ValidationException {
		File xmlFile = new File(module_dir, pathToXmlSamples + xmlFileName);
		Reader xmlReader = new FileReader(xmlFile);
		Unmarshaller unm = new Unmarshaller(ObsProposal.class);
		unm.setValidation(false);
		ObsProposal oprop = (ObsProposal) unm.unmarshal(xmlReader);
		return oprop;
	}

	public ObsProject unmarshalObsProject(String xmlFileName) 
			throws FileNotFoundException, MarshalException, ValidationException {
		File xmlFile = new File(module_dir, pathToXmlSamples + xmlFileName);
		Reader xmlReader = new FileReader(xmlFile);
		ObsProject oproj = ObsProject.unmarshalObsProject(xmlReader);
		return oproj;
	}

	public String marshalObsProposalToFile(ObsProposal prop, String originalXmlFilename) 
			throws MarshalException, ValidationException, IOException {
		String filename = getRemarshaledFilename(originalXmlFilename);
		File xmlFile = new File(module_dir, pathToXmlSamples + filename);

		Marshaller m = new Marshaller(new FileWriter(xmlFile));
		m.setNamespaceMapping("tprj", "AlmaTest/ObsProject");
		m.marshal(prop);

		return filename;
	}

	public String marshalObsProjectToFile(ObsProject proj, String originalXmlFilename) 
			throws MarshalException, ValidationException, IOException {
		String filename = getRemarshaledFilename(originalXmlFilename);
		File xmlFile = new File(module_dir, pathToXmlSamples + filename);

		Marshaller m = new Marshaller(new FileWriter(xmlFile));
		// m.setNamespaceMapping("tprj", "AlmaTest/ObsProject");
		m.marshal(proj);

		return filename;
	}

	private String getRemarshaledFilename(String filename) {
		return filename.substring(0, filename.lastIndexOf(".xml")) + "_remarshalled.xml";
	}
}
