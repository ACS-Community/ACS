/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
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
package alma.tools.idlgen.comphelpgen;

import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.jacorb.idl.Interface;

import alma.acs.tools.comphelpergen.CompHelperGenerator;
import alma.acs.tools.comphelpergen.generated.ComponentHelperInfo;
import alma.acs.tools.comphelpergen.generated.ComponentInterface;



/**
 * Gets the information together that the generator for component helper classes 
 * (<code>alma.acs.tools.comphelpergen.CompHelperGenerator</code>) needs, and runs it.
 * 
 * The communication is through XML to decouple this module from the <code>comphelpgen</code> 
 * module. The XML complies with the schema HelperInfo.xsd from the module comphelpgen 
 * which is listed here for convenience:
 * <pre>
 * &lt;?xml version="1.0" encoding="ISO-8859-1"?&gt;
 * &lt;xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema" elementFormDefault="qualified" attributeFormDefault="unqualified"&gt;
 * 	&lt;xs:element name="ComponentHelperInfo"&gt;
 * 		&lt;xs:complexType&gt;
 * 			&lt;xs:sequence&gt;
 * 				&lt;xs:element ref="ComponentInterface" minOccurs="0" maxOccurs="unbounded"/&gt;
 * 			&lt;/xs:sequence&gt;
 * 			&lt;xs:attribute name="outputRootDirectory" type="xs:string" use="required"/&gt;
 * 		&lt;/xs:complexType&gt;
 * 	&lt;/xs:element&gt;
 * 	&lt;xs:element name="ComponentInterface"&gt;
 * 		&lt;xs:complexType&gt;
 * 			&lt;xs:attribute name="idlPackage" type="xs:string" use="required"/&gt;
 * 			&lt;xs:attribute name="componentClassName" type="xs:string" use="required"/&gt;
 * 			&lt;xs:attribute name="internalInterface" type="xs:string" use="optional"/&gt;			
 * 		&lt;/xs:complexType&gt;
 * 	&lt;/xs:element&gt;
 * &lt;/xs:schema&gt;
 * </pre> 
 * In the implementation, schema conformance is guaranteed by using identical binding classes on either
 * side of the XML communication.
 * 
 * @author hsommer Jan 16, 2003 5:47:15 PM
 */
public class ComponentHelperGeneratorProxy
{
	/**
	 * key = interface name , value = interface node
	 */
	private Map<String, Interface> m_interfaceMap = new HashMap<String, Interface>();
	
	private boolean m_verbose;
	private ComponentHelperInfo m_compHelpInfo;
	
	
	/**
	 * Constructor for ComponentHelperGeneratorProxy.
	 * 
	 * @param outputRootDir  the directory under which the component helper classes will be put.
	 * @param verbose  if true, some information will be dumped to System.out, including the XML.
	 */
	public ComponentHelperGeneratorProxy(String outputRootDir, boolean verbose)
	{
		m_verbose = verbose;
		m_compHelpInfo = new ComponentHelperInfo();
		m_compHelpInfo.setOutputRootDirectory(outputRootDir);
	}


	/**
	 * Sets the ACS component interfaces from the original parse tree 
	 * (those from the main IDL file, not from included IDL).
	 * <p>
	 * This method must be called before these interfaces get renamed in the parse tree, 
	 * because here we must store the original names for later.
	 * The interface nodes must be "live" so that later we also see the changed names ("xxxJ"). 
	 */
	public void setOriginalParseTree(Set<Interface> componentInterfaces) {
		
		for (Interface interfce : componentInterfaces) {
			m_interfaceMap.put(interfce.name(), interfce);
		}
		
		if (m_verbose) {
			System.out.println("done ComponentHelperGeneratorProxy#setOriginalParseTree.");
		}
	}


	/**
	 * Creates the configuration XML and sends it to an
	 * <code>alma.acs.tools.comphelpergen.CompHelperGenerator</code> for code generation.
	 */
	public void generateComponentHelperCode()
	{
		for (String interfaceName : m_interfaceMap.keySet()) {

			Interface idlInterface = m_interfaceMap.get(interfaceName);
			
			String idlPackageName = idlInterface.pack_name;
			
			// the name might have changed between invocations of setOriginalParseTree
			// and generateComponentHelperCode; one is the CORBA, the other 
			// the XML-binding interface, or "inner" interface
			String innerInterfaceName = idlInterface.name();
			
			if (m_verbose) {
				System.out.println("Corba-IF=" + interfaceName + 
									"  innerIF=" + innerInterfaceName +
									"  package=" + idlPackageName );
			}

			// -- CASTOR -- 
			
			ComponentInterface compIF = new ComponentInterface();
			m_compHelpInfo.addComponentInterface(compIF);
			String almostIdlInterfaceName = idlInterface.id(); // may be 'polluted' with "J" name
			String idlInterfaceName = almostIdlInterfaceName.substring(0,almostIdlInterfaceName.lastIndexOf('/')+1) + interfaceName + ":1.0";
			compIF.setCorbaRepositoryId(idlInterfaceName);
			compIF.setIdlPackage(idlPackageName);
			compIF.setComponentClassName(interfaceName);
			if (!innerInterfaceName.equals(interfaceName))
			{
				compIF.setInternalInterface(innerInterfaceName);
			}
			
		}

		StringWriter xmlStringWriter = new StringWriter();
		try {
			m_compHelpInfo.marshal(xmlStringWriter);
		}
		catch (Exception e) {
			System.err.println("failed to generate configuration file (xml) for component helper generator:");
			e.printStackTrace(System.err);
			System.exit(1);
		}
		String xmlString = xmlStringWriter.toString();
		
		if (m_verbose) {
			System.out.println("\n\nWill invoke component helper generator (ACS module comphelpgen).");
			System.out.println("************ component helper xml ********");
			System.out.println(xmlString);
		}
		
		
		// call helper generator
				
		CompHelperGenerator compHelpGen = new CompHelperGenerator(m_verbose);
		compHelpGen.generate(xmlString);
	}
	
}

