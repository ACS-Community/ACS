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
package alma.acs.tools.comphelpergen;

import java.io.IOException;
import java.io.StringReader;
import java.io.Reader;

import org.exolab.castor.xml.MarshalException;
import org.exolab.castor.xml.ValidationException;

import alma.acs.tools.comphelpergen.generated.ComponentHelperInfo;
import alma.acs.tools.comphelpergen.generated.ComponentInterface;

/**
 * @author rgeorgie
 *
 * Main class of a tool that generates component "helper" classes 
 * given an xml string as an input. If a required attribute is missing, 
 * there is no call for a class to be generated.
 */

public class CompHelperGenerator
{
	private static ComponentInterface[] compInterfaces;
	private static String outputRootDir;
	private CompHelperClass compHelper;
	private ComponentHelperInfo compHelperInfo;
	private IOSpecification ioSpecification;
	private int xmlFileNum = 1;

	private boolean m_verbose = false;


	public CompHelperGenerator()
	{
		this(false);
	}
	
	/**
	 * @see java.lang.Object#Object()
	 * Constructor creates instances of the classes needed 
	 * for the generation of a helper class.
	 */
	public CompHelperGenerator(boolean verbose)
	{
		m_verbose = verbose;
		compHelper = new CompHelperClass();
		ioSpecification = new IOSpecification();
	}


	/**
	 * Method getFilePar. Gets information about COBs from xml input string.
	 * @param contents
	 * @return ComponentHelperInfo
	 */
	protected ComponentHelperInfo getFilePar(String contents)
	{
		ComponentHelperInfo compHelpInfo = null;
		if (m_verbose)
		{
			System.out.println(contents);
		}
		Reader xmlReader = null;
		try
		{
			xmlReader = new StringReader(contents);

			if (xmlReader != null)
			{
				compHelpInfo = ComponentHelperInfo.unmarshalComponentHelperInfo(xmlReader);
			}
		}

		catch (ValidationException e)
		{
			e.printStackTrace();
		}

		catch (MarshalException e)
		{
			e.printStackTrace();
		}
		finally
		{
			if (xmlReader != null)
				try
				{
					xmlReader.close();
				}
				catch (IOException e)
				{
					e.printStackTrace();
				}
		}
		return compHelpInfo;
	}

	/**
	 * Method saveHelperClasses. Saves generated helper classes.
	 * @param compHelpInfo
	 */
	protected void saveHelperClasses(ComponentHelperInfo compHelpInfo, String contents, int fileNum)
	{
		// get root
		outputRootDir = compHelpInfo.getOutputRootDirectory();
		// get interfaces
		compInterfaces = compHelpInfo.getComponentInterface();

		for (int i = 0; i < compInterfaces.length; i++)
		{
			// get idl interface
			ComponentInterface componentInterface = compInterfaces[i];
			// get the name of the class
			String componentClassName = componentInterface.getComponentClassName();
			if (componentClassName == null)
			{
				System.err.println("...");
				System.err.println("Error: attribute componentClassName missing. No helper class has been generated.");
				continue;
			}
			
			if (m_verbose)
			{
				System.out.println("will try to generate component helper for '" + componentClassName + "'.");
			}
			
			// get idl package
			String idlPackage = componentInterface.getIdlPackage();
			if (idlPackage == null)
			{
				System.out.println(
					"Error: attribute idlPackage missing. No helper class for "
						+ componentClassName
						+ " has been generated.");
				continue;
			}

			// get the internal interface (could be null, if has not been specified)
			String internalInterface = componentInterface.getInternalInterface();
			String repositoryId = componentInterface.getCorbaRepositoryId();
			compHelper.initCompHelper(repositoryId, componentClassName, internalInterface, idlPackage, outputRootDir);
			try
			{
				compHelper.saveFile();
				if (m_verbose)
				{
					System.out.println("done. ");
				}
			}
			catch (Exception e)
			{
				System.out.println("saveHelperClasses(ComponentHelperInfo): exception " + e);
			}
		}
		if (m_verbose)
		{
			try
			{
				// save the transient xml as a file in the dierctory specified in the attribute outputRootDir
				ioSpecification.saveFile(
					contents,
					compHelper.computeFullXmlOutputPath() + "CompHelpGenInfo" + fileNum + ".xml");

			}
			catch (Exception e)
			{
				System.out.println("saveHelperClasses(ComponentHelperInfo): exception " + e);
			}
		}
	}

	/**
	 * TODO: DeRadification of the idiosynchratic formation of methods..
	 * <p>
	 * Method generate. Generates a file by reading the xml specification.
	 * @param contents
	 */
	public void generate(String contents)
	{
		System.out.print("start generating helper classes \n");
		
		if (contents == null)
		{
			return;
		}
		try
		{
			compHelperInfo = getFilePar(contents);
			if (compHelperInfo == null)
			{
				return;
			}

			saveHelperClasses(compHelperInfo, contents, xmlFileNum);
			xmlFileNum++;
		}
		catch (Exception e)
		{
			System.err.println("generate(String): exception while running CompHelperGenerator: " + e);
		}

		System.out.print("done generating helper classes \n");
	}
}
