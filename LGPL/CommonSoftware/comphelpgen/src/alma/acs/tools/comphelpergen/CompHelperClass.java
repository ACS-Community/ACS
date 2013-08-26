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

import java.io.File;

/**
 * @author rgeorgie
 * This class generates helper classes that are specified by the parameters componentClassName, internalInterface, 
 * idlPackage, outputRootDir.
 * Update http://jira.alma.cl/browse/COMP-4783 bhola.panta@naoj 2011/08/05
 */

public class CompHelperClass
{
	private char sep = File.separatorChar;
	private static final String m_helperSuffix = "Helper";
	
	// external component info 
	private String m_outputRootDir;
	private String m_idlPackage;
	private String m_componentClassName;
	private String m_internalInterface;

	// computed component info 
	private String m_implPackage;
	private String m_operationsClass;
	private String m_repositoryId; // like "IDL:alma/demo/HelloDemo:1.0"

	private boolean m_verbose = false;

	IOSpecification ioSpecification;
	
	
	/**
	 * Constructs an object with a reference to the IO Specifications variables 
	 * current directory.
	 * @see java.lang.Object#Object()
	 */
	public CompHelperClass()
	{
		ioSpecification = new IOSpecification();
	}
	/**
	 * Constructs an object with a reference to the IO Specifications variables 
	 * current directory.
	 * @param verbose 
	 */
	public CompHelperClass(boolean verbose)
	{
		ioSpecification = new IOSpecification();
		m_verbose = verbose;
	}

	/**
	 * Returns an instance of the class.
	 * @param componentClassName
	 * @param internalInterface
	 * @param idlPackage
	 * @param outputRootDir
	 */
	public void initCompHelper(
		String repositoryId,
		String componentClassName,
		String internalInterface,
		String idlPackage,
		String outputRootDir)
	{
		ioSpecification = new IOSpecification();
		m_componentClassName = componentClassName;
		m_operationsClass = componentClassName + "Operations";
		m_outputRootDir = outputRootDir;
		m_idlPackage = idlPackage;
		m_implPackage = idlPackage + "." + componentClassName + "Impl";

		if (internalInterface != "")
		{
			m_internalInterface = internalInterface;
		}
		else
		{
			m_internalInterface = componentClassName + "J";
		}
		
		m_repositoryId = repositoryId;
	}

	/**
	 * Gets a line of code with the package description.
	 * @return String
	 */
	private String getImplPackage()
	{
		return "package " + m_implPackage + ";\n";
	}

	/**
	 * Gets the full name of the file.
	 * @return String
	 */
	private String getJavaComponentClass()
	{
		//return m_componentClassName.trim() + m_helperSuffix + ".java.tpl";
		return m_componentClassName.trim() + "Component" + m_helperSuffix + ".java.tpl";
	}

	/**
	 * Gets the full output path without having the name of the output file.
	 * @return String
	 */
	protected String computeFullOutputPath()
	{
		if (m_outputRootDir == null)
		{
			m_outputRootDir = ".";
		}

		String curPackage = m_implPackage.replace('.', File.separatorChar);
		String pathWithoutFile = m_outputRootDir + sep + curPackage + sep;

		// creates new directory or file
		try
		{
			boolean success = (new File(pathWithoutFile)).mkdir();
			if (!success)
			{
				success = (new File(pathWithoutFile)).mkdirs();
			}
			if (m_verbose)
			{
				System.out.println("computeFullOutputPath():" + pathWithoutFile);
			}
		}
		catch (Exception e)
		{
			System.out.println("computeFullOutputPath(): exception occured while creating a directory: " + e);
		}
		return pathWithoutFile;
	}

	/**
	 * Method computeFullXmlOutputPath. 
	 * @return String
	 */
	protected String computeFullXmlOutputPath()
	{
		if (m_outputRootDir == null)
		{
			m_outputRootDir = ".";
		}

		if (m_verbose)
		{
			System.out.println("outputRootDir: " + m_outputRootDir);
			System.out.println("implPackage: " + m_implPackage);
		}

		int root = m_implPackage.lastIndexOf(".");

		String curPackage = m_implPackage.substring(0, root).replace('.', sep);

		String pathWithoutFile = m_outputRootDir + sep + curPackage + sep;

		if (m_verbose)
		{
			System.out.println("curPackage: " + curPackage);
			System.out.println("pathWithoutFile: " + pathWithoutFile);
		}

		try
		{
			boolean success = (new File(pathWithoutFile)).mkdir();
			if (!success)
			{
				success = (new File(pathWithoutFile)).mkdirs();
			}
			if (m_verbose)
			{
				System.out.println("computeFullXmlOutputPath():" + pathWithoutFile);
			}
		}
		catch (Exception e)
		{
			System.out.println("computeFullXmlOutputPath(): exception occured while creating a directory: " + e);
		}
		return pathWithoutFile;
	}

	/**
	 * Outputs the the class name.
	 * @return String
	 */
	private String getClassName()
	{
		StringBuffer stringBuffer = new StringBuffer();
		stringBuffer.append(
			//"public class " + m_componentClassName + m_helperSuffix + " extends Component" + m_helperSuffix + "\n");
			"public class " + m_componentClassName + "Component" + m_helperSuffix + " extends Component" + m_helperSuffix + "\n");
		return stringBuffer.toString();
	}

	/**
	 * Outputs the code with the author's name.
	 * @return String
	 */
	private String getClassJavadoc()
	{
		StringBuffer stringBuffer = new StringBuffer();
		stringBuffer.append("/**\n");
		stringBuffer.append(" * Component helper class. \n");
		stringBuffer.append(" * Generated for convenience, but can be modified by the component developer. \n");
		stringBuffer.append(" * Must therefore be treated like any other Java class (CVS, ...). \n");
		stringBuffer.append(" * <p>\n");
		// might be null since some old tests have not been properly migrated...
		if (m_repositoryId != null)
		{
			String cdbEntryAttrs = "Name=\"" + m_componentClassName.toUpperCase() + "_1\" " +
							"Code=\"" + m_implPackage + "." + m_componentClassName + "Component" + m_helperSuffix + "\" " +
							"Type=\"" + m_repositoryId + "\" " +
							"Container=\"frodoContainer\" ImplLang=\"java\"";
			stringBuffer.append(" * To create an entry for your component in the Configuration Database, \n");
			stringBuffer.append(" * copy the line below into a new entry in the file $ACS_CDB/MACI/Components/Components.xml \n");
			stringBuffer.append(" * and modify the instance name of the component and the container: \n");
			stringBuffer.append(" * <p>\n");
			stringBuffer.append(" * " + cdbEntryAttrs + "\n");
			stringBuffer.append(" * <p>\n");
		}
		stringBuffer.append(" * @author alma-component-helper-generator-tool\n");
		stringBuffer.append(" */\n");
		return stringBuffer.toString();
	}

	/**
	 * Outputs the code for all the imports.
	 * @return String
	 */
	private String getImports()
	{
		StringBuffer stringBuffer = new StringBuffer();
		
		stringBuffer.append("import java.util.logging.Logger;\n\n");
		stringBuffer.append("import org.omg.PortableServer.Servant;\n");
		stringBuffer.append("import alma.ACS.ACSComponentOperations;\n");
		stringBuffer.append("import alma.acs.component.ComponentLifecycle;\n");
		stringBuffer.append("import alma.acs.container.Component" + m_helperSuffix + ";\n");
		stringBuffer.append("import " + m_idlPackage + "." + m_operationsClass + ";\n");
		stringBuffer.append("import " + m_idlPackage + "." + m_componentClassName + "POATie;\n");
		stringBuffer.append(
			"import " + m_idlPackage + "." + m_componentClassName + "Impl." + m_componentClassName + "Impl;\n");

		if (m_internalInterface != null)
		{
			// alma.HelloApp.SchedulerJ - SchedulerImpl
			stringBuffer.append("import " + m_idlPackage + "." + m_internalInterface + ";\n");
		}

		return stringBuffer.toString();
	}

	/**
	 * Outputs the code for the constructor method.
	 * @return String
	 */
	private String ctor()
	{
		StringBuffer stringBuffer = new StringBuffer();
		stringBuffer.append("\t/**\n");
		stringBuffer.append("\t * Constructor\n");
		stringBuffer.append("\t * @param containerLogger logger used only by the parent class.\n");
		stringBuffer.append("\t */\n");
		//stringBuffer.append("\tpublic ").append(m_componentClassName).append(m_helperSuffix);
		stringBuffer.append("\tpublic ").append(m_componentClassName).append("Component").append(m_helperSuffix);
		stringBuffer.append("(Logger containerLogger)\n");
		stringBuffer.append("\t{\n");
		stringBuffer.append("\t\tsuper(containerLogger);\n");
		stringBuffer.append("\t}\n");

		return stringBuffer.toString();
	}

	/**
	 * Outputs the code for the method _createComponentImpl().
	 * @return String
	 */
	private String createComponentImpl()
	{
		StringBuffer stringBuffer = new StringBuffer();
		stringBuffer.append("\t/**\n");
		stringBuffer.append("\t* @see alma.acs.container.Component" + m_helperSuffix + "#_createComponentImpl()\n");
		stringBuffer.append("\t*/\n");
		stringBuffer.append("\tprotected ComponentLifecycle _createComponentImpl()\n");
		stringBuffer.append("\t{\n");
		stringBuffer.append("\t\treturn new " + m_componentClassName + "Impl();\n");
		stringBuffer.append("\t}\n");

		return stringBuffer.toString();
	}

	/**
	 * Outputs the code for the method _getPOATieClass().
	 * @return String
	 */
	private String getPOATieClass()
	{
		String POATieClass = m_componentClassName + "POATie.class";
		StringBuffer stringBuffer = new StringBuffer();
		stringBuffer.append("\t/**\n");
		stringBuffer.append("\t* @see alma.acs.container.Component" + m_helperSuffix + "#_getPOATieClass()\n");
		stringBuffer.append("\t*/\n");
		stringBuffer.append("\tprotected Class<? extends Servant> _getPOATieClass()\n");
		stringBuffer.append("\t{\n");
		stringBuffer.append("\t\treturn " + POATieClass + ";\n");
		stringBuffer.append("\t}\n");

		return stringBuffer.toString();
	}

	/**
	 * Outputs the code for the method _getOperationsInterface()
	 * @return String
	 */
	private String getOperationsIrf()
	{
		StringBuffer stringBuffer = new StringBuffer();
		stringBuffer.append("\t/**\n");
		stringBuffer.append("\t* @see alma.acs.container.Component" + m_helperSuffix + "#getOperationsInterface()\n");
		stringBuffer.append("\t*/\n");
		stringBuffer.append("\tprotected Class<? extends ACSComponentOperations> _getOperationsInterface()\n");
		stringBuffer.append("\t{\n");
		stringBuffer.append("\t\treturn " + m_operationsClass + ".class;\n");
		stringBuffer.append("\t}\n");

		return stringBuffer.toString();
	}

	/**
	 * Outputs the code for the method _getInternalInterface().
	 * @return String
	 */
	private String getInternalIrf()
	{
		StringBuffer stringBuffer = new StringBuffer();
		stringBuffer.append("\t/**\n");
		stringBuffer.append("\t* @see alma.acs.container.Component" + m_helperSuffix + "#getInternalInterface()\n");
		stringBuffer.append("\t*/\n");
		stringBuffer.append("\tprotected Class<?> getInternalInterface()\n");
		stringBuffer.append("\t{\n");
		stringBuffer.append("\t\treturn " + m_internalInterface + ".class;\n");
		stringBuffer.append("\t}\n");

		return stringBuffer.toString();
	}
	/**
	 * Outputs the code for the LGPL.
	 * @return String
	 */
	private String getLicenceHeader()
	{
		StringBuffer stringBuffer = new StringBuffer();
		stringBuffer.append("/*\n");
		stringBuffer.append(" *    ALMA - Atacama Large Millimiter Array\n");
		stringBuffer.append(" *    (c) European Southern Observatory, 2002\n");
		stringBuffer.append(" *    Copyright by ESO (in the framework of the ALMA collaboration),\n");
		stringBuffer.append(" *    All rights reserved\n");
		stringBuffer.append(" *\n");
		stringBuffer.append(" *    This library is free software; you can redistribute it and/or\n");
		stringBuffer.append(" *    modify it under the terms of the GNU Lesser General Public\n");
		stringBuffer.append(" *    License as published by the Free Software Foundation; either\n");
		stringBuffer.append(" *    version 2.1 of the License, or (at your option) any later version.\n");
		stringBuffer.append(" *\n");
		stringBuffer.append(" *    This library is distributed in the hope that it will be useful,\n");
		stringBuffer.append(" *    but WITHOUT ANY WARRANTY; without even the implied warranty of\n");
		stringBuffer.append(" *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\n");
		stringBuffer.append(" *    Lesser General Public License for more details.\n");
		stringBuffer.append(" *\n");
		stringBuffer.append(" *    You should have received a copy of the GNU Lesser General Public\n");
		stringBuffer.append(" *    License along with this library; if not, write to the Free Software\n");
		stringBuffer.append(" *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, \n");
		stringBuffer.append(" *    MA 02111-1307  USA\n");
		stringBuffer.append(" */\n");
		return stringBuffer.toString();
	}

	/**
	 * Collects all the blocks needed for outputing the current file's contents.
	 * @return String
	 */
	private String getContents()
	{
		StringBuffer stringBuffer = new StringBuffer();
		stringBuffer.append(getLicenceHeader());
		stringBuffer.append("\n");
		stringBuffer.append(getImplPackage());
		stringBuffer.append("\n");
		stringBuffer.append(getImports());
		stringBuffer.append("\n");
		stringBuffer.append(getClassJavadoc());
		stringBuffer.append(getClassName());
		stringBuffer.append("{\n");
		stringBuffer.append(ctor());
		stringBuffer.append("\n");
		stringBuffer.append(createComponentImpl());
		stringBuffer.append("\n");
		stringBuffer.append(getPOATieClass());
		stringBuffer.append("\n");
		stringBuffer.append(getOperationsIrf());
		stringBuffer.append("\n");
		if (m_internalInterface != null)
		{
			stringBuffer.append(getInternalIrf());
			stringBuffer.append("\n");
		}
		stringBuffer.append("}\n");

		return stringBuffer.toString();
	}

	/**
	 * Saves a file according to its contents and location.
	 */
	protected void saveFile()
	{
		try
		{
			if (m_verbose)
			{
				System.out.println(
					"computeFullOutputPath() + getJavaComponentClass(): "
						+ computeFullOutputPath()
						+ getJavaComponentClass());
			}
			String file = computeFullOutputPath() + getJavaComponentClass();

			ioSpecification.saveFile(getContents(), file);
		}
		catch (Exception e)
		{
			System.out.println("saveFile(): exception " + e);
		}

	}
}
