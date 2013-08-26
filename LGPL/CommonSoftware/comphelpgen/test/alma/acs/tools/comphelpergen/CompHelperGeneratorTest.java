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

/**
 * @author rgeorgie
 *
 * Test for CompHelperGenerator. Uses samples of xml to generate helper classes for xmlcomponent, component and both.
 * Update: http://jira.alma.cl/browse/COMP-4783 bhola.panta@naoj 2011/09/05 
 */

public class CompHelperGeneratorTest extends junit.framework.TestCase
{
	CompHelperGenerator compHelperGenerator = new CompHelperGenerator(true);
	IOSpecification ioSpecification = new IOSpecification();
	CompHelperClass compHelperClass = new CompHelperClass();

	private char sep = System.getProperty("file.separator").charAt(0);

	private String outputRootDir = "";

	public CompHelperGeneratorTest(String name)
	{
		super(name);

	}

	public void testGenerateAllComp()
	{
		outputRootDir = ioSpecification.getDir();
		generateHelpComp();
		generateHelpXmlComp();
		generateCompDir();
		generateHelpMixedComp();
	}

	public void generateHelpComp()
	{
		// Xml String for PrimaryComponent

		String firstComponentList =
			"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n"
				+ "<ComponentHelperInfo outputRootDirectory=\""
				+ outputRootDir
				+ "\">\n"
				+ "<ComponentInterface componentClassName=\"PrimaryComponent\" idlPackage=\"alma.exmplCompHelpGen\"/>\n"
				+ "</ComponentHelperInfo>";
		compHelperGenerator.generate(firstComponentList);
		compHelperClass.initCompHelper(null, "PrimaryComponent", null, "alma.exmplCompHelpGen", outputRootDir);

		// read produced file
		//String producedFile =
		//	ioSpecification.getOutputRootDir() + "PrimaryComponentImpl" + sep + "PrimaryComponentHelper.java.tpl";
		String producedFile =
			ioSpecification.getOutputRootDir() + "PrimaryComponentImpl" + sep + "PrimaryComponentComponentHelper.java.tpl";
		String producedConts = "", sampleConts = "";
		String sampleFile = null;
		try
		{
			producedConts = ioSpecification.readingFile(producedFile);
			//	read sample file
			sampleFile = ioSpecification.getExmplDir() + "exmplPrimaryComponentHelper.java.tpl";
			sampleConts = ioSpecification.readingFile(sampleFile);
		}
		catch (Exception e)
		{
			System.out.println("exception: " + e.getStackTrace());
		}
//		System.out.println("sampleConts\n" + sampleConts);
//		System.out.println("producedConts\n" + producedConts);
		assertEquals("produced helper class must match " + sampleFile, sampleConts, producedConts);
	}

	public void generateHelpXmlComp()
	{
		// Xml String for PrimaryXmlComponent

		String firstXmlComponentList =
			"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n"
				+ "<ComponentHelperInfo outputRootDirectory=\""
				+ outputRootDir
				+ "\">\n"
				+ "<ComponentInterface componentClassName=\"PrimaryXmlComponent\" idlPackage=\"alma.exmplCompHelpGen\" internalInterface=\"PrimaryXmlComponentJ\"/>\n"
				+ "</ComponentHelperInfo>";
		compHelperGenerator.generate(firstXmlComponentList);
		compHelperClass.initCompHelper(null, "PrimaryXmlComponent", "XmlComponentJ", "alma.exmplCompHelpGen", outputRootDir);

		// read produced file
		//String producedXmlFile =
		//	ioSpecification.getOutputRootDir() + "PrimaryXmlComponentImpl" + sep + "PrimaryXmlComponentHelper.java.tpl";
		String producedXmlFile =
			ioSpecification.getOutputRootDir() + "PrimaryXmlComponentImpl" + sep + "PrimaryXmlComponentComponentHelper.java.tpl";
		String producedXmlConts = "", sampleXmlConts = "";
		String sampleXmlFile = null;
		try
		{
			producedXmlConts = ioSpecification.readingFile(producedXmlFile);
			// read sample file
			sampleXmlFile = ioSpecification.getExmplDir() + "exmplPrimaryXmlComponentHelper.java.tpl";
			sampleXmlConts = ioSpecification.readingFile(sampleXmlFile);

		}
		catch (Exception e)
		{
			System.out.println("exception: " + e.getStackTrace());
		}
		assertEquals("produced helper class must match " + sampleXmlFile, sampleXmlConts, producedXmlConts);
	}

	public void generateCompDir()
	{
		// Xml String for PrimaryXmlComponent

		String firstXmlComponentList =
			"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n"
				+ "<ComponentHelperInfo outputRootDirectory=\""
				+ "."
				+ "\">\n"
				+ "<ComponentInterface componentClassName=\"PrimaryXmlComponent\" idlPackage=\"alma.exmplCompHelpGen\" internalInterface=\"PrimaryXmlComponentJ\"/>\n"
				+ "</ComponentHelperInfo>";
		compHelperGenerator.generate(firstXmlComponentList);
		compHelperClass.initCompHelper(null, "PrimaryXmlComponent", "XmlComponentJ", "alma.exmplCompHelpGen", outputRootDir);

		// read produced file
		//String producedXmlFile =
			//ioSpecification.getOutputRootDir() + "PrimaryXmlComponentImpl" + sep + "PrimaryXmlComponentHelper.java.tpl";
		String producedXmlFile =
			ioSpecification.getOutputRootDir() + "PrimaryXmlComponentImpl" + sep + "PrimaryXmlComponentComponentHelper.java.tpl";
		String producedXmlConts = "", sampleXmlConts = "";
		try
		{
			producedXmlConts = ioSpecification.readingFile(producedXmlFile);
			// read sample file
			String sampleXmlFile = ioSpecification.getExmplDir() + "exmplPrimaryXmlComponentHelper.java.tpl";
			sampleXmlConts = ioSpecification.readingFile(sampleXmlFile);
		}
		catch (Exception e)
		{
			System.out.println("exception: " + e.getStackTrace());
		}
		assertEquals("not equal.", sampleXmlConts, producedXmlConts);
	}

	public void generateHelpMixedComp()
	{
		//	Xml String for SecondaryComponent and SecondaryXmlComponent

		String firstMixedComponentList =
			"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n"
				+ "<ComponentHelperInfo outputRootDirectory=\""
				+ outputRootDir
				+ "\">\n"
				+ "<ComponentInterface componentClassName=\"SecondaryComponent\" idlPackage=\"alma.exmplCompHelpGen\"/>\n"
				+ "<ComponentInterface componentClassName=\"SecondaryXmlComponent\" idlPackage=\"alma.exmplCompHelpGen\" internalInterface=\"SecondaryXmlComponentJ\"/>\n"
				+ "</ComponentHelperInfo>";

		compHelperGenerator.generate(firstMixedComponentList);
		compHelperClass.initCompHelper(null, "SecondaryComponent", null, "alma.exmplCompHelpGen", outputRootDir);

		String producedFile1 =
			//ioSpecification.getOutputRootDir() + "SecondaryComponentImpl" + sep + "SecondaryComponentHelper.java.tpl";
			ioSpecification.getOutputRootDir() + "SecondaryComponentImpl" + sep + "SecondaryComponentComponentHelper.java.tpl";

		compHelperClass.initCompHelper(
			null,
			"SecondaryXmlComponent",
			"XmlComponentJ",
			"alma.exmplCompHelpGen",
			outputRootDir);

		String producedFile2 =
			ioSpecification.getOutputRootDir()
				+ "SecondaryXmlComponentImpl"
				+ sep
				//+ "SecondaryXmlComponentHelper.java.tpl";
				+ "SecondaryXmlComponentComponentHelper.java.tpl";
		String sampleConts1 = "", producedConts1 = "", sampleConts2 = "", producedConts2 = "";
		try
		{
			producedConts1 = ioSpecification.readingFile(producedFile1);
			producedConts2 = ioSpecification.readingFile(producedFile2);

			String sampleFile1 = ioSpecification.getExmplDir() + "exmplSecondaryComponentHelper.java.tpl";
			String sampleFile2 = ioSpecification.getExmplDir() + "exmplSecondaryXmlComponentHelper.java.tpl";

			sampleConts1 = ioSpecification.readingFile(sampleFile1);
			sampleConts2 = ioSpecification.readingFile(sampleFile2);
		}
		catch (Exception e)
		{
			System.out.println("exception: " + e.getStackTrace());
		}
		assertEquals("not equal.", sampleConts1, producedConts1);
		assertEquals("not equal.", sampleConts2, producedConts2);
	}
}
