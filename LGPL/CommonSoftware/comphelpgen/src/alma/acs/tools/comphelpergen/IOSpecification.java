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
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

/**
 * @author rgeorgie
 *
 * Class that is used for getting the current directory 
 * and saves a file when helper classes are generated.
 */
public class IOSpecification
{

	private char sep = System.getProperty("file.separator").charAt(0);

	/**
	 * Gets the current root input directory.
	 * @return String
	 */
	protected String getDir()
	{
		String curDir = System.getProperty("user.dir");
		return curDir;
	}

	/**
	 * Saves a file given its contents and path are specified.
	 * @param contents
	 * @param file
	 */
	protected void saveFile(String contents, String file)
	{
		try
		{
			FileWriter fileWriter = new FileWriter(file);
			fileWriter.write(contents);
			fileWriter.close();
		}
		catch (FileNotFoundException e)
		{
			e.printStackTrace();
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}

	}
	/**
	 * Gets the directory where the sample files are found.
	 * @return String
	 */
	protected String getExmplDir()
	{
		String dir = getDir() + sep + "alma" + sep + "exmplExpected" + sep;
		// check whether the file is availabel from the current settings
		String file = getDir() + sep + "alma" + sep + "exmplExpected" + sep + "exmplPrimaryXmlComponentHelper.java.tpl";
		File propFile = new File(file);
		if (!propFile.exists())
		{
			System.err.println("Please set user.dir to point to your test directory.");
			//	-Duser.dir=$PWD
		}
		return dir;
	}
	/**
	 * Gets the directory where the sample files should be output.
	 * @return String
	 */
	protected String getOutputRootDir()
	{
		String dir = getDir() + sep + "alma" + sep + "exmplCompHelpGen" + sep;

		return dir;
	}
	/**
	 * Reads a sample test file and ignores all the line terminators.
	 * @return String
	 */
	protected String readingFile(String file)
	{
		String contents = "";
		FileReader fr = null;
		try
		{
			fr = new FileReader(file);
			int c = fr.read();
			while (c != -1)
			{
				Character character = new Character((char) c);
				Character blankSpace = new Character(' ');
				Character newLine = new Character('\n');
				Character ret = new Character('\r');
				Character tab = new Character('\t');
				if (character.compareTo(newLine) != 0
					&& character.compareTo(blankSpace) != 0
					&& character.compareTo(ret) != 0
					&& character.compareTo(tab) != 0)
				{
					contents = contents + (char) c;
				}
				c = fr.read();
			}
			fr.close();
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
		finally
		{
			try
			{
				if (fr != null)
					fr.close();
			}
			catch (IOException e)
			{
				e.printStackTrace();
			}
		}
		return contents;
	}
}