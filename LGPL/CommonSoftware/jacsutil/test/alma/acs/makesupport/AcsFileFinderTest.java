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
package alma.acs.makesupport;

import java.io.File;
import java.io.FileOutputStream;
import java.io.FileNotFoundException;
import java.util.jar.JarFile;
import java.util.jar.JarOutputStream;
import java.util.logging.Logger;

import junit.framework.TestCase;

import alma.acs.testsupport.tat.TATJUnitRunner;

/**
 * @author hsommer
 * created Sep 16, 2003 3:02:44 PM
 */
public class AcsFileFinderTest extends TestCase
{
	private Logger m_logger;
	private File[] m_dirs;


	protected void setUp() throws Exception
	{
		super.setUp();
		
		m_logger = Logger.getLogger("AcsFileFinderTest");

		String introot = System.getProperty("ACS.introot");
		String acsroot = System.getProperty("ACS.acsroot");

		File introotLibDir = null;
		if (introot != null)
		{
			introotLibDir = new File(introot + File.separator + "lib");
		}
		else
		{
			throw new NullPointerException("Property 'ACS.introot' must be defined!"); 
		}

		File acsrootLibDir = null;
		if (acsroot != null)
		{
			acsrootLibDir = new File(acsroot + File.separator + "lib");
		}
		else
		{
			throw new NullPointerException("Property 'ACS.acsroot' must be defined!"); 
		}

		m_dirs = new File[]{introotLibDir, acsrootLibDir};

	}


	public void testGetJarFiles()
	{
		AcsJarFileFinder jarFinder = new AcsJarFileFinder(m_dirs, m_logger);

		File[] jarFiles = jarFinder.getAllFiles();
		
		assertNotNull(jarFiles);
				
		String cp = jarFinder.getClasspath();
		assertNotNull(cp);
		m_logger.info("here's your classpath:\n" + cp);
	}
	
	
	public void testExtractor() throws Exception
	{
		AcsJarFileFinder jarFinder = new AcsJarFileFinder(m_dirs, m_logger);

		File[] jarFiles = jarFinder.getAllFiles();
		assertNotNull(jarFiles);
		assertTrue(jarFiles.length > 0);
		
		JarSourceExtractor extractor = new JarSourceExtractor();
		
		// test writing separate .java files
		File tempDir = new File("jsrc"); // System.getProperty("java.io.tmpdir"
		tempDir.mkdir();
		for (int i = 0; i < jarFiles.length; i++)
		{
			JarFile jarFile = new JarFile(jarFiles[i]);
			extractor.extractJavaSourcesToFiles(jarFile, tempDir);			
		}
		
		// test writing a JAR file with all .java inside\
		File targetJarFile = new File(tempDir, "alljava.jar");
		targetJarFile.delete();
		
		FileOutputStream out = new FileOutputStream(targetJarFile);
		JarOutputStream jarOut = new JarOutputStream(out);

		for (int i = 0; i < jarFiles.length /*&& i < 20*/; i++)
		{
			JarFile jarFile = new JarFile(jarFiles[i]);
			extractor.extractJavaSourcesToJar(jarFile, jarOut);			
		}
		jarOut.finish();
		jarOut.close();
	}
	

	public static void main(String[] args)
	{
                try{
		        TATJUnitRunner.run(AcsFileFinderTest.class);
                }catch(FileNotFoundException ex){
                        System.err.print("Error opening file:"+ex.getMessage());
                }
	}

}
