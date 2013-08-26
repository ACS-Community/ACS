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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.jar.JarOutputStream;
import java.util.logging.Logger;

import junit.framework.TestCase;

import alma.acs.testsupport.tat.TATJUnitRunner;
import alma.acs.util.StopWatch;

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
		String jacorbhome = System.getProperty("ACS.jacorbhome");
		String jdkhome = System.getProperty("JAVA_HOME");

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
		
		File jacorbLibDir = null;
		if (jacorbhome != null)
		{
			jacorbLibDir = new File(jacorbhome + File.separator + "lib");
		}
		else
		{
			throw new NullPointerException("Property 'ACS.jacorbhome' must be defined!");
		}
		
		File jdkLibDir = null;
		File jreLibDir = null;
		if (jdkhome != null)
		{
			jdkLibDir = new File(jdkhome + File.separator + "lib");
			jreLibDir = new File(jdkhome + File.separator + "jre" + File.separator + "lib");
		}
		else
		{
			throw new NullPointerException("Property 'JAVA_HOME' must be defined!");
		}

		m_dirs = new File[]{introotLibDir, acsrootLibDir, jacorbLibDir, jdkLibDir, jreLibDir};

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
		if (tempDir.mkdir() == false) 
			m_logger.finest("Directory "+tempDir.toString()+" might already exist.");
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
	
	public void testClassExtractor() throws Exception
	{
		AcsJarFileFinder jarFinder = new AcsJarFileFinder(m_dirs, m_logger);

		File[] jarFiles = jarFinder.getAllFiles();
		assertNotNull(jarFiles);
		assertTrue(jarFiles.length > 0);
		
		JarClassExtractor extractor = new JarClassExtractor();
		
		File tempDir = new File("jclass");
		if (tempDir.mkdir() == false) 
			m_logger.finest("Directory "+tempDir.toString()+" might already exist.");
		long numClasses = 0;
		StopWatch sw = new StopWatch(m_logger);
		HashMap<String,String> classToJarMap = new HashMap<String,String>();
		for (int i= 0; i < jarFiles.length; i++)
		{
			JarFile jarFile = new JarFile(jarFiles[i]);
			JarEntry[] entries = extractor.getJavaEntries(jarFile);
			String jarName = jarFile.getName();
			numClasses+=entries.length;

			for (JarEntry jarEntry : entries) {
			    String className = jarEntry.getName();
			    className = className.substring(0,className.length()-6); // Remove ".class" extension
				String earlierJar = classToJarMap.put(className,jarName);
				if (earlierJar != null)
					{
					//m_logger.info(className+" "+jarName);	
					//m_logger.severe("Class "+className+" was also in "+earlierJar);
					}
				//m_logger.info(className+" "+jarName);
			}
		}

		m_logger.info("Number of classes found: "+numClasses+ " in "+sw.getLapTimeMillis()+" ms.");
		
		File jcontClasses = new File("jcontClasses.txt");
		List<String> jarsFound = new ArrayList<String>();
		try {
		      //use buffering, reading one line at a time
		      //FileReader always assumes default encoding is OK!
		      BufferedReader input =  new BufferedReader(new FileReader(jcontClasses));
		      try {
		        String className = null; //not declared within while loop
		        /*
		        * readLine is a bit quirky :
		        * it returns the content of a line MINUS the newline.
		        * it returns null only for the END of the stream.
		        * it returns an empty String if two newlines appear in a row.
		        */
		        while (( className = input.readLine()) != null){
		          className = className.trim();
		          String jarFound = classToJarMap.get(className);
		          if (jarFound == null && !className.startsWith("java")) System.out.println("Can't find jar for "+className);
		          if (jarFound != null && !jarsFound.contains(jarFound)) 
		          {
		        	  jarsFound.add(jarFound);
		          }
		        }
		      }
		      finally {
		        input.close();
		      }
		    }
		    catch (IOException ex){
		      ex.printStackTrace();
		    }
		    for (String string : jarsFound) {
				System.out.println(string);
			}

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
