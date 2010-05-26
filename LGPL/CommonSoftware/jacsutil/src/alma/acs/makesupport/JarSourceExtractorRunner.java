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
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.jar.Attributes;
import java.util.jar.JarFile;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.logging.Logger;

/**
 * Creates a jar file with java sources copied from inside jar files that are found
 * in a number of given directories.
 * <p>
 * The java source files are allowed to be stored with a prefix path "src" or "test" 
 * inside the jar files. The constructed jar file will not use any prefix path though.
 *
 * @author hsommer
 * created Sep 23, 2003 10:50:00 AM
 */
public class JarSourceExtractorRunner
{
	public static final String PROPERTY_EXTRACT_ONLY_GENERATED_JARS = "jarExtract.onlyGeneratedJars";
	
	public final static String Manifest_Attr_ACSGeneratedFromFile = "ACS-Generated-FromFile";
	
	
	/**
	 * First argument must be the jar file name to which all Java sources 
	 * @param args
	 */
	public static void main(String[] args)
	{
		if (args.length < 2)
		{
			System.err.println("usage: " + JarSourceExtractorRunner.class.getName() + 
								" outputJarFile jarDirectory1 jarDirectory2 ...");
			return;
		}
		
		try
		{
			Logger logger = Logger.getLogger("ACS.JarSourceExtractorRunner");
			
			// set up output jar file
			File targetJarFile = new File(args[0]);
			if (targetJarFile.exists())
			{
				targetJarFile.delete();
			}
			else
			{
				File parent = targetJarFile.getParentFile();
				if (parent != null)
				{
					parent.mkdirs();
				}
			}
			targetJarFile.createNewFile();
			if (!targetJarFile.isFile() || !targetJarFile.canWrite())
			{
				throw new IOException(targetJarFile + " is not a writable file.");
			}
			
			// get all input jar files
			File[] dirs = getDirectories(args);
			AcsJarFileFinder jarFinder = new AcsJarFileFinder(dirs, logger);
			File[] jarFiles = jarFinder.getAllFiles();

			// extract java sources
			if (jarFiles.length > 0)
			{
				JarSourceExtractor extractor = new JarSourceExtractor();
				FileOutputStream out = new FileOutputStream(targetJarFile);
				JarOutputStream jarOut = new JarOutputStream(out);
				
				for (int i = 0; i < jarFiles.length; i++)
				{
					JarFile jarFile = new JarFile(jarFiles[i]);
					if (needsProcessing(jarFile)) {
						extractor.extractJavaSourcesToJar(jarFile, jarOut);
					}
				}
				jarOut.finish();
				jarOut.close();
			}
			else
			{
				System.out.println("no jar files found.");
			}
		}
		catch (Exception e)
		{
			e.printStackTrace();
			System.exit(1);
		}
	}
	
	
	/**
	 * Encapsulates evaluation of possible flags (properties) that may restrict the set of jar files to be processed
	 * for source extraction, for example based on the manifest information.
	 * <p>
	 * Currently only looks at the boolean property <code>jarExtract.onlyGeneratedJars</code>.
	 *  
	 * @param jarFile
	 * @return
	 * @throws IOException
	 */
	static boolean needsProcessing(JarFile jarFile) throws IOException {
		// flag to 
		boolean onlyGeneratedJars = Boolean.getBoolean(PROPERTY_EXTRACT_ONLY_GENERATED_JARS);
		boolean needed = !onlyGeneratedJars;
		
		// only look inside if we are not already sure that this jar file is needed
		if (!needed) { 
		
			Manifest mani = jarFile.getManifest();
			if (mani != null) {
				Map<String, Attributes> entries = mani.getEntries();
	//			System.out.println("\n\nManifest for file " + jarFile.getName());
				
				Attributes mainAttrs = mani.getMainAttributes();
				for (Iterator mainAttrIter = mainAttrs.keySet().iterator(); mainAttrIter.hasNext();) {
					String attr = ((Attributes.Name) mainAttrIter.next()).toString();
					String value = mainAttrs.getValue(attr);
					if (attr.equals(Manifest_Attr_ACSGeneratedFromFile)) {
						needed = true;
						break;
					}
	//				System.out.println(attr + "=" + value);
				}
			}
		}
		return needed;
	}

	
	/**
	 * Identifies valid directories from the argument list.
	 * 
	 * @param mainArgs  arguments supplied to main method; 
	 * 					directories are given at indices 1...n
	 * @return  the valid directories as File objects 
	 * @throws Exception 
	 */
	static File[] getDirectories(String[] mainArgs) 
	{
		List<File> dirList = new ArrayList<File>();

		for (int i = 1; i < mainArgs.length; i++)
		{
			File jarDir = new File(mainArgs[i]);
			if (jarDir.exists() && jarDir.isDirectory())
			{
				dirList.add(jarDir);
			}
			else
			{
				System.err.println("ignoring invalid directory " + jarDir.getAbsolutePath());
			}
		}

		return (File[]) dirList.toArray(new File[dirList.size()]);
	}

	

}
