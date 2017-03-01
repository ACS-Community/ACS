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
package alma.acs.classloading;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URI;
import java.net.URL;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.zip.InflaterOutputStream;

/**
 * Helps sort a list of jar files so that more important jar files appear first.
 * The more important jar files are listed in {@link #orderedAcsJarNames} and optionally also 
 * in the property <code>acs.system.classpath.appltopjars</code>.
 * <p>
 * The classloader will then not have to read through unimportant jar files first, 
 * which should improve class loading performance on IO-challenged machines.
 *   
 * @author hsommer
 * created Sep 21, 2004 2:28:51 PM
 */
public class JarOrderOptimizer implements Comparator<File>
{
	private boolean verbose = false;

	public static final String PROPERTY_APPLICATION_TOPJARS = "acs.system.classpath.appltopjars";
	
	/**
	 * The name of the file in the alma.acs.classloading package with the list of top jars
	 */
	public static final String ACS_TOP_JARS_FILENAME = "AcsTopJars.txt";
	
	/**
	 * key = (String) jarname, value = (Integer) position.
	 */
	private final Map<String, Integer> topJarMap;
	
	JarOrderOptimizer(boolean verbose) {
		this.verbose = verbose;
		
		topJarMap = readJarsFromFile();
		int i = topJarMap.size();
		
		String applJarPath = System.getProperty(PROPERTY_APPLICATION_TOPJARS);
		if (applJarPath != null) {
			String[] applJarNames = parseJarNames(applJarPath);
			for (int j=0; j < applJarNames.length; j++) {
				if (!topJarMap.containsKey(applJarNames[j])) {
					topJarMap.put(applJarNames[j], (i+j));
				}
			}
		}
//		if (verbose) {		
//			for (Iterator iter = topJarMap.keySet().iterator(); iter.hasNext();) {
//				String jarName = (String) iter.next();
//				Integer pos = topJarMap.get(jarName);
//				System.out.print(pos.toString() + "-" + jarName + "  ");
//			}
//			System.out.println();
//		}
	}
	
	/**
	 * Get the list of files from {@value #ACS_TOP_JARS_FILENAME} file.
	 */
	private HashMap<String, Integer> readJarsFromFile() {
		HashMap<String, Integer> ret = new HashMap<String, Integer>();
		InputStream stream = getClass().getResourceAsStream("AcsTopJars.txt");
		Reader reader = new InputStreamReader(stream);
		try {
		    BufferedReader br = new BufferedReader(reader);
		    String line;
		    int key = 0;
		    while((line = br.readLine()) != null){
		        // Remove comment
		        int pos = line.indexOf('#');
		        if (pos>=0) {
		        	line = line.substring(0,pos).trim();
		        }
		        // Minimal check of the validity of the jar name
		        //
		        // The error will show up in the test and let us know that something is wrong in the
		        // list of jars of the file
		        if (line.endsWith(".jar")) {
		        	ret.put(line, key++);
		        } else if (!line.isEmpty()) {
		        	System.err.println("\nWrong format of ACS top jar ["+line+"]: jar discarded!\nCheck content of "+ACS_TOP_JARS_FILENAME+"\n");
		        }
	        	
	        }
		    br.close();
		    reader.close();
		} catch (Throwable t) {
			t.printStackTrace(System.err);
		}
		return ret;
	}
	
	public int compare(File f1, File f2)
	{
		int ret = 0;
		
		if (f1 == null || f2 == null) { // todo: check if this can happen
			throw new NullPointerException("bad null arg"); 
		}
		String n1 = f1.getName();
		String n2 = f2.getName();

		Integer i1 = topJarMap.get(n1);
		Integer i2 = topJarMap.get(n2);

		if (i1 != null) {
			if (i2 != null) {
				ret = i1.compareTo(i2);
			}
			else {
				ret = -1;
			}
		}
		else {
			if (i2 != null) {
				ret = 1;
			}
			else { // both not in list -- need to find some sorting criterion, why not alphabetically
				return n1.compareTo(n2);
			}
		}
		return ret;
	}

	
	/**
	 * Sorts <code>jars</code> using {@link #compare(Object, Object)}.
	 * @see Arrays#sort(java.lang.Object[], java.util.Comparator)
	 */
	void sortJars(List<File> jarlist) {
		Collections.sort(jarlist, this);
	}

	
	/**
	 * To be used for testing only -- allows to filter out all jar files from a list which are not 
	 * given priority by the <code>compare</code> method of this class.
	 * This allows to write tests that will fail unless all required classes are listed explicitly.
	 * @param allJars  jar files to be filtered
	 * @return  those whose name matches one from the prio list
	 */
	List<File> getTopJarsOnly(List<File> allJars) {
		List<File> topJars = new ArrayList<File>();
		for (Iterator<File> iter = allJars.iterator(); iter.hasNext();) {
			File jarfile = iter.next();
			if (topJarMap.containsKey(jarfile.getName())) {
				topJars.add(jarfile);
			}
		}
		return topJars;
	}
	
	
	/**
	 * Parses a string of concatenated jar file names.
	 * For example, the string "ab:cd.jar:ef" should yield {"ab.jar", "cd.jar", "ef.jar"}. 
	 */
	private String[] parseJarNames(String jarNamePath)
	{
		StringTokenizer tok = new StringTokenizer(jarNamePath, ":"
				+ File.pathSeparatorChar); // to also allow platform style separator
		List jarNameList = new ArrayList();
		for (int i = 0; tok.hasMoreTokens(); i++) {
			String jarName = tok.nextToken();
			if (jarName != null && jarName.trim().length() > 0) {
				jarName = jarName.trim();
				if (!jarName.toLowerCase().endsWith(".jar")) {
					jarName += ".jar";
				}
				jarNameList.add(jarName);
			}
		}
		return (String[]) jarNameList.toArray(new String[jarNameList.size()]);
	}

	/**
	 * Checks if a class comes from any of the subpackages of <code>sun.</code> or <code>com.sun.</code>
	 * which we strongly assume to not be contained in any jar files that our classloaders have to deal with.
	 * These classes should either be loaded by the real system class loader, and when it fails, we assume that they 
	 * don't exist anywhere so we can skip searching for them. 
	 * @param name
	 * @return
	 */
	boolean isClassKnownToBeUnavailable(String name) {
		return (name.startsWith("sun.util.logging.") || 
				name.startsWith("sun.text.resources.") ||
				name.startsWith("sun.awt.resources.") ||
				name.startsWith("com.sun.swing.internal.plaf.")
			);
	}
}
