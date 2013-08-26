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
package alma.acs.eclipse.utils.pluginbuilder;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.Set;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Read the list of jars from the <code>build.properties</code>.
 * 
 * @author acaproni
 * @since ACS-8.0.1
 *
 */
public class BuildProperties {
	
	/**
	 * The name of the file of eclipse properties to read
	 */
	public static final String fileOfProperties = "build.properties";
	
	/**
	 * The regular expression with the name of the property in the 
	 * <code>fileOfProperties</code> listing the required jars.
	 */
	public final Pattern propertyPattern = Pattern.compile("\\s*jars.extra.classpath\\s*=\\s*");
	
	/**
	 * The regular expression with the name of a jar 
	 */
	public final Pattern jarPattern = Pattern.compile(".*jar");
	
	/**
	 * The list of required jars read from the file.
	 * <P>
	 * The vector preserve the order i.e. item at position 0
	 * contains the first read jar and so on.
	 * <P>
	 * The name of a jar is intended as written in the <code>fileOfProperties</code>
	 * file like for example <code>platform:/plugin/ACS_Utility_Jars/jacorb.jar</code>.
	 * <P><b>Note</b>: the order of the entries in the file is important and therefore
	 * 	a {@link Set} can't be used.
	 */
	private final Vector<String> classpathEntries = new Vector<String>();
	
	/**
	 * Constructor
	 * 
	 * @param folder The folder containing the <code>fileOfProperties</code> file.
	 */
	public BuildProperties(String folder) throws Exception {
		if (folder==null || folder.isEmpty()) {
			throw new IllegalArgumentException("Invalid folder: "+folder);
		}
		File f = new File(folder+"/"+fileOfProperties);
		try {
			generateListOfJars(f);
		} catch (Throwable t) {
			System.out.println(t.getMessage());
			t.printStackTrace();
			throw new Exception("Error reading "+f.getAbsolutePath(),t);
		}
		System.out.println(""+classpathEntries.size()+" jars found in "+fileOfProperties);
	}
	
	/**
	 * Read the file and get the jars of the 
	 * <code>propertyPattern</code> property.
	 * <P>
	 * Implementation note:
	 * <OL>
	 * 	<LI>scan the file until found a string matching <code>propertyPattern</code>
	 * 	<LI>read and concatenate all the jars in the string
	 * 	<LI>repeat the previous step if the line continue in the next line 
	 * 		i.e. the line terminates with "\"
	 * 	<LI>clean the string and extract the jar names
	 * </OL>
	 * 
	 * @param inF The file to read the list of jars from 
	 */
	private void generateListOfJars(File inF) throws Exception {
		if (inF==null || !inF.canRead()) {
			throw new IllegalArgumentException("Invalid file");
		}
		// The reader of the file
		BufferedReader reader = new BufferedReader(new FileReader(inF));
		// The string containing all the requested jars
		StringBuilder builder = new StringBuilder();
		
		// Read all the lines until find the propertyPattern
		String line=null;
		boolean found = false;
		while ((line=reader.readLine())!=null) {
			if (line.isEmpty() || line.indexOf("=")==-1) {
				// Skip empty lines
				continue;
			}
			String str=line.substring(0, line.indexOf("=")+1);
			Matcher m = propertyPattern.matcher(str);
			if (m.matches()) {
				found=true;
				break;
			}
		}
		if (!found) {
			throw new Exception(propertyPattern.pattern()+" not found");
		}
		// The first line starts with the name of the property
		Pattern p = Pattern.compile(".*\\\\s*");
		
		// Clean line
		String[] temp = propertyPattern.split(line);
		System.out.println(temp.length);
		for (String s: temp) {
			System.out.println(s);
		}
		line=temp[1];
		
		// The following loop matches all the lines ending with ,\
		// i.e. the last line with a valid jar of build.properties
		// is not caught by the loop
		while (p.matcher(line).matches()) {
			// The line ends with ",\"
			line = line.trim();
			line = line.replace('\\', ' ');
			builder.append(line.trim());
			line=reader.readLine();
		}
		builder.append(line.trim());
		System.out.println("FOUND: "+builder.toString()+"\n");
		
		// Some cleaning
		String str= builder.toString().replaceAll("  ", " ");
		while (!str.replaceAll("  ", " ").equals(str)) {
			str=str.replaceAll("  ", " ");
		}
		
		// Extract the jars and put their names in the classpathEntries vector
		String[] pkgs= str.split(",");
		for (String jar: pkgs) {
			System.out.println("\t>> ["+jar+"]");
			classpathEntries.add(jar);
		}
		System.out.println(classpathEntries.size());
		removeDuplicates(classpathEntries);
		
	}
	
	/**
	 * Get the names of the jars in the passed string String
	 * <P>
	 * The names of the jars are intended as written in the <code>fileOfProperties</code>
	 * file like for example <code>platform:/plugin/ACS_Utility_Jars/jacorb.jar</code>.
	 * <P>
	 * A line can contain a comma separated list of jars
	 * 
	 * @param str The string containing a jar  
	 * @return The names of the jars or
	 * 				<code>null</code> if the string does not contain a jar 
	 */
	private String getJarNames(String str) {
		if (str==null || str.isEmpty()) {
			return null;
		}
		str=str.trim();
		System.out.println("Checking ["+str+"]");
		// Check if the line is that containing the name of the property
		if (str.indexOf("=")!=-1) {
			String[] items = propertyPattern.split(str,1);
			System.out.println(items.length);
			for (String temp: items) {
				System.out.println("\t"+temp);
			}
			str= items[0].trim();
		}
		// Clean the string by removing any extra char at the end (",\" for instance)
		if (str.endsWith("\\")) {
			str = str.substring(0,str.length()-1).trim();
		}
		return str;
	}
	
	/**
	 * Remove duplicated entries from the vector.
	 * <P>
	 * Given that the order of the entries is important, in case
	 * the same entry appears more then once, the last one will be
	 * removed.
	 * 
	 * @param v
	 */
	private void removeDuplicates(Vector<String> v) {
		if (v==null || v.size()==0) {
			return;
		}
		for (int t=0; t<v.size(); t++) {
			int pos;
			do {
				pos = v.lastIndexOf(v.get(t));
				if (pos>=0 && pos>t) {
					v.remove(pos);
				}
			}
			while (pos>t &&  pos>=0);
		}
	}
	
	/**
	 * Return the name of the jars in the <code>build.properties</code>
	 * like for example <code>acscommon.jar</code>
	 * 
	 * @return The name of the jars
	 */
	public String[] getJars() throws Exception {
		String[] ret = new String[classpathEntries.size()];
		for (int t=0; t<classpathEntries.size(); t++) {
			String fullJarName=classpathEntries.get(t);
			if (!fullJarName.toLowerCase().endsWith(".jar")) {
				throw new Exception("Wrong jar file name in "+fileOfProperties+": "+fullJarName);
			}
			fullJarName=fullJarName.trim();
			String str[]=fullJarName.split("/");
			ret[t]=str[str.length-1];
		}
		return ret;
	}
}
