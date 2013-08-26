/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.makesupport;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

public abstract class AbstractJarEntryExtractor {

	/**
	 * prefix paths inside JAR files which are not considered part of Java package paths
	 */
	public static final String[] PREFIX_PATHS = new String[] {"src", "test"};
	
	public final String FILETYPE;

	public AbstractJarEntryExtractor(String filetype) {
		super();
		FILETYPE = filetype;
	}

	/**
	 * Gets the Java class name from a JarEntry.
	 * 
	 * Uses {@link #PREFIX_PATHS} to remove a leading prefix path
	 * that is not part of the Java package. 
	 *
	 * @param javaEntry
	 * @return the class name
	 */
	protected String getClassName(JarEntry javaEntry) {
		String className = javaEntry.getName();
		for (int j = 0; j < PREFIX_PATHS.length; j++)
		{
			if (className.startsWith(PREFIX_PATHS[j]))
			{
				className = className.substring(PREFIX_PATHS[j].length());
				break;
			}
		}
		if (className.startsWith("/")) {
			className = className.substring(1);
		}
		return className;
	}

	/**
	 * Lists all Java files files of type FILETYPE that are contained inside a given JAR file.
	 * 
	 * The current implementation only looks for a FILETYPE file ending, 
	 * ignoring the contents of such files.
	 * 
	 * @param jarfile the JAR file to be searched
	 * @return  entries that are Java files of type FILETYPE (array != null, possibly empty)
	 */
	public JarEntry[] getJavaEntries(JarFile jarfile) {
		List<JarEntry> javaEntries = new ArrayList<JarEntry>();
		
		Enumeration<JarEntry> jarEntries = jarfile.entries();
		
		while (jarEntries.hasMoreElements())
		{
			JarEntry entry = (JarEntry) jarEntries.nextElement();
			if (entry.getName().endsWith(FILETYPE))
			{
				javaEntries.add(entry);
			}
		}		
		
		return (JarEntry[]) javaEntries.toArray(new JarEntry[javaEntries.size()]);
	}

}