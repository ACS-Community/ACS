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
package alma.acs.eclipse.utils.jar;

import java.io.File;
import java.io.FilenameFilter;
import java.util.Collection;
import java.util.Vector;

/**
 * {@link JarFolder} holds a folder of jar files allowing to perform the
 * most common operation.
 * 
 * @author acaproni
 *
 */
public class JarFolder {
	
	/**
	 * A filter to get on the jar files in the folder
	 * @author almadev
	 *
	 */
	public class JarFileNameFilter implements FilenameFilter {

		/** 
		 * The filtering is based on the extension of the name
		 * 
		 * @see java.io.FilenameFilter#accept(java.io.File, java.lang.String)
		 */
		@Override
		public boolean accept(File dir, String name) {
			if (name==null || name.trim().isEmpty()) {
				return false;
			}
			return name.trim().toLowerCase().endsWith(".jar");
		}
		
	}
	
	/**
	 * A data structure representing the classes of a jar file.
	 * 
	 * @author acaproni
	 *
	 */
	public class JarClasses {
		/**
		 * The name of the jar
		 */
		public final String jarName;
		
		/**
		 * The vector of class names
		 */
		public Collection<String> classes;
		
		/**
		 * Constructor 
		 * 
		 * @param jar The name of the jar (for example <code>lc.jar</code>)
		 */
		public JarClasses(String jar, Collection<String> classnames) {
			if (jar==null || jar.isEmpty()) {
				throw new IllegalArgumentException("Invalid jar name");
			}
			if (classnames== null || classnames.isEmpty()) {
				throw new IllegalArgumentException("No classes in "+jar);
			}
			jarName=jar;
			classes=classnames;
			System.out.println(jarName+classnames.size());
		}
	}
	
	/**
	 * The folder containing jar files
	 */
	private final File jarFolder; 
	
	public JarFolder(File folder) {
		if (folder==null || !folder.canRead()) {
			throw new IllegalArgumentException("Null or unreadable folder of jars");
		}
		this.jarFolder=folder;
	}
	
	/**
	 * @return the list of the jar files in the folder
	 */
	public File[] getJarFiles() {
		return jarFolder.listFiles(new JarFileNameFilter());
	}
	
	/**
	 * Return the list of jar names in folder. A jar name can be for
	 * example <code>lc.jar</code>.
	 * 
	 * @return The list of jars in the folder
	 */
	public String[] getJars() {
		File[] jars=getJarFiles();
		if (jars==null || jars.length==0) {
			return new String[0];
		}
		String[] ret = new String[jars.length];
		for (int t=0; t<jars.length; t++) {
			ret[t] = jars[t].getName();
		}
		return ret;
	}
	
	/**
	 * Get the {@link File} of the jar with the given name.
	 * 
	 * @param name The name of the jar like for example <code>lc.jar</code>
	 * @return The File of the jar or <code>null</code> if the jar does not exist 
	 * 		in the folder
	 */
	public File getJar(String name) {
		if (name==null || name.isEmpty()) {
			throw new IllegalArgumentException("Invalid name of jar: "+name);
		}
		File[] jars=getJarFiles();
		if (jars==null) {
			return null;
		}
		for (File jar: jars) {
			if (jar.getName().equals(name)) {
				return jar;
			}
		}
		return null;
	}
	
	/**
	 * Check if the folder contains a given jar file.
	 * 
	 * @param jarname A jar file like <code>lc.jar</code>
	 * @return <code>true</code> if the folder contains the passed jar
	 */
	public boolean containsJar(String jarname) {
		if (jarname==null) {
			throw new IllegalArgumentException("The jar name can't be null");
		}
		if (!jarname.toLowerCase().endsWith(".jar") || jarname.indexOf('/')>=0) {
			throw new IllegalArgumentException("Invalid jar name: "+jarname);
		}
		File[] jars = getJarFiles();
		for (File f: jars) {
			if (f.getName().equals(jarname)) {
				return true;
			}
		}
		return false;
	}
	
	/**
	 * Return a list of jar files containing a given class.
	 * <P>
	 * The class name can be the whole name of a class or the beginning of its name.
	 * For example if <code>javaClass</code> is <code>LoggingClient</code>, this class
	 * returns both <code>LoggingClient</code> and <code>LoggingClientText</code> from 
	 * <code>lc.jar</code>. 
	 * 
	 * @param javaClassName The name of a class or a the beginning of its name
	 * @return A list of jar names containing the (partial) name of the class or <code>null</code>
	 * 			if no classes in the jars match with the passed partial name
	 */
	public Collection<JarClasses> getJarsContainingClass(String javaClass) throws Exception {
		Vector<JarClasses> ret=null;
		File[] jars = getJarFiles();
		for (File jar: jars) {
			JarFileHelper jarHelper = new JarFileHelper(jar);
			Collection<String> classNames = jarHelper.getMatchingClasses(javaClass);
			if (classNames==null || classNames.isEmpty()) {
				continue;
			}
			if (ret==null) {
				ret = new Vector<JarClasses>();
			}
			JarClasses jarClasses = new JarClasses(jarHelper.getName(),classNames);
			ret.add(jarClasses);
		}
		return ret;
	}
	
	/**
	 * Return the absolute path of this folder of jars.
	 * 
	 * @return
	 */
	public String getAbsolutePath() {
		return jarFolder.getAbsolutePath();
	}
	
}
