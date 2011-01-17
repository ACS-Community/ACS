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
import java.io.IOException;
import java.util.Collection;
import java.util.Enumeration;
import java.util.Vector;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.logging.Logger;

import alma.acs.makesupport.AcsJarFileFinder;

/**
 * Objects from this class allow to browse the content of a jar file focusing 
 * on classes and packages.
 * <P>
 * There are 2 ways to use <code>JarFileHelper</code> objects:
 * <OL>
 * 	<LI>from the name of a jar like for example <code>lc.jar</code>: in this case
 * 		the jar file is retrieved by following ACS rules i.e. ../lib, INROOT/lib...
 * 	<LI>by passing directly the jar file like for example 
 * 		<code>$INTROOT/lib/lc.jar</code>
 * </OL>
 * <P>  
 * Given the name of a jar file, <code>JarFile</code> get the right jar file from the
 * file system looking the folder as ACS rules.
 * The content of thSe jar file is then examined to get the list of the java classes and packages 
 * it contains as well as any other info needed.
 * <P>
 * <b>Note</b>: this class is intended to work only with ACS distributed jar files.
 * 
 * @author acaproni
 *
 */
public class JarFileHelper {
	
	/**
	 * When adding a package in {@link JarFileHelper#addJavaPackage(String, Collection)} a package
	 * is added to the vector only if it was not already there.
	 * 
	 * We need to know in the returned value of that method if what happened with a package
	 * because we want to send a log if a jar file does not contain any package because it be an error.
	 * <i>NOTE</i> that it is different to say that a jar does not contain any package (an error) or that the package
	 * 	 has not been added to the vector because already present (not an error).
	 *      
	 * @author acaproni
	 *
	 */
	private enum AddPackageReturnType {
		NO_PACKAGE,
		PKG_ALREADY_IN_VECTOR,
		PKG_ADDED
	};
	
	/**
	 * The filter is used by {@link AcsJarFileFinder} to look for more then one
	 * jar in the same time.
	 * <P>
	 * In our case the filter will allow accessing only the file whose name
	 * is the name of the jar.
	 * 
	 * @author acaproni
	 *
	 */
	class JarFileNameFilter implements FilenameFilter {
		
		/**
		 * The name of the jar file to look for 
		 * (for example <code>lc.jar</code>)
		 */
		private final String name;
		
		/**
		 * Constructor
		 * 
		 * @param name The name of the jar file
		 */
		public JarFileNameFilter(String name) {
			if (name==null || name.isEmpty()) {
				throw new IllegalArgumentException("Invalid jar file name file");
			}
			if (!name.toLowerCase().endsWith(".jar")) {
				System.out.println("warning: suspicious name of a jar file "+name);
			}
			this.name=name;
		}

		/**
		 * @see java.io.FilenameFilter#accept(java.io.File, java.lang.String)
		 */
		@Override
		public boolean accept(File dir, String name) {
			return name.equals(this.name);
		}
		
	}

	/**
	 * The name of the jar file like for example <code>lc.jar</code>
	 */
	private final String name;
	
	/**
	 * The jar file.
	 * <P>
	 * This file is obtained by scanning ACS folder like INTROOT, ACSROOT
	 * and so on 
	 */
	private final File file; 
	
	/**
	 * The logger
	 */
	private static final Logger logger = Logger.getAnonymousLogger();
	
	/**
	 * Constructor.
	 * 
	 * @param name The name of the ACS jar file (like <code>lc.jar</code>)
	 * @param folders The folders to look for the jar files
	 * @throws Exception 
	 */
	public JarFileHelper(String name, String[] folders) throws Exception {
		if (name==null || name.isEmpty()) {
			throw new IllegalArgumentException("Invalid jar file name");
		}
		if (folders==null || folders.length==0) {
			throw new IllegalArgumentException("Invalid folders");
		}
		this.name=name;
		File[] dirs = new File[folders.length];
		for (int t=0; t<folders.length; t++) {
			dirs[t] = new File(folders[t]);
			if (!dirs[t].canRead()) {
				throw new Exception("Unreadable folder "+folders[t]);
			}
		}
		AcsJarFileFinder jarFileFinder= new AcsJarFileFinder(dirs,logger);
		File[] files = jarFileFinder.getFiles(new JarFileNameFilter(name));
		if (files==null || files.length==0) {
			throw new Exception(name+" not found");
		}
		if (files.length>1) {
			System.out.println("Warning; found more then one file for "+name);
			for (File f: files) {
				System.out.println("\t"+f.getAbsolutePath());
			}
			System.out.println("\tusing the first one: "+files[0]);
		}
		file=files[0];
		// Ensure the jar is readable
		if (!file.canRead()) {
			throw new Exception(file.getAbsolutePath()+" unreadable");
		}
	}
	
	/**
	 * Constructor
	 * 
	 * @param file The jar file
	 */
	public JarFileHelper(File file) throws Exception {
		if (file==null || !file.canRead()) {
			throw new IllegalArgumentException("Invalid/unreadable file");
		}
		this.file=file;
		this.name=file.getName();
	}
	
	/**
	 * Flush the java packages contained in this jar file
	 * into the passed vector.
	 * 
	 * @return the number of java packages in the jar file
	 * @throws IOException In case of error reading the jar file
	 */
	public int getPackages(Collection<String> javaPackages) throws IOException {
		if (javaPackages==null) {
			throw new IllegalArgumentException("The vector can't be null");
		}
		AddPackageReturnType ret;
		int sz=0;
		JarFile jar = new JarFile(file);
		Enumeration<JarEntry> entries = jar.entries();
	    for (Enumeration<JarEntry> em1 = entries; em1.hasMoreElements();) {
	        String str=em1.nextElement().toString().trim();
	        ret=addJavaPackage(str,javaPackages);
	        if (ret!=AddPackageReturnType.NO_PACKAGE) {
	        	sz++;
	        }
	    }
	    return sz;
	}
	
	/**
	 * Add a java package to the vector if it is not already there.
	 * <P>
	 * The package name is calculated by checking the passed string
	 * 
	 * @param str The entry of the jar that could point to a jar
	 * @param vector The vector of packages where the package must be added
	 * @return <code>true</code> if the package has been added to the vector;
	 * 		<code>false</code> otherwise
	 * 
	 */
	private AddPackageReturnType addJavaPackage(String str, Collection<String> vector) {
		if (vector==null) {
			throw new IllegalArgumentException("The vector can't be null");
		}
		if (str==null || str.isEmpty()) {
			return AddPackageReturnType.NO_PACKAGE;
		}
		if (!str.toLowerCase().endsWith(".class")) {
        	return AddPackageReturnType.NO_PACKAGE;
        }
		int pos = str.lastIndexOf("/");
		if (pos>0) {
			str=str.substring(0,pos);
		}
		str=str.replaceAll("/", ".");
		if (!vector.contains(str)) {
			vector.add(str);
			return AddPackageReturnType.PKG_ADDED;
		} else {
			return AddPackageReturnType.PKG_ALREADY_IN_VECTOR;
		}
	}
	
	/**
	 * Flush the java classes contained in this jar file
	 * into the passed vector.
	 * 
	 * @return The java classes in the jar file
	 */
	public void getClasses(Collection<String> javaClasses) throws IOException {
		if (javaClasses==null) {
			throw new IllegalArgumentException("The vector can't be null");
		}
		JarFile jar = new JarFile(file);
		Enumeration<JarEntry> entries = jar.entries();
	      for (Enumeration<JarEntry> em1 = entries; em1.hasMoreElements();) {
	        String str=em1.nextElement().toString().trim();
	        addJavaClass(str,javaClasses);
	      }
	}
	
	/**
	 * Check if a class with the given name is in the jar.
	 * The match is not done for the whole name of the class
	 * but from its initial part.
	 * For example if the parameter is Logging and the jar is <code>lc.jar</code> then
	 * the returned array contains LoggingClient and LoggingClientText 
	 * 
	 * @param name The beginning of the name of the class
	 * @return A list of class names matching the parameter or <code>null</code> 
	 * 			if no class name in the jar matches with the parameter
	 * @throws IOException In case of error reading the jar file
	 */
	public Collection<String> getMatchingClasses(String name) throws IOException {
		// All the java classes in the jar
		Vector<String> classes = new Vector<String>();
		Vector<String> matchingClassesVector=null;
		getClasses(classes);
		for (String javaClass: classes) {
			// javaClass can be com.cosylab.logging.LoggingClient.class
			String[] parts = javaClass.split("\\.");
			if (parts.length<2) {
				continue;
			}
			if (parts[parts.length-2].startsWith(name)) {
				if (matchingClassesVector==null) {
					matchingClassesVector= new Vector<String>();
				}
				matchingClassesVector.add(javaClass);
			}
		}
		return matchingClassesVector;
	}
	
	/**
	 * Add a java class to the vector if it is not already there.
	 * <P>
	 * The class name is calculated by checking the passed string.
	 * 
	 * @param str The entry of the jar that could point to a java class
	 * @param vector The vector of classes where the package must be added
	 * 
	 */
	private void addJavaClass(String str, Collection<String> vector) {
		if (vector==null) {
			throw new IllegalArgumentException("The vector can't be null");
		}
		if (str==null || str.isEmpty()) {
			return;
		}
		if (!str.toLowerCase().endsWith(".class")) {
        	return;
        }
		str=str.replaceAll("/", ".");
		if (!vector.contains(str)) {
			vector.add(str);
		}
	}
	
	/**
	 * @return the absolute path of the file of the jar
	 */
	public String getFilePathName() {
		return file.getAbsolutePath();
	}

	/**
	 * @return the name
	 */
	public String getName() {
		return name;
	}
}
