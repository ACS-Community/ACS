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

import java.io.File;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

import alma.acs.makesupport.AcsJarFileFinder;

/**
 * Custom class loader which correctly handles multiple occurences of identical jar files in 
 * "overlayed" directories. These directories would typically be ../lib, INTROOT/lib, ACSROOT/lib and ORB dirs.
 * <p>
 * The main purpose is to cut down the very long classpath produced otherwise by the ACS start scripts.
 * Only the jar file that contains this class loader must be supplied to the JVM using the "-classpath" option. 
 * The JVM must be started with property definitions similar to:
 * <ul>
 * <li><code>-Djava.system.class.loader=alma.acs.classloading.AcsSystemClassLoader</code>.
 * <li><code>-Dacs.system.classpath.jardirs=../lib/:$INTROOT/lib:$ACSROOT/lib</code>. 
 * 		If an operating system has a native style path separator different from "<code>:</code>", it can optionally be used instead.
 * </ul>
 * Notes on the classloader hierarchies:
 * <ul>
 * <li>Don't confuse the inheritance hierarchy and the delegation hierarchy.
 *     Both this CL as well as its delegation-parent inherit from <code>URLClassLoader</code>.
 * <li>This classloader is meant to be registered with the JVM using the property <code>java.system.class.loader</code>.
 * <li>Instead of using the real system classloader (<code>sun.misc.Launcher$AppClassLoader</code>) as the parent, 
 *     since ACS 6.0 this CL takes over the functionality of the direct parent, and uses the original grandparent CL for parent-delegation.
 *     Circumventing the original parent CL after copying its CLASSPATH entries, removes the distinction between the CLASSPATH entries
 *     and the jar files from the <code>acs.system.classpath.jardirs</code> directories.
 *     <ul>
 *     <li>The effect is that we can add jar files to the CLASSPATH, which use classes from the normal pool of jar files.
 *         One use case for this is testing component implementation classes outside of containers: the jar files under
 *         <code>../lib/ACScomponents</code> are not picked up by the classloader unless we add them to the CLASSPATH, 
 *         e.g. using <code>acsStartJava -addToClasspath</code>.
 *     <li>The CL hierarchy becomes (1) Bootstrap CL, (2) Extention CL, (3) AcsSystemClassLoader, [(4) optional component CL etc]
 *     </ul>
 * <li>The method {@link java.lang.ClassLoader#getSystemClassLoader()} "tricks" the caller and returns this class loader,
 *     even though it thinks that the real system CL is our circumvented sibling, the <code>sun.misc.Launcher$AppClassLoader</code>.
 *     This allows JUnit tests run, since the JUnit framework sets up their own classloader to be a child
 *     of the system classloader, shortcutting any "regular" child class loaders in between. 
 * </ul>
 * 
 * @author hsommer created Sep 13, 2004 6:31:32 PM
 */
public class AcsSystemClassLoader extends URLClassLoader
{
	public static final String PROPERTY_JARDIRS = "acs.system.classpath.jardirs";
	public static final String PROPERTY_TOPJARSONLY = "acs.system.classpath.topjarsonly";
	public static final String PROPERTY_CLASSLOADERVERBOSE = "acs.system.classloader.verbose";

	private boolean verbose;
	private final JarOrderOptimizer optimizer;
	private final URLClassLoader appClassLoader;
	
	/**
	 * Constructor will be called by the JVM.
	 * The standard system class loader (instance of <code>sun.misc.Launcher$AppClassLoader</code> 
	 * is expected to be the <code>parent</code> parameter, although no such assumption is made in the code.
	 * <p>
	 * In the ctor, the directories given in <code>acs.system.classpath.jardirs</code> are searched for JAR files,
	 * which are fed as URLs into the classpath maintained by the <code>URLClassLoader</code> base class.
	 * 
	 * @see AcsJarFileFinder
	 */
	public AcsSystemClassLoader(ClassLoader parent)
	{
		super(new URL[0], parent.getParent());
		
		if (!(parent instanceof URLClassLoader)) {
			throw new IllegalStateException("AcsSystemClassLoader expects parent CL of type URLClassLoader!");
		}
		appClassLoader = (URLClassLoader) parent;
		verbose = Boolean.getBoolean(PROPERTY_CLASSLOADERVERBOSE);
				
		List<File> jarFileList = new ArrayList<File>();
		optimizer = new JarOrderOptimizer(verbose);
		
		String jarDirPath = System.getProperty(PROPERTY_JARDIRS);
		if (jarDirPath != null) {
			File[] jarDirs = parseJarDirs(jarDirPath);
	
			// extract jar files from directories
			AcsJarFileFinder jarFinder = new AcsJarFileFinder(jarDirs, null);
			jarFileList.addAll(Arrays.asList(jarFinder.getAllFiles()));
		
			// test mode 
			if (Boolean.getBoolean(PROPERTY_TOPJARSONLY)) {
				System.out.println("AcsSystemClassLoader running in test mode: will only use jar files from priority list.");
				jarFileList = optimizer.getTopJarsOnly(jarFileList);
			}
			
			// sort the jar files
			optimizer.sortJars(jarFileList); 
		}
		else {
			System.err.println("No jar path given in property " + PROPERTY_JARDIRS + "! If that is intended, you should not use AcsSystemClassLoader at all!");
			// Still we go on, acting just like the normal system class loader.
		}
		
		// we prepend the CLASSPATH entries so that this CL can replace the normal JVM application CL 
		prependCLASSPATHJars(jarFileList);

		// add the jars and directories 
		for (Iterator<File> iter = jarFileList.iterator(); iter.hasNext();) {
			File cpEntry = iter.next();
			try {
				addURL(cpEntry.toURI().toURL());
				if (verbose) {
					System.out.println("added " + cpEntry.getAbsolutePath() + " to the custom system class loader's path.");
				}
			}
			catch (Exception e) {
				System.err.println("failed to add " + cpEntry.getAbsolutePath() + " to the custom class loader's path." + e.toString());
			}
		}
	}

	private File[] parseJarDirs(String jarDirPath)
	{
		StringTokenizer tok = new StringTokenizer(jarDirPath, File.pathSeparator);
		File[] jarDirs = new File[tok.countTokens()];
		for (int i = 0; tok.hasMoreTokens(); i++) {
			jarDirs[i] = new File(tok.nextToken());
		}
		return jarDirs;
	}
	
	/**
	 * Extracts the CLASSPATH jar files (or directories) from the parent class loader 
	 * and adds them at the beginning of the given list.
	 * <p>
	 * It is assumed that thanks to the <code>java.system.class.loader</code> property, 
	 * this AcsSystemClassLoader is a direct child of the application system class loader 
	 * which normally would handle the CLASSPATH.
	 */
	private void prependCLASSPATHJars(List<File> jarFileList) {
		if (verbose) {
			System.out.print("Will prepend the following entries from the CLASSPATH to AcsSystemClassLoader's entries: ");
		}
		URL[] sysCP = appClassLoader.getURLs();
		List<File> sysCPList = new ArrayList<File>();
		for (int i = 0; i < sysCP.length; i++) {
			try {
				File entry = new File(sysCP[i].toURI());
				sysCPList.add(entry);
				if (verbose) {
					System.out.print(entry.getAbsolutePath() + ", ");
				}
			} catch (URISyntaxException e) {
				System.err.println("Failed to add CLASSPATH entry '" + sysCP[i] + "' to AcsSystemClassLoader: " + e.toString());
			}
		}
		if (verbose) {
			System.out.println();
		}
		jarFileList.addAll(0, sysCPList);
	}
	

	
	protected synchronized Class<?> loadClass(String name, boolean resolve)
			throws ClassNotFoundException
	{
		if (verbose) {
			if (ClassLoader.getSystemClassLoader() != this) {
				System.err.println("Warning: AcsSystemClassLoader is not registered as the JVM's system class loader!");
			}
		}
		
		// First, check if the class has already been loaded by this classloader
		Class c = findLoadedClass(name);
		if (c == null) {
			// don't delegate to parent class loader for classes from this module (this jar must be on the system class path!)
			// The shortcutting will also be a tiny performance improvement since all "alma.acs.xyz" classes even from other jar files
			// are supposed to be loaded through this class loader, in which case the parent would only delegate back to us anyway.
			if (name.startsWith("alma.acs.") || name.startsWith("com.cosylab.util") ) { 
				try {
					c = findClass(name);
				}
				catch (ClassNotFoundException e) {
					// fallthrough: try parent class loader after all;
					// perhaps some jar files with ACS classes were put on the system class path 
				}
			}
			if (c == null) {
				// The super implementation will delegate to the parent class loader, and only call findClass(name) if required.
				// This is the default for all well-behaved class loaders: first try parent, then self
				c = super.loadClass(name, false);
			}
		}
		if (resolve) {
			resolveClass(c);
		}
		return c;
	}

	/**
	 * Delegates to base class implementation.
	 * <p>
	 * Discards requests to load classes from certain sub-packages of <code>sun.</code> or <code>com.sun.</code> 
	 * by throwing a <code>ClassNotFoundException</code>. 
	 * The JDK libs try to search (e.g. during ACS container startup) for some optional system classes such as 
	 * <code>sun.text.resources.DateFormatZoneData_en_US</code> or <code>sun.util.logging.resources.logging_en</code>. 
	 * We know that ALMA jar files do not contain any classes in those packages, and can thus speed up the applications,
	 * in cases where those classes have already been searched unsuccessfully by any parent class loaders,
	 * which as the last step desparately would ask this class loader, which would cause a search through <b>all</b> jar files.
	 * sun.awt.resources.
	 * com.sun.swing.internal.plaf.     
	 */
	protected Class<?> findClass(String name) throws ClassNotFoundException
	{
		if (optimizer.isClassKnownToBeUnavailable(name)) {
			throw new ClassNotFoundException("ACS and application software are not supposed to deliver a class like '" 
					+ name + "' and therefore AcsSystemClassLoader won't attempt to load it (for IO optimization).");
		}
		Class<?> clazz = null;
		try {
//			System.out.println("****AcsSystemClassLoader will try to find class " + name);
			clazz = super.findClass(name);
		}
		catch (ClassNotFoundException e) {
			if (verbose) {
				System.err.println(AcsSystemClassLoader.class.getName() + " failed to load class " + name);
				e.printStackTrace();
			}
			throw e;
		}
		return clazz;
	}

	/**
	 * This method, even though private, allows jconsole and possibly similar profilers and other tools 
	 * to add their required jar files, 
	 * see {@linkplain java.lang.instrument.Instrumentation#appendToSystemClassLoaderSearch(java.util.jar.JarFile)}.
	 * <p>
	 * For example, jconsole would currently call this method with argument 
	 * <code>/usr/java/jdk1.6.0_02/jre/lib/management-agent.jar </code>.
	 * 
	 * @since ACS 7.0
	 */
	private void appendToClassPathForInstrumentation(String jarPathName) {
		File jarFile = new File(jarPathName);
		try {
			addURL(jarFile.toURI().toURL());
			System.out.println("AcsSystemClassLoader#appendToClassPathForInstrumentation called with jar file " + jarPathName);
		} catch (MalformedURLException ex) {
			System.err.println("AcsSystemClassLoader#appendToClassPathForInstrumentation failed with MalformedURLException for jar file " + jarPathName);
		}
	}
}