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
import java.net.URL;
import java.net.URLClassLoader;
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
 * Note that by setting the property <code>java.system.class.loader</code> to this class loader,
 * the method {@link java.lang.ClassLoader#getSystemClassLoader()} "tricks" the caller and returns an instance of this class loader
 * rather than its parent (<code>sun.misc.Launcher$AppClassLoader</code>) which it otherwise would.
 * This is neccessary to make JUnit tests run, since the JUnit framework sets up their own classloader to be a child
 * of the system classloader, shortcutting any "regular" child class loaders. 
 * 
 * @author hsommer created Sep 13, 2004 6:31:32 PM
 */
public class AcsSystemClassLoader extends URLClassLoader
{
	public static final String PROPERTY_JARDIRS = "acs.system.classpath.jardirs";
	public static final String PROPERTY_TOPJARSONLY = "acs.system.classpath.topjarsonly";
	public static final String PROPERTY_CLASSLOADERVERBOSE = "acs.system.classloader.verbose";

	private boolean verbose;
	private JarOrderOptimizer optimizer;
	
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
		super(new URL[0], parent);

		verbose = Boolean.getBoolean(PROPERTY_CLASSLOADERVERBOSE);
		
		String jarDirPath = System.getProperty(PROPERTY_JARDIRS);
		File[] jarDirs = parseJarDirs(jarDirPath);

		AcsJarFileFinder jarFinder = new AcsJarFileFinder(jarDirs, null);
		File[] allJars = jarFinder.getAllFiles();

		optimizer = new JarOrderOptimizer(verbose);

		if (Boolean.getBoolean(PROPERTY_TOPJARSONLY)) {
			System.out.println("AcsSystemClassLoader running in test mode: will only use jar files from priority list.");
			allJars = optimizer.getTopJarsOnly(allJars);
		}
		optimizer.sortJars(allJars); 
		
		for (int i = 0; i < allJars.length; i++) {
			try {
				addURL(allJars[i].toURL());
				if (verbose) {
					System.out.println("added " + allJars[i].getAbsolutePath() + " to the custom system class loader's path.");
				}
			}
			catch (Exception e) {
				System.err.println("failed to add " + allJars[i].getAbsolutePath() + " to the custom class loader's path." + e.toString());
			}
		}

		if (verbose) {		
//			//test 
//			URL[] urls = getURLs();
//			System.out.println("first URL is " + urls[0].toString());
//			System.out.println("last URL is " + urls[urls.length-1].toString());
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
	
	
	protected synchronized Class loadClass(String name, boolean resolve)
			throws ClassNotFoundException
	{
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
	protected Class findClass(String name) throws ClassNotFoundException
	{
		if (optimizer.isClassKnownToBeUnavailable(name)) {
			throw new ClassNotFoundException("ACS and application software are not supposed to deliver a class like '" 
					+ name + "' and therefore AcsSystemClassLoader won't attempt to load it (for IO optimization).");
		}
		Class clazz = null;
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

}