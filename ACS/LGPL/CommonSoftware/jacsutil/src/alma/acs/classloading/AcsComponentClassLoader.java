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
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.StringTokenizer;
import java.util.logging.Level;
import java.util.logging.Logger;

// import sun.misc.ClassLoaderUtil;

import alma.acs.makesupport.AcsJarFileFinder;

/**
 * The component-specific class loader.
 * Attempts to load classes directly, and only delegates to parent class loader after it failed.
 * This bottom-up direction of classloading in the classloader hierarchy follows the J2EE convention,
 * and thus violates the normal J2SE top-down direction.
 * <p>
 * This class requires the directories that contain component impl jars to be specified in the <code>acs.components.classpath.jardirs</code>
 * property. 
 * The startup scripts must set this property.
 * Other jar files (e.g. ACS jars) must be in different directories than those given by this property.  
 * <p>
 * TODO-: this class has a few things in common with {@link alma.acs.classloading.AcsSystemClassLoader}, so
 * perhaps during some future refactoring a common base class could be extracted (between URLClassLoader and these).
 */
public class AcsComponentClassLoader extends URLClassLoader
{
	/**
	 * Name of the property that defines the directories for component implementation jar files.
	 * Example value: <code>../lib/ACScomponents:/alma/ACS-5.0/ACSSW/lib/ACScomponents:"</code>.
	 */
	public static final String PROPERTY_JARDIRS = "acs.components.classpath.jardirs";
		
	/**
	 * Name of the property that flags verbose mode of the component classloader. 
	 * Verbose mode is a debugging tool, only to be enabled locally by defining this property.
	 * <p>
	 * If enabled to be verbose, a component classloader will log the jar files it works with,
	 * the classes it loads or fails to load, and also prints a message when it is getting finalized.
	 * The latter is useful to monitor component class unloading. 
	 */
	public static final String PROPERTY_CLASSLOADERVERBOSE = "acs.components.classloader.verbose";

	private boolean verbose;
	private final Logger logger;
	private final String componentName;

	private ClassLoaderUtil classLoaderUtil;

    /**
     * @param parent  parent class loader (currently the container class loader)
     * @param logger  the container logger, for debug output (see <code>PROPERTY_CLASSLOADERVERBOSE</code>). 
     *                  This is also used to derive the processName when logging exceptions.
     * @param componentName  used for log messages in verbose mode 
     */
    public AcsComponentClassLoader(ClassLoader parent, Logger logger, String componentName)
	{
		super(new URL[0], parent);
		
		try {
			classLoaderUtil = new ClassLoaderUtil(logger);
		} catch (Exception ex) {
			// @TODO (HSO): or should we simply fail with an exception?
			logger.log(Level.SEVERE, "Component class loader for " + componentName + " will work without " + ClassLoaderUtil.class.getName() +
					" which may cause JVM native memory problems.", ex);
		}

		verbose = Boolean.getBoolean(PROPERTY_CLASSLOADERVERBOSE);
		this.logger = logger;
		this.componentName = componentName;
			
		String jarDirPath = System.getProperty(PROPERTY_JARDIRS);
		if (verbose) {
			logger.fine("Property '" + PROPERTY_JARDIRS + "' is set to " + jarDirPath);
		}
//System.out.println("Property '" + PROPERTY_JARDIRS + "' is set to " + jarDirPath);
		
		File[] jarDirs = parseJarDirs(jarDirPath);

		AcsJarFileFinder jarFinder = new AcsJarFileFinder(jarDirs, null);
		jarFinder.setVerbose(verbose);
		File[] allJars = jarFinder.getAllFiles();
		
		for (int i = 0; i < allJars.length; i++) {
			try {
				addURL(allJars[i].toURL());
				if (verbose) {
		        	logger.finer("added " + allJars[i].getAbsolutePath() + 
		        			" to the path of the component classloader for " + componentName);
				}
			}
			catch (Exception e) {
				logger.log(Level.WARNING, "failed to add " + allJars[i].getAbsolutePath() + 
					" to the path of the component classloader for " + componentName, e);
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
			if (verbose) {
				logger.finer("Classloader for component '" + componentName + "' will look for jar files in directory "
						+ jarDirs[i].getAbsolutePath());
			}
		}
		return jarDirs;
	}
	
	
	/**
	 * Attempts to load the given class, and only delegates to parent class loader if it failed.
     * This bottom-up direction of classloading in the classloader hierarchy resembles the J2EE convention,
     * and thus violates the normal J2SE top-down direction.
     * <p>
     * TODO-: check if certain system or ACS classes should be skipped and delegated upward right away.
     * This may improve performance, assuming a bunch of <code>name.startsWith("java.")</code> etc are faster than 
     * <code>findLoadedClass</code> and <code>findClass</code>.
     * 
	 * @see java.lang.ClassLoader#loadClass(java.lang.String, boolean)
	 */
	protected synchronized Class<?> loadClass(String name, boolean resolve)
			throws ClassNotFoundException
	{
//		System.out.println("### load " + name);
		// First, check if the class has already been loaded by this classloader
		Class c = findLoadedClass(name);
		if (c == null) {
			// try to load the component impl class before delegating to the parent class loader.
			try {
				c = findClass(name);
			}
			catch (ClassNotFoundException e) {
				// fallthrough: try parent class loader after all;
			}
			if (c == null) {
				// The super implementation will delegate to the parent class loader.
				// This is the default for all J2SE class loaders: first try parent, then self
				if (verbose) {
					logger.finer("AcsComponentClassLoader will delegate loading '" + name + "' to parent CL, a "
							+ getParent().getClass().getName());
				}
				c = super.loadClass(name, false);
			}
		}
		else if (verbose) {
        	logger.finer("Class '" + name + "' already loaded by AcsComponentClassLoader for '" + componentName + 
        			"'. Nothing to do.");
        }
        
		if (resolve) {
			resolveClass(c);
		}
		return c;
	}

    
	/**
	 * Calls <code>super.findClass(name)</code> and provides some System.out logging if in verbose mode.
	 * 
	 * @see java.lang.ClassLoader#findClass(java.lang.String)
	 */
	protected Class<?> findClass(String name) throws ClassNotFoundException
	{
		Class clazz = null;
		try {
			clazz = super.findClass(name);
        }
		catch (ClassNotFoundException e) {
// should be commented out even for verbose mode, since this method is called for any class in the J2EE bottom-up classloader approach
//			if (verbose) {
//				System.out.println(AcsComponentClassLoader.class.getName() + " failed to load class " + name);
//			}
			throw e;
		}
        if (verbose) {
        	logger.finer("Class loader for component '" + componentName + "' loaded class " + name);
        }
		return clazz;
	}

    protected void finalize() throws Throwable {
        if (verbose) {
          	logger.finer("Class loader for component '" + componentName + "' about to be finalized.");
        }
        super.finalize();
    }
    public String getSourceObject(){
        return componentName;
    }
    /**
     * Taken from ClientLogManager.stripKnownLoggerNamespacePrefix(). Maybe it should be nice to generalize it and put it somewhere else. 
     * Strips the prepended constants {@link #NS_CORBA}, {@link #NS_CONTAINER}, {@link #NS_COMPONENT} etc from the logger namespace.
     * This allows for a short, but possibly not unique display of the logger name.
     */
    public String getProcessName(){
            /** logger namespace for CORBA classes (ORB, POAs, etc) */
            String NS_CORBA = "alma.acs.corba";

            /** logger namespace for container classes during operation */
            String NS_CONTAINER = "alma.acs.container";

            /** parent of logger namespaces for application components */
            String NS_COMPONENT = "alma.component";
            
            String loggerName=logger.getName();
            if (loggerName != null) {
                    // try to strip off fixed prefix from logger namespace
                    try {
                            if (loggerName.startsWith(NS_COMPONENT)) {
                                    loggerName = loggerName.substring(NS_COMPONENT.length()+1);
                            }
                            else if (loggerName.startsWith(NS_CONTAINER)) {
                                    loggerName = loggerName.substring(NS_CONTAINER.length()+1);
                            }
                            else if (loggerName.startsWith(NS_CORBA)) {
                                    loggerName = loggerName.substring(NS_CORBA.length()+1);
                            }
                    } catch (Exception e) {
                            // fallback: use logger namespace
                    }
            }
            return loggerName;
    }
    
    /**
     * This method will be added to {@link URLClassLoader} in JDK 1.7. 
     * Until then, we do something similar already with JDK 1.6.
     * <p>
     * @TODO: Test / remove this once we use JDK 1.7
     * @since ACS 9.1 
     */
    public void close() throws IOException {
    	if (classLoaderUtil != null) {
    		classLoaderUtil.releaseLoader(this);
    	}
    }

}

