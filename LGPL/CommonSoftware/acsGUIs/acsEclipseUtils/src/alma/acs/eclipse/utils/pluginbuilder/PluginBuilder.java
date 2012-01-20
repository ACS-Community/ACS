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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import alma.acs.classloading.AcsSystemClassLoader;
import alma.acs.eclipse.utils.jar.AcsFolders;
import alma.acs.eclipse.utils.jar.FileHelper;
import alma.acs.util.CmdLineArgs;
import alma.acs.util.CmdLineRegisteredOption;

/**
 * Build an eclipse plugin from one or more jar files.
 * <P>
 * The required ACS jars are read from $INTROOT, $ACSROOT and so on.
 * <BR>
 * The purpose of <code>PluginBuilder</code> is to build an eclipse plugin to import ACS jars 
 * while developing RCP applications for ACS. 
 * The computation requires:
 * <UL>
 *  <LI>the list of jars to include in the plugin
 * 	<LI>the name of the plugin to build
 * 	<LI>the eclipse plugin of ACS jars is built in the target location 
 * </UL> 
 * <P>
 * The structure of a plugin is:
 * <code>
 * plugin_folder
 *    META-INF
 *    	MANIFEST.MF
 *    plugin.xml
 *    jarFile1.jar
 *    jarFile2.jar
 *    ...
 *    jarFile3.jar
 * </code>
 * 
 * @author acaproni
 * @since ACS-8.0.1
 */
public class PluginBuilder {
	
	/**
	 * The jacorb folder of jar files.
	 * <P>
	 * It comes from the $JAVA_HOME environment variable.
	 */
	public final String jacorbFolderPropertyName="acs.system.classpath.jacorb.jardirs";
	
	/**
	 * The endorsed folders of jars
	 */
	public final String endorsedFoldersPropertyName="java.endorsed.dirs";
	
	/**
	 * The folder where JacORB stores the jar files
	 */
	private String jacorbFileOfJars;
	
	/**
	 * The location in the file system where the generated plugin will
	 * be created.
	 * <P>
	 * <code>target</code> is a writable folder for example could be the 
	 * target platform like <code>~/eclipse_target/eclipse/plugins/</code>.
	 * 
	 */
	private String target;
	
	/**
	 * The name of the plugin
	 */
	private String name;
	
	/**
	 * The folders to look for jars
	 */
	private final Vector<String> jarDirs= new Vector<String>();
	
	/**
	 * A set of folder to look for jars provided by the user 
	 * <P>
	 * This folders are not standard ACS folders.
	 */
	private String[] userDirs=new String[0];
	
	/**
	 * The plugins this plugin depends on (i.e. the dependencies)
	 */
	private String[] requiredPlugins;
	
	/**
	 * The logger to print info and debug messages
	 */
	private static final Logger logger=Logger.getLogger(PluginBuilder.class.getName());
	
	/**
	 * The handler getting logs
	 */
	private static final LogHandler logHandler= new LogHandler();
	
	
	/**
	 * If <code>true</code> the jars are included in the plugin folder (i.e. they 
	 * are copied from the ACS INTROOT, or ACSROOT into the plugin folder).
	 * If <code>wrapJars</code> is <code>true</code> the process bundles by wrapping.
	 * 
	 * If it is <code>false</code>, a link to the jars is created into the plugin folder.
	 * If <code>wrapJars</code> is <code>false</code> the process bundles by reference.
	 */
	private boolean wrapJars;
	
	/**
	 * The level of log
	 */
	private boolean verbose;
	
	/**
	 * If <code>true</code> look for jars in endorsed dirs (passed as java properties)
	 */
	private boolean endorsed;
	
	/**
	 * The folder of the plugin where the jars are copied
	 * <P>
	 * It is the main folder of the plugin and its name usually has a form like
	 * <code>alma.acs.eclipse.test_3.0.2</code>.
	 */
	private File pluginRootFolder=null;
	
	/**
	 * The <code>META-INF</code> folder inside the plugin root folder
	 */
	private File metaFolder=null;
	
	/**
	 * The jars to include in the created plugin.
	 * 
	 */
	private String[] jars;

	/**
	 * The jars to include in the created plugin.
	 * 
	 */
	private String[] finalJarsLocations;

	/**
	 * Constructor
	 * 
	 * @param args Command line arguments
	 */
	public PluginBuilder(String[] args) throws Exception {
		if (parseCmdLineArgs(args)) {
			PluginBuilder.printUsage(System.err);
			System.exit(-1);
		}
		if (verbose) {
			logger.setLevel(Level.FINEST);
		}
		logger.addHandler(logHandler);
		if (verbose) {
			logHandler.setLevel(Level.FINE);
		} else {
			logHandler.setLevel(Level.WARNING);
		}
		
		logger.fine("Plugin name: "+name);
		logger.fine("Plugin folder: "+target);
		logger.fine("bundle by wrapping: "+wrapJars);
		logger.fine("bundle by reference: " + !wrapJars);
		for (String dep: requiredPlugins) {
			logger.fine("Dependency: "+dep);
		}
		logger.fine("ACSROOT: " + System.getenv("ACSROOT"));
		logger.fine("INTROOT: " + System.getenv("INTROOT"));
		if (endorsed) {
			logger.fine("Use endorsed folders");
		} else {
			logger.fine("Do NOT use endorsed folders");
		}
		for (String tmp: userDirs) {
			logger.fine("UserDir: "+tmp);
		}
		logger.fine("Tot. num. of plugins to wrap: "+jars.length);
		for (String jar: jars) {
			logger.fine("Plugin to wrap: "+jar);
		}
		
		// Check if the target is a readable folder containing build.properties
		File targetF = new File(target);
		if (!targetF.isDirectory() || !targetF.canRead() || !targetF.canWrite()) {
			throw new IllegalArgumentException("Wrong permissions or invalid folder "+target);
		}
		// Check if the target is a readable folder
		File pluginF = new File(target);
		if (!pluginF.isDirectory() || !pluginF.canRead()) {
			throw new IllegalArgumentException("Wrong permissions or invalid folder "+target);
		}
		// Check if the name is a valid plugin name
		// 
		// The check is done by checking if the name contains dots and underscore
		if (name.indexOf('.')<0 || name.indexOf('_')<0) {
			logger.warning("Plugin name does not follow eclipse rules (it should be something like eso.alma.ecs_3.0.0)");
		}

		this.finalJarsLocations = new String[jars.length];

		if (this.jars==null || this.jars.length==0) {
			throw new IllegalArgumentException("There must be at least one jar to wrap in the plugin");
		}
		// Init the folder of jars
		initdFolders();
	}
	
	/**
	 * Get the folder to look for jars to wrap in the plugin
	 * 
	 * @throws Exception If at least one of the folder is not valid
	 */
	private void initdFolders() throws Exception {
		
		// Get the endorse folders
		String endorsedProperty = System.getProperty(endorsedFoldersPropertyName);
		String[] endorsedFolders;
		if (endorsed) {
			if (endorsedProperty==null) {
				endorsedFolders=new String[0];
			} else {
				endorsedFolders=endorsedProperty.split(":");
			}
		} else {
			endorsedFolders=new String[0];
		}
		// Get jacorb folder
		jacorbFileOfJars=System.getProperty(jacorbFolderPropertyName);
		// Get the folder of jars
		String jarFolders = System.getProperty(AcsSystemClassLoader.PROPERTY_JARDIRS);
		String[] temp=jarFolders.split(File.pathSeparator);
		
		jarDirs.add(jacorbFileOfJars+"/lib");
		for (String str: temp) {
			jarDirs.add(str);
		}
		for (String str: endorsedFolders) {
			jarDirs.add(str);
		}
		for (String str: userDirs) {
			jarDirs.add(str);
		}
		
		// Check if the folders are readable
		for (String folder: jarDirs) {
			File f = new File(folder);
			if (!f.isDirectory() || !f.canRead()) {
				throw new Exception(folder+" unreadable or not a directory");
			}
			logger.fine("Jar file folder: "+folder);
		}
	}
	
	/**
	 * Create the folder structure of the plugin
	 * 
	 */
	private void initPluginFolders() {
		pluginRootFolder = new File(target+File.separator+name);
		if (!pluginRootFolder.exists()) {
			pluginRootFolder.mkdir();
			logger.fine("Creating folder "+pluginRootFolder.getAbsolutePath());
		}
		metaFolder = new File(target+File.separator+name+File.separator+"META-INF");
		if (!metaFolder.exists()) {
			metaFolder.mkdir();
			logger.fine("Creating folder "+metaFolder.getAbsolutePath());
		}
	}
	
	/**
	 * Build the plugin
	 * 
	 * @throws Exception In case of error building the plugin
	 */
	public void build() throws Exception {
		logger.info("Creating directory structure");
		// Create the folder structure
		initPluginFolders();
		// Copy/link the jars into the plugin folder
		addJars();
		// Write the plugin XML
		addPluginXML();
		// Write the MANIFEST
		addManifest();
	}
	
	/**
	 * Add the <code>plugin.xml</code>
	 * 
	 * @throws IOException In case of erro writing the file
	 * @see PluginXmlWriter
	 */
	private void addPluginXML() throws IOException {
		PluginXmlWriter xmlWriter = new PluginXmlWriter(pluginRootFolder);
		xmlWriter.write();
	}
	
	/**
	 * Add the <code>MANIFEST.MF</code> to the <code>META-INF</code> folder.
	 * 
	 * @throws Exception In case of error writing the manifest.
	 */
	private void addManifest() throws Exception {
		ManifestWriter manifestWriter = new ManifestWriter(
				metaFolder, 
				pluginRootFolder, 
				!wrapJars, 
				finalJarsLocations, 
				requiredPlugins, 
				logger);
		manifestWriter.write();
	}
	
	/**
	 * Add (or link) the jars into the plugin folder.
	 * <P>
	 * jar files are found following ACS rules (i.e. first <code>../lib</code>, then <code>$INTROOT/lib</code> and so on).
	 * 
	 * @throws Exception in case of error 
	 */
	private void addJars() throws Exception {
		if (pluginRootFolder==null || metaFolder==null) {
			throw new IllegalStateException("Directory struct not initialized");
		}
		AcsFolders jarFolders = new AcsFolders(jarDirs);
		int i = 0;
		for (String jar: jars) {
			logger.fine("Adding "+jar);
			File jarFile = jarFolders.getJar(jar);
			if (jarFile==null) {
				throw new Exception(jar+" NOT found");
			}
			finalJarsLocations[i++] = jarFile.getAbsolutePath();
			File dest = new File(pluginRootFolder+File.separator+jar);
			if (wrapJars) {
				FileHelper.copy(jarFile, dest);
			} else {
				//FileHelper.link(jarFile, dest);
			}
		}
	}
	
	/**
	 * Parse the command line
	 * 
	 * @param args The command line params
	 * @return <code>true</code> if the user asked for help
	 */
	public boolean parseCmdLineArgs(String[] args) {
		CmdLineArgs cmdLineArgs = new CmdLineArgs();
		CmdLineRegisteredOption helpOpt = new CmdLineRegisteredOption("-h","--help",0);
		cmdLineArgs.registerOption(helpOpt);
		CmdLineRegisteredOption verboseOpt = new CmdLineRegisteredOption("-v","--verbose",0);
		cmdLineArgs.registerOption(verboseOpt);
		CmdLineRegisteredOption includeOpt = new CmdLineRegisteredOption("-i","--include",0);
		cmdLineArgs.registerOption(includeOpt);
		CmdLineRegisteredOption linkOpt = new CmdLineRegisteredOption("-l","--link",0);
		cmdLineArgs.registerOption(linkOpt);
		CmdLineRegisteredOption dirsOpt = new CmdLineRegisteredOption("-d","--userDirs",0);
		cmdLineArgs.registerOption(dirsOpt);
		CmdLineRegisteredOption endorsedOpt = new CmdLineRegisteredOption("-e","--endorsedDirs",0);
		cmdLineArgs.registerOption(endorsedOpt);
		CmdLineRegisteredOption requiredOpt = new CmdLineRegisteredOption("-r","--requiredPlugins",0);
		cmdLineArgs.registerOption(requiredOpt);
		cmdLineArgs.parseArgs(args);
		
		// Help
		if (cmdLineArgs.isSpecified(helpOpt)) {
			return true;
		}
		
		// Verbose mode
		verbose=cmdLineArgs.isSpecified(verboseOpt);
		
		// Endorsed
		endorsed=cmdLineArgs.isSpecified(endorsedOpt);
		
		if (cmdLineArgs.isSpecified(requiredOpt)) {
			String temp=cmdLineArgs.getValues(requiredOpt)[0];
			requiredPlugins=temp.split(":");
		} else {
			requiredPlugins=new String[0];
		}

		// Set the wrap mode
		String[] mainArgs; // what follows -i|-l
		if (cmdLineArgs.isSpecified(includeOpt) && cmdLineArgs.isSpecified(linkOpt)) {
			System.out.println("Set only one between -i and -l");
			return true;
		}
		if (!cmdLineArgs.isSpecified(includeOpt) && !cmdLineArgs.isSpecified(linkOpt)) {
			System.out.println("Set one between -i and -l");
			return true;
		}
		if (cmdLineArgs.isSpecified(includeOpt)) {
			wrapJars=true;
			mainArgs=cmdLineArgs.getValues(includeOpt);
		} else {
			wrapJars=false;
			mainArgs=cmdLineArgs.getValues(linkOpt);
		}
		if (mainArgs.length<3) {
			System.err.println("Wrong command line args!");
			return true;
		}
		name=mainArgs[0];
		target=mainArgs[1];
		jars=new String[mainArgs.length-2];
		for (int t=2; t<mainArgs.length; t++) {
			jars[t-2]=mainArgs[t];
		}
		
		// User defined folders
		if (cmdLineArgs.isSpecified(dirsOpt)) {
			String udirs=cmdLineArgs.getValues(dirsOpt)[0];
			userDirs=udirs.split(":");
		} else {
			userDirs=new String[0];
		}
		
		
		return false;
	}

	/**
	 * The main 
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		System.out.println();
		PluginBuilder pluginBuilder=null;
		try {
			pluginBuilder= new PluginBuilder(args);
		} catch (Exception e) {
			logger.log(Level.SEVERE,"Error: "+e.getMessage(),e);
			System.exit(-1);
		}
		logger.fine("Creating plugin");
		try {
			pluginBuilder.build();
			logger.fine("Done");
		} catch (Throwable t) {
			logger.log(Level.SEVERE,"Error building plugin: "+t.getMessage(),t);
			System.exit(-1);
		}
	}
	
	/**
	 * Print the usage string in the passed stream
	 * 
	 * @param stream The stream to print the string
	 */
	public static void printUsage(OutputStream stream) {
		StringBuilder ret = new StringBuilder("\nacsPluginBuilder build an eclipse plugin from a list ACS jars.\n");
		ret.append("USAGE: \n");
		ret.append("acsPluginBuilder [OPTION] -l|-i pluginName destFolder jar...\n");
		ret.append("\t-i: the jars files are included in the plugin\n");
		ret.append("\t-l: the jars files are linked in the plugin\n");
		ret.append("\tpluginName: the name of the plugin\n");
		ret.append("\tdestFolder: the destination folder of the created plugin of jars\n");
		ret.append("\tjars...: the list of jars to include in the plugin\n");
		ret.append("Options:\n");
		ret.append("\t-h, --help: print this help and exit\n");
		ret.append("\t-v, --verbose: verbose output\n");
		ret.append("\t-d, --userDirs <dirs>: supply a comma separated list of folder for searching jars\n");
		ret.append("\t-e, --endorsedDirs: looks for jars in endosed folders\n");
		ret.append("\t-r, --requiredPlugins: a comma separated list of plugin names required by this plugin\n");
		ret.append("Required plugin must be inserted in the following way: org.eclipse.swt_3.6.0:org.aparche.xerces\n");
		ret.append("  means to add version 3.6.0 of org.eclipse.swt and org.aparche.xerces\n");
		try {
			stream.write(ret.toString().getBytes());
		} catch (IOException e) {
			System.err.println(ret.toString());
		}
	}

}
