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
	private final String target;
	
	/**
	 * The name of the plugin
	 */
	private final String name;
	
	/**
	 * The folders to look for jars
	 */
	private String[] jarDirs;
	
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
	 * are copied fomr the ACS INTROOT, or ACSROOT into the plugin folder).
	 * If <code>wrapJars</code> is <code>true</code> the process bundles by wrapping.
	 * 
	 * If it is <code>false</code>, a link to the jars is created into the plugin folder.
	 * If <code>wrapJars</code> is <code>false</code> the process bundles by reference.
	 */
	private final boolean wrapJars;
	
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
	private final String[] jars;

	/**
	 * The jars to include in the created plugin.
	 * 
	 */
	private String[] finalJarsLocations;

	/**
	 * Constructor
	 * 
	 * @param pluginName the name of the generated plugin
	 * @param target the folder to install the created plugin into
	 * @param wrap <code>true</code> if the jars must be copied in the destination plugin;
	 * 				<code>false</code> if the jars are linked into the plugin folder
	 * @param jars The jars to wrap in the plugin (For example <code>lc.jar</code>)
	 */
	public PluginBuilder(String pluginName, String target, Boolean wrap, String[] jars) throws Exception {
		if (target==null || target.isEmpty()) {
			throw new IllegalArgumentException("Invalid destination");
		}
		this.target=target;
		if (pluginName==null || pluginName.isEmpty()) {
			throw new IllegalArgumentException("Invalid plugin name");
		}
		this.name=pluginName;
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
		if (wrap==null) {
			throw new IllegalArgumentException("Wrap mode not set");
		}

		this.wrapJars=wrap;
		this.jars=jars;
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
		// Get jacorb folder
		jacorbFileOfJars=System.getProperty(jacorbFolderPropertyName);
		// Get the folder of jars
		String jarFolders = System.getProperty(AcsSystemClassLoader.PROPERTY_JARDIRS);
		String[] temp=jarFolders.split(":");
		jarDirs=new String[temp.length+1];
		jarDirs[0]=jacorbFileOfJars+"/lib";
		for (int t=0; t<temp.length; t++) {
			jarDirs[t+1]=temp[t];
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
		ManifestWriter manifestWriter = new ManifestWriter(metaFolder, pluginRootFolder, !wrapJars, finalJarsLocations, logger);
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
			finalJarsLocations[i++] = jarFile.getAbsolutePath();
			if (jarFile==null) {
				throw new FileNotFoundException(jar+" not found");
			}
			File dest = new File(pluginRootFolder+File.separator+jar);
			if (wrapJars) {
				FileHelper.copy(jarFile, dest);
			} else {
				//FileHelper.link(jarFile, dest);
			}
		}
	}

	/**
	 * The main 
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		if (args.length<4) {
			printUsage(System.err);
			System.exit(-1);
		}
		String name=null;
		String target=null;
		Boolean wrap=null;
		Vector<String> theJars = new Vector<String>();
		boolean verbose=false;
		for (String str: args) {
			if (str.toLowerCase().equals("-h") || str.toLowerCase().equals("--help")) {
				printUsage(System.err);
				System.exit(0);
			}
			if (str.toLowerCase().equals("-v") || str.toLowerCase().equals("--verbose")) {
				verbose=true;
				continue;
			}
			if (str.toLowerCase().equals("-i")) {
				if (wrap!=null) {
					// -i or -l already set in the command line
					printUsage(System.err);
					System.exit(-1);
				}
				wrap=Boolean.TRUE;
				continue;
			}
			if (str.toLowerCase().equals("-l")) {
				if (wrap!=null) {
					// -i or -l already set in the command line
					printUsage(System.err);
					System.exit(-1);
				}
				wrap=Boolean.FALSE;
				continue;
			}
			if (name==null) {
				name=str;
				continue;
			}
			if (target==null) {
				target=str;
				continue;
			}
			theJars.add(str);
		}
		if (theJars.size()==0) {
			System.err.println("No jars found in command line!");
			printUsage(System.err);
			System.exit(-1);
		}
		String[] jars=new String[theJars.size()];
		theJars.toArray(jars);
		// Set the log level
		logger.setLevel(Level.FINEST);
		logger.addHandler(logHandler);
		if (verbose) {
			logHandler.setLevel(Level.FINE);
		} else {
			logHandler.setLevel(Level.WARNING);
		}
		logger.fine("Plugin name: "+name);
		logger.fine("Plugin folder: "+target);
		logger.fine("bundle by wrapping: "+wrap);
		logger.fine("bundle by reference: " + !wrap);
		logger.fine("ACSROOT: " + System.getenv("ACSROOT"));
		logger.fine("INTROOT: " + System.getenv("INTROOT"));
		logger.fine("Tot. num. of plugins to wrap: "+jars.length);
		for (String jar: jars) {
			logger.fine("Plugin to wrap: "+jar);
		}
		PluginBuilder pluginBuilder=null;
		try {
			pluginBuilder= new PluginBuilder(name,target,wrap,jars);
		} catch (Exception e) {
			logger.log(Level.SEVERE,"Error detected: "+e.getMessage(),e);
		}
		logger.fine("Creating plugin");
		try {
			pluginBuilder.build();
			logger.fine("Done");
		} catch (Throwable t) {
			logger.log(Level.SEVERE,"Error building plugin: "+t.getMessage(),t);
		}
	}
	
	/**
	 * Print the usage string in the passed stream
	 * 
	 * @param stream The stream to print the string
	 */
	public static void printUsage(OutputStream stream) {
		String[] classNames=PluginBuilder.class.getName().split("\\.");
		StringBuilder ret = new StringBuilder(classNames[classNames.length-1]);
		ret. append(" build an eclipse plugin (a folder not a jar) from a list ACS jars.\n");
		ret.append("USAGE: ");
		ret.append(PluginBuilder.class.getName());
		ret.append(" [-h||--help] [-v|--verbose] -l|-i pluginName destFolder jar...\n");
		ret.append("\t-h, --help: print this help and exit\n");
		ret.append("\t-v, --verbose: verbose output\n");
		ret.append("\t-i: the jars files are included in the plugin\n");
		ret.append("\t-l: the jars files are linked in the plugin\n");
		ret.append("\tpluginName: the name of the plugin\n");
		ret.append("\tdestFolder: the destination folder of the created plugin of jars\n");
		ret.append("\tjars...: the list of jars to include in the plugin\n");
		ret.append("The jars must be in the ACS folders (../lib, $INTROOT....).\n\n");
		try {
			stream.write(ret.toString().getBytes());
		} catch (IOException e) {
			System.err.println(ret.toString());
		}
	}

}
