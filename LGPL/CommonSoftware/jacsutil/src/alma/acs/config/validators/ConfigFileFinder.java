/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2005
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

package alma.acs.config.validators;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.cosylab.util.FileHelper;

import alma.acs.testsupport.TestLogger;
import alma.acs.util.CmdLineArgs;
import alma.acs.util.CmdLineRegisteredOption;

/**
 * Tool that scans modules (or all of ALMA software) for files that may be configuration files
 * and therefore should be considered for moving to a different location.
 * <p>
 * All files in and below the directory given by the option <code>-baseDir</code> are scanned.
 * Those files with certain endings are suspected of being configuration files, but then some of these files get cleared
 * if they are recognized by one of the filters which check all suspicious files.  
 * For example, a filter specialized in xml files will recognize those files used for ACS error definitions.
 * All files that are still suspect after filtering will be reported, either by printing their pathname to stdout, 
 * or by copying them to a target directory if the <code>-targetDir</code> option is given.
 *  
 * @author hsommer
 */
public class ConfigFileFinder {

	private Logger logger;
	
	/** directory under which we look for config files */
	private File baseDir;
	
	/** optional target dir to which suspected config files are copied */
	private File targetDir;

	/** endings of files to consider, e.g. {"xml", "properties"} */
	private Set<String> fileEndings = new HashSet<String>();

	/** should files be copied to targetDir flat, i.e. w/o subdir structure? */
	private boolean targetFilesFlat;

	/** classes that can recognize specific suspect files and state that they are actually not config files */ 
	private List<ConfigFileRedeemer> redeemers = new ArrayList<ConfigFileRedeemer>();
	
	
	/**
	 * @throws Exception 
	 */
	public ConfigFileFinder() throws Exception {
		targetFilesFlat = false;
		logger = TestLogger.getLogger(getClass().getName());

		// hardwired file endings that don't need to be supplied by any filters 
		addFileEndings(new String[] {".properties", ".config"});
		
	}

	public void configureRedeemers() throws Exception {
		addRedeemer(new ConfigFileRedeemerFilepath(logger, baseDir));
		addRedeemer(new ConfigFileRedeemerTestDir(logger));
		addRedeemer(new ConfigFileRedeemerXml(logger));
		
		// todo: perhaps allow user-supplied redeemers to be added, based on a command line parameter (classname) 			
	}

	
	public void addRedeemer(ConfigFileRedeemer redeemer) {
		redeemers.add(redeemer);
		addFileEndings(redeemer.getFileEndings());
	}
	
	
	public void configureFromArgs(String[] args) {
		CmdLineArgs cmdArgs = new CmdLineArgs();
		
		CmdLineRegisteredOption optBaseDir = new CmdLineRegisteredOption("-baseDir", 1);
		cmdArgs.registerOption(optBaseDir);

		CmdLineRegisteredOption optTargetDir = new CmdLineRegisteredOption("-targetDir", 1);
		cmdArgs.registerOption(optTargetDir);
		
		CmdLineRegisteredOption optTargetFilesFlat = new CmdLineRegisteredOption("-targetFilesFlat", 0);
		cmdArgs.registerOption(optTargetFilesFlat);

		CmdLineRegisteredOption optFileEndings = new CmdLineRegisteredOption("-fileEndings", 1);
		cmdArgs.registerOption(optFileEndings);
		
		cmdArgs.parseArgs(args);
		
		if (cmdArgs.isSpecified(optBaseDir)) {
			String baseDirName = cmdArgs.getValues(optBaseDir)[0].trim();
			setBaseDir(new File(baseDirName));
		}
		
		if (cmdArgs.isSpecified(optTargetDir)) {
			String targetDirName = cmdArgs.getValues(optTargetDir)[0].trim();
			setTargetDir(new File(targetDirName));
		}

		targetFilesFlat = cmdArgs.isSpecified(optTargetFilesFlat);

		if (cmdArgs.isSpecified(optFileEndings)) {
			addFileEndings(cmdArgs.getValues(optFileEndings));
		}

	}

	public void checkConfiguration() throws IllegalStateException {
		String err = "";
		String warn = "";
		
		if (baseDir == null || !baseDir.exists()) {
			err += "baseDir '" + baseDir + "' does not exist. ";
		}
		if (targetDir == null) {
			warn += "targetDir not specified. No files will be copied. ";
		}
		if (fileEndings.isEmpty()) {
			// should never happen thanks to default endings 
			err += "no files can be selected. Specify option \"-fileEndings\". ";
		}
		if (err.length() > 0) {
			throw new IllegalStateException("Bad configuration: " + err);
		}
		if (warn.length() > 0) {
			System.err.println("Configuration warning: " + warn);
		}
	}

	/**
	 * Gets all Files under {@link #baseDir} whose name ends with any of the Strings given in {@link #fileEndings},
	 * and runs them through the filter chain to suppress files of known and safe content.
	 * @return
	 */
	private void run() {
		String msg = "Tool " + getClass().getSimpleName() + " will search for configuration files in '" + baseDir + 
					"' and below, suspecting all files ending with ";
		for (Iterator<String> iter = fileEndings.iterator(); iter.hasNext();) {
			msg += "'" + iter.next() + "'";
			if (iter.hasNext()) {
				msg += ", ";
			}
		}
		msg += " and will then prune those files recognized by any of the filters ";
		for (Iterator<ConfigFileRedeemer> iter = redeemers.iterator(); iter.hasNext();) {
			msg += iter.next().getName();
			if (iter.hasNext()) {
				msg += ", ";
			}
		}
		msg += ". ";
		if (targetDir != null) {
			msg += "The remaining potential config files are copied to '" + targetDir + "' with their original directory structure ";
			if (targetFilesFlat) {
				msg += "flattened.";
			}
			else {
				msg += "maintained.";
			}
		}
		logger.info(msg);
		
		FileFilter fileFilter = new FileFilter() {
			public boolean accept(File pathname) {
				if (pathname.isDirectory()) {
					return true;
				}
				String name = pathname.getName();
				for (Iterator iter = fileEndings.iterator(); iter.hasNext();) {
					String fnEnding = (String) iter.next();
					if (name.endsWith(fnEnding)) {
						return true;
					}
				}
				return false;
			}
		};

		runRecursive(baseDir, fileFilter);
	}
	

	private void runRecursive(File currentDir, FileFilter fileFilter) {
		if (currentDir == null || !currentDir.exists() || !currentDir.isDirectory() ) {
			return;
		}
		if (!currentDir.canRead()) {
			logger.warning("failed to read from directory " + currentDir.getAbsolutePath());
			return;
		}
		
		// get files and subdirs from the current directory
		File[] allFiles = currentDir.listFiles(fileFilter);

		for (int i = 0; i < allFiles.length; i++) {
			if (allFiles[i].isFile()) {				
				// check if the current file is known to be not a config file 
				boolean redeemed = false;
				if (allFiles[i].canRead()) {
					for (Iterator<ConfigFileRedeemer> iter = redeemers.iterator(); iter.hasNext();) {
						ConfigFileRedeemer filter = iter.next();
						if (filter.isNotAConfigFile(allFiles[i])) {
							redeemed = true;
							break;
						}
					}
				}
				else {
					logger.severe("Failed to read file " + allFiles[i]);
				}
				if (!redeemed) {
					handleConfigFile(allFiles[i]);
				}
			}
			else if (allFiles[i].isDirectory()) {
				runRecursive(allFiles[i], fileFilter);
			}
		}		
	}

	protected void handleConfigFile(File configFile) {
		try {
			if (targetDir != null) {
				try {
					if (targetFilesFlat) {
						File targetFile = new File(targetDir, configFile.getName());
						FileHelper.copy(configFile, targetFile, true);
					}
					else {
						String relPathName = configFile.getAbsolutePath().substring(baseDir.getAbsolutePath().length());
						if (!relPathName.startsWith(File.separator)) {
							relPathName = File.separator + relPathName;
						}
						File targetFile = new File(targetDir, relPathName);
//						System.err.println("will create " + targetFile.getAbsolutePath());
						FileHelper.copy(configFile, targetFile, true);
					}
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
			System.out.println("Potential config file: " + configFile.getAbsolutePath());
		}
		catch (Throwable thr) {
			logger.log(Level.SEVERE, "Failed to handle file " + configFile.getAbsolutePath(), thr);
		}
	}
	
	
	protected void setBaseDir(File baseDir) {
		this.baseDir = baseDir;
	}

	protected void setTargetDir(File targetDir) {
		this.targetDir = targetDir;
	}

	protected void addFileEndings(String[] moreFileEndings) {
		if (moreFileEndings != null) {
			for (int i = 0; i < moreFileEndings.length; i++) {
				if (moreFileEndings[i] == null || moreFileEndings[i].trim().length() == 0) {
					throw new IllegalArgumentException("illegal empty file ending.");
				}
				this.fileEndings.add(moreFileEndings[i].trim());
			}
		}
	}


	
	public static void main(String[] args) {
		try {
			ConfigFileFinder configFinder = new ConfigFileFinder();
			configFinder.configureFromArgs(args);
			configFinder.configureRedeemers();
			configFinder.checkConfiguration();
			
			configFinder.run();
			
			System.out.println("done.");
		} catch (Exception e) {
			e.printStackTrace();
		}
	}


}