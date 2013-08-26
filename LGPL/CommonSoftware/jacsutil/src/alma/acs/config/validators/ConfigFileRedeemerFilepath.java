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
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

/**
 * Redeems files based on (part of) their path name.
 * This class should be used to ignore files under certain directories, or specific files.
 * <p>
 * One should be careful to use path name snippets that match as accurately as possible,
 * to avoid excluding other files whose path names unexpectedly also match.
 * For better accuracy, you should
 * <ul>
 *  <li>Anchor top-level directories to the base dir of the search 
 *      using the <code>^</code> character prepended to the path name snippet set in {@link #addRedeemedFilePathSnippet(String)}. <br> 
 *      For example, the path name snippet <code>^ITS</code> will exclude all files under <code>/alma/src/MONTHLY-2006-03-ITER-2/ITS</code>
 *      if the base directory in {@link alma.acs.config.validators.ConfigFileFinder ConfigFileFinder} is given as <code>/alma/src/MONTHLY-2006-03-ITER-2</code>.
 *      However it will not exclude files under <code>mymodule/src/alma/mypackage/ITS/blabla</code>.
 *  <li>To exclude the <code>test</code> directory in all modules, use the 
 *      specialized filter {@link alma.acs.config.validators.ConfigFileRedeemerTestDir ConfigFileRedeemerTestDir}
 *      which not only checks whether /test/ is on the path name, but also looks for a parallel <code>src</code>
 *      directory with a Makefile and thus minimizes the risk of a false match.
 * </ul>
 * @author hsommer
 */
public class ConfigFileRedeemerFilepath extends ConfigFileRedeemer {

	private List<String> redeemedFilePathSnippets;
	private String baseDirPath;
	
	public ConfigFileRedeemerFilepath(Logger logger, File baseDir) {
		super(logger);
		baseDirPath = baseDir.getAbsolutePath();
		redeemedFilePathSnippets = new ArrayList<String>();
		configure();
	}

	protected boolean _isNotAConfigFile(File file) {
		String relPathName = file.getAbsolutePath();
		if (relPathName.startsWith(baseDirPath)) {
			// this is expected
			relPathName = relPathName.substring(baseDirPath.length() + 1); // +1 to cut off separator char
//			System.out.println("relative file name: " + relPathName);
		}
		else {
			logger.warning(("File '" + file.getAbsolutePath() + "' does not lie under base dir " + baseDirPath));
			return false;
		}
		
		if (File.separatorChar != '/') {
			relPathName = relPathName.replace(File.separatorChar, '/');
		}
		for (Iterator<String> iter = redeemedFilePathSnippets.iterator(); iter.hasNext();) {
			String snippet = iter.next();
			boolean snippetAtStart = false;
			if (snippet.charAt(0) == '^') {
				snippetAtStart = true;
				snippet = snippet.substring(1);
			}
			if ((snippetAtStart && relPathName.startsWith(snippet)) || (!snippetAtStart && relPathName.indexOf(snippet) > 0)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Returns <code>null</code> so that it gets to redeem any kind of file. 
	 */
	public String[] getFileEndings() {
		return null;
	}

	
	public void addRedeemedFilePathSnippet(String filePathSnippet) {
		if (filePathSnippet != null) {
			redeemedFilePathSnippets.add(filePathSnippet);
		}
	}
	
	/**
	 * This method may be overridden by a subclass.
	 * TODO: read in from file instead of hardcoding the snippets
	 */
	public void configure() {
		
		String[] snippets = new String[] {
			"^ACS/LGPL/Tools", // either fixed config files, or false positives (test files etc)
			"^ACS/LGPL/CommonSoftware/acscourse", // does not get installed
			"^ITS",
			"^ITS.old",
			"^OBSPREP/ObservingTool/idl/ObsToolUserPrefs.xsd"
		};
		
		for (int i = 0; i < snippets.length; i++) {
			addRedeemedFilePathSnippet(snippets[i]);
		}
	}
}
