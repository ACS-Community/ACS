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
import java.util.logging.Logger;

/**
 * Redeems all files that are located underneath a module's test directorie(s).
 * To determine whether a directory is a module's test dir, this class uses the heuristics that
 * the name must be "test", that it must contain a Makefile, and that there be a sibling directory "src"
 * which must also contain a Makefile.
 *  
 * @author hsommer
 */
public class ConfigFileRedeemerTestDir extends ConfigFileRedeemer {

	public static final String testDirName = File.separator + "test" + File.separator;
	public static final String srcDirName = File.separator + "src" + File.separator;
	
	public ConfigFileRedeemerTestDir(Logger logger) {
		super(logger);
	}

	protected boolean _isNotAConfigFile(File file) {
		String name = file.getAbsolutePath();
		int testDirPos = name.lastIndexOf(testDirName);
		if (testDirPos >= 0) {
			String modulePath = name.substring(0, testDirPos); // or path to "module/ws" dir in modules that have this structure
//			System.out.println("Found config file in test dir in module " + modulePath);
			File testDirMakefile = new File(modulePath + testDirName, "Makefile");
			File srcDirMakefile = new File(modulePath + srcDirName, "Makefile");
			if (testDirMakefile.exists() && srcDirMakefile.exists()) {
				return true;
			}
			else {
				System.out.println("Test-dir sanity check failed for '" + name + "'. Please add Makefile files under test and src dirs.");
			}
		}
		return false;
	}

	/**
	 * Returns <code>null</code> so that it gets to redeem any kind of file 
	 * which is under a module's test dir.
	 */
	public String[] getFileEndings() {
		return null;
	}

}
