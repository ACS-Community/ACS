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
 * 
 */
public abstract class ConfigFileRedeemer {

	protected Logger logger;
	
	public ConfigFileRedeemer(Logger logger) {
		this.logger = logger;
	}
	
	/**
	 * Checks whether the given file is known to be not a config file,
	 * but to serve a more respectable purpose.
	 * <p>
	 * Note that the selection is negative: from all files that are suspected based on their file ending,
	 * we prune those which are known to be not config files.
	 */
	public final boolean isNotAConfigFile(File file) {
		String[] supportedFileEndings = getFileEndings();
		if (supportedFileEndings == null) {
			// any file
			return _isNotAConfigFile(file);
		}
		else {			
			for (int i = 0; i < supportedFileEndings.length; i++) {
				if (file.getName().endsWith(supportedFileEndings[i])) {
					return _isNotAConfigFile(file);
				}
			}
		}
		// we don't know, some other redeemer should tell.
		return false;
	}

	/**
	 * Called by {@link #isNotAConfigFile(File)} only if the file ending matches
	 * one of the endings in {@link #getFileEndings()}. 
	 */
	protected abstract boolean _isNotAConfigFile(File file);

	/**
	 * Subclasses supply the endings of files that they can possibly redeem.
	 * <p>
	 * Note that in order to match, the file name must end with one of the given strings,
	 * see {@link String#endsWith(java.lang.String)}. The ending may therefore contain more than one '.'.   
	 * @return
	 */
	public abstract String[] getFileEndings();
	
	public String getName() {
		return getClass().getSimpleName();
	}

}
