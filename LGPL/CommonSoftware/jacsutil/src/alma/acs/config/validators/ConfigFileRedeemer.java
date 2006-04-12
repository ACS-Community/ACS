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

/**
 * Class that 
 */
public abstract class ConfigFileRedeemer {

	/**
	 * Checks whether the given file is known to be not a config file,
	 * but to serve a more respectable purpose.
	 */
	public final boolean isNotAConfigFile(File file) {
		String[] supportedFileEndings = getFileEndings();
		for (int i = 0; i < supportedFileEndings.length; i++) {
			if (file.getName().endsWith(supportedFileEndings[i])) {
				return _isNotAConfigFile(file);
			}
		}
		// we don't know, some other redeemer should tell.
		return false;
	}

	protected abstract boolean _isNotAConfigFile(File file);

	public abstract String[] getFileEndings();
}
