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
package alma.acs.util;

import java.io.File;
import java.io.IOException;

/**
 * Class that gives access to the current working directory, effectively encapsulating the "user.dir" property.
 * This feature was requested in Feb. 2005 by Lindsey, Brian, et.al.
 * <p>
 * Note that using files goes very much against ALMA's idea of location transparency of component locations, 
 * and against having a central archive to store data. 
 * This class should only be used in exceptional situations, such as for offline data reduction,
 * or for other inevitable temporary file storage, where a component <b>creates and deletes</b> the files within its life cycle.
 * Files should almost never be used for inter-component communication!
 * <p>
 * Note that on a diskless machine, the file created may well be located on a RAM disk and 
 * does not survive machine rebooting. 
 *  
 * @author hsommer
 * @since ACS 5.0
 */
public class AcsCWD {
	
	/**
	 * Gets the current working directory. 
	 * @return File object which represents the directory.
	 */
	public static File getCWD() {
		String cwdName = System.getProperty("user.dir");
		return new File(cwdName);
	}
	
	/**
	 * Sets the current working directory.
	 * @param cwd
	 */
	public static synchronized void setCWD(String cwd) {
		System.setProperty("user.dir", cwd);
	}
	
	/**
	 * Creates a temp file in the current working dir.
     * @param  prefix     The prefix string to be used in generating the file's
     *                    name; must be at least three characters long
     * @param  suffix     The suffix string to be used in generating the file's
     *                    name; may be <code>null</code>, in which case the
     *                    suffix <code>".tmp"</code> will be used
     * @return a newly created file.
	 * @throws IOException
	 * @see File#createTempFile(java.lang.String, java.lang.String, java.io.File)
	 */
	public static synchronized File createTempFileInCWD(String prefix, String suffix) throws IOException {
		return File.createTempFile(prefix, suffix, getCWD());
	}
}
