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
package alma.acs.eclipse.utils.jar;

import java.io.File;
import java.util.Vector;


/**
 * {@link AcsFolders} helps getting jar files from a given set of folders.
 * <P>
 * ACS states that the jar file to use should be found by scanning an ordered
 * set of folders, typically <code>../lib</code>, <code>$INTROOT/lib</code>
 * and so on.
 * {@link AcsFolders} Takes an ordered set of folders and helps getting jar files.
 * 
 * @author acaproni
 *
 */
public class AcsFolders {
	
	/**
	 * The folder of jars
	 */
	private final JarFolder[] folders;
	
	/**
	 * Build a {@link AcsFolders} object by passing the folder of jars
	 * 
	 * 
	 * @throws Exception If the passed path are not valid or the folders do not exist
	 */
	public AcsFolders(final Vector<String> folders) throws Exception {
		if (folders==null || folders.size()==0) {
			throw new IllegalArgumentException("Empty/null folders of jars");
		}
		this.folders = new JarFolder[folders.size()];
		for (int t=0; t<this.folders.length; t++) {
			File directory = new File(folders.get(t));
			if (!checkFolder(directory)) {
				throw new Exception(directory.getAbsolutePath()+" invalid");
			}
			this.folders[t]=new JarFolder(directory);
		}
	}
	
	/**
	 * Check if the passed folder is valid.
	 * 
	 * @return <code>true</code> if the folder is a valid ACS folder for INTROOT or ACSROOT
	 */
	private boolean checkFolder(File folder) {
		if (folder==null) {
			throw new IllegalArgumentException("Invalid null folder");
		}
		return folder.isDirectory() && folder.canRead();
	}
	
	/**
	 * Get the file of the jar with the passed name by scanning the folders.
	 * 
	 * @param name The name of the jar like for example <code>maci.jar</code>
	 * @return The {@link File} of the jar or <code>null</code> if the file does not exist
	 * 			in the ACS folders.
	 */
	public File getJar(String name) {
		if (name==null || name.isEmpty()) {
			throw new IllegalArgumentException("Invalid jar name");
		}
		File ret=null;
		for (int t=0; t<folders.length && ret==null; t++) {
			ret = folders[t].getJar(name);
		}
		return ret;
	}
	
}
