/*
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2008 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.acs.logging.io;

import java.io.File;

import javax.swing.filechooser.FileFilter;

/**
 * The filter of files shown by the chooser.
 * <P>
 * The filtering is based on the extension of the files (case insensitive)
 * 
 * @author acaproni
 *
 */
public class FileChooserFilter extends FileFilter {

	/**
	 * The extensions used for filtering like for example ".gz" or ".xml"
	 */
	private String[] extensions;
	
	/**
	 * Constructor
	 * 
	 * @param extensions The extension used for filtering
	 */
	public FileChooserFilter(String[] extensions) {
		if (extensions==null || extensions.length==0) {
			throw new IllegalArgumentException("Invalid extensions");
		}
		this.extensions=extensions;
	}

	/**
	 * Check if the file is readable and if its extensions matches one
	 * of the available extensions.
	 * 
	 * @see javax.swing.filechooser.FileFilter#accept(java.io.File)
	 */
	@Override
	public boolean accept(File f) {
		if (!f.canRead()) {
			return false;
		}
		if (f.isDirectory()) {
			return true;
		}
		String fileName = f.getAbsolutePath().toLowerCase();
		for (String ext: extensions) {
			if (fileName.endsWith(ext.toLowerCase())) {
				return true;
			}
		}
		return false;
	}

	/**
	 * @see javax.swing.filechooser.FileFilter#getDescription()
	 */
	@Override
	public String getDescription() {
		StringBuilder ret = new StringBuilder("Files of type ");
		for (String str: extensions) {
			ret.append('*');
			ret.append(str);
			ret.append(' ');
		}
		return ret.toString();
	}

}
