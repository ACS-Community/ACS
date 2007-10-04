

/** 
 * @author Mauricio Araya (maray[at]inf.utfsm.cl)
 * @author Jorge Avarias (javarias[at]inf.utfsm.cl)
 * 
 * @since 1.0
 */

package cl.utfsm.acs.ebe.util;

import java.io.File;

import javax.swing.filechooser.FileFilter;

/**
 * 
 * Restricts the file type to open and save to xml
 *
 */

public class EbeOpenFileFilter extends FileFilter {

	public boolean accept(File f) {
		if (f.isDirectory())
			return true;
		String extension = f.getName();
		
		if(extension.endsWith(".xml"))
			return true;
	
		return false;
	}

	public String getDescription() {
		return "ACS Error Files (.xml)";
	}

}
