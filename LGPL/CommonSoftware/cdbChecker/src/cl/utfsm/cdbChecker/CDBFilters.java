
/**
 * @author Rodrigo Araya (raraya[at]inf.utfsm.cl) & Nicolas Barriga (nbarriga[at]inf.utfsm.cl) & Marco Salgado (msalgado[at]inf.utfsm.cl)
 * */

package cl.utfsm.cdbChecker;

import java.io.File;
import java.io.FilenameFilter;

class XSDFilter implements FilenameFilter{
	public boolean accept(File dir, String name){
		return name.endsWith(".xsd");
	}
}

class DirFilter implements FilenameFilter{
	public boolean accept(File dir, String name){
		File file = new File(dir.getAbsolutePath()+"/"+name);
		return file.isDirectory();
	}
}

class XMLFilter implements FilenameFilter{
	public boolean accept(File dir, String name){
		return name.endsWith(".xml");
	}
}