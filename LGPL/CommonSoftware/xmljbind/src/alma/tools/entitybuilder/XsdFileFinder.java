/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
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
package alma.tools.entitybuilder;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;

import alma.acs.makesupport.AcsFileFinder;
import alma.acs.testsupport.TestLogger;

/**
 * Finds xsd-bindingclass-config files (.xml) 
 * in the wicked depth of overlayed ACS directories such as INTROOT/idl and ACSROOT/idl.
 *  
 * @author hsommer
 * created Feb 10, 2005 
 */
public class XsdFileFinder extends AcsFileFinder
{
	private List<String> xsdConfigFileNames;
    private boolean verbose;

    /**
	 * @param dirs the directories to search for files in
     * @param xsdConfigFileNames the path-free names of xsd config files to use, such as "systementities.xml" 
	 */
	public XsdFileFinder(List<File> dirs, List<String> xsdConfigFileNames) {
        // todo-: the base class should be changed to not always call scanDirs(..) in the ctor.
        // then our filename filter could be smarter and only allow the known xsdConfigFileNames 
        // instead of any .xml file. Not a big deal though since we handle this in a second step filtering.
		super(dirs.toArray(new File[dirs.size()]), new XsdConfigFileNameFilter(), TestLogger.getLogger("xmljbind-XsdFileFinder", Level.FINER));
        this.xsdConfigFileNames = xsdConfigFileNames;
	}

//	public String getClasspath() {
//		String cp = "";
//		for (Iterator iter = m_fileMap.values().iterator(); iter.hasNext();)
//		{
//			File jarfile = (File) iter.next();
//			cp += jarfile.getAbsolutePath() + File.pathSeparator;
//		}
//		return cp;
//	}
	
//	/**
//     * Finds an XML schema file given its file name without path.
//     * @return the file, or null if it's not found.
//	 */
//	public File resolveXsdFile(String xsdFileName) {
//        File schema = (File) m_fileMap.get(xsdFileName);
//        if (verbose) {
//            m_logger.fine("requested=" + xsdFileName + "; resolved=" + schema);
//        }
//        return schema;
//    }

    /**
     * Finds a config file for XML schema binding class generation, given its file name without path.
     * @return the file, or null if it's not found or if it is not one of the registered <code>xsdConfigFileNames</code> from the c'tor.
     */
    public File resolveXsdConfigFile(String xsdConfigFileName) {
        File schemaConfigFile = (File) m_fileMap.get(xsdConfigFileName);
        if (schemaConfigFile != null && !xsdConfigFileNames.contains(xsdConfigFileName)) {
            String msg = XsdFileFinder.class.getName() + "#resolveXsdConfigFile: requested file name matched '" + 
                        schemaConfigFile.getAbsolutePath() + "', however it was not registered as a valid config file in the constructor of this class.";
            m_logger.warning(msg);
            schemaConfigFile = null;
        }
        if (verbose) {
            String msg = "requested=" + xsdConfigFileName + "; resolved=" + schemaConfigFile;
            m_logger.fine(msg);
        }
        return schemaConfigFile;
    }

    
    public File[] getAllXsdConfigFiles() {
        ArrayList<File> files = new ArrayList<File>();
        for (String xsdConfigFileName : xsdConfigFileNames) {
            File configFile = resolveXsdConfigFile(xsdConfigFileName);
            if (configFile != null) {
                files.add(configFile);
            }
            else {
                m_logger.warning("Binding configuration file '" + xsdConfigFileName + "' not found. Will try to continue without.");
            }
        }
        return files.toArray(new File[files.size()]);
    }
    
    
	public static class XsdConfigFileNameFilter implements FilenameFilter {
		public boolean accept(File dir, String name) {
			return ( name.toLowerCase().endsWith("xml") );
		}

	}


    public void setVerbose(boolean verbose) {
        this.verbose = verbose;        
    }


}
