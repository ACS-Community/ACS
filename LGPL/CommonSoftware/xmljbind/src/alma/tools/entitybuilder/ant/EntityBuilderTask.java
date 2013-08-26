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
package alma.tools.entitybuilder.ant;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Task;

import alma.tools.entitybuilder.CastorBuilder;

/**
 * @author hsommer
 * created Aug 17, 2004 2:00:21 PM
 */
public class EntityBuilderTask extends Task
{
	private String javaOutputDir;
	private File xsdBindConfigFile;
	private List<XsdIncludedConfigFile> xsdIncludedConfigFileList;	
	private List<XsdIncludeDir> xsdIncludeDirList;

	private boolean verbose;

	
	public EntityBuilderTask() {
		super();
		xsdIncludedConfigFileList = new ArrayList<XsdIncludedConfigFile>();
		xsdIncludeDirList = new ArrayList<XsdIncludeDir>();
		verbose = false;
	}
	
	
	public void execute() throws BuildException {
		if (verbose) {
			log("EntityBuilderTask.execute called");
		}
		
		Properties oldProps = (Properties)System.getProperties().clone();
		try {
			String[] mainArgs = new String[2 + xsdIncludeDirList.size()];
			mainArgs[0] = xsdBindConfigFile.getAbsolutePath();
			mainArgs[1] = javaOutputDir;

			for (int x = 0; x < xsdIncludeDirList.size(); x++) {
				mainArgs[2+x] = "-I" + xsdIncludeDirList.get(x).getDir().getAbsolutePath(); 
			}			

			// optional config files for included XSD files (to get their java packages right)
			String xsdBindConfIncluded = "";
			for (int x = 0; x < xsdIncludedConfigFileList.size(); x++) {
				xsdBindConfIncluded += xsdIncludedConfigFileList.get(x).getFilename() + " ";
			}			
			System.setProperty("ACS.schemaconfigfiles", xsdBindConfIncluded);

			CastorBuilder.main(mainArgs);
		} 
		finally {
			System.setProperties( oldProps );
		}		
	}

	
	public void setXsdBindConfigFile(File xsdBindConfigFile)
	{
		this.xsdBindConfigFile = xsdBindConfigFile;
		if (verbose) {
			log("set xsdBindConfigFile to " + xsdBindConfigFile.getAbsolutePath());
		}
	}
	
	public void setJavaOutputDir(String javaOutputDir) {
		this.javaOutputDir = javaOutputDir;
		if (verbose) {
			log("set javaOutputDir to " + javaOutputDir);
		}
	}

	
    public void addConfiguredXsdIncludedConfigFile(XsdIncludedConfigFile xsdIncludedConfigFile) {
   		xsdIncludedConfigFileList.add(xsdIncludedConfigFile);
		if (verbose) {
			log("added xsdIncludedConfigFile " + xsdIncludedConfigFile.getFilename());
		}
    }

    public void addConfiguredXsdIncludeDir(XsdIncludeDir newXsdIncludeDir) {
    	if (newXsdIncludeDir.isValid()) {
        	xsdIncludeDirList.add(newXsdIncludeDir);
    		if (verbose) {
    			log("added XsdIncludeDir " + newXsdIncludeDir.getDir().getAbsolutePath());
    		}
    	}
    	else {
    		if (verbose) {
    			log("ignored invalid XsdIncludeDir " + newXsdIncludeDir.getDir().getAbsolutePath());
    		}
    	}
    }
    
	public void setVerbose(boolean verbose)
	{
		this.verbose = verbose;
	}
}
