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
package alma.acs.makesupport;

import java.io.File;
import java.io.FilenameFilter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

/**
 * Utility class that finds files in the staged directory structure of ACS, 
 * with $INTROOT and $ACSROOT etc.
 *  
 * @author hsommer
 * created Sep 16, 2003 2:14:55 PM
 */
public class AcsFileFinder
{
	protected boolean verbose = false;
	
	/** key = filename without path; value = File object */
	protected Map m_fileMap;
	
	protected Logger m_logger;

	
	/**
	 * ctor
	 * 
	 * @param dirs  the directories to search files in, e.g. $INTROOT/lib and $ACSROOT/lib;
	 * 				Files that appear under more than one directory will be taken at their first
	 * 				occurence.
	 * @param logger  logger to be used by this class
	 */
	public AcsFileFinder(File[] dirs, Logger logger)
	{
		this(dirs, null, logger);
	}


	/**
	 * ctor
	 * 
	 * @param dirs  the directories to search files in, e.g. $INTROOT/lib and $ACSROOT/lib;
	 * 				Files that appear under more than one directory will be taken at their first
	 * 				occurence.
	 * @param logger  logger to be used by this class
	 */
	public AcsFileFinder(File[] dirs, FilenameFilter filenameFilter, Logger logger)
	{
//		if (logger == null)
//		{
//			throw new NullPointerException("Logger object must be provided!");
//		}
		m_logger = logger;

		// todo: perhaps in different thread? 
		// seems not necessary for the moment: takes around 50 - 100 milliseconds with local jar files
		scanDirs(dirs, filenameFilter);
	}


	public void setVerbose(boolean verbose) {
		if (m_logger != null) {
			this.verbose = verbose;
		}
	}
	
	
	/**
	 * @param dirs
	 * @param filenameFilter  can be null
	 */
	private void scanDirs(File[] dirs, FilenameFilter filenameFilter)
	{
		m_fileMap = new LinkedHashMap(); 

		if (dirs == null || dirs.length == 0)
		{
			return;
		} 
		
		// iterate over directories
 		for (int i = 0; i < dirs.length; i++)
		{
			File dir = dirs[i];
			if (dir == null || !dir.exists() || !dir.isDirectory() || !dir.canRead())
			{
				continue;
			}
			
			// get files from the current directory
			String[] filenames = null;
			if (filenameFilter != null) {
				filenames = dir.list(filenameFilter);
			}
			else {
				filenames = dir.list();
			}
			for (int j = 0; j < filenames.length; j++) 
			{
				String filename = filenames[j];
				
				File file = new File(dir, filename);
				if (!file.isFile())
				{
					continue;
				}
				
//				URL url = new URL("file", null, file.getCanonicalPath());
				File existing = (File) m_fileMap.get(filename);
				if (existing != null)
				{
					if (verbose) {
						m_logger.finer("skipping " + file.getAbsolutePath() + 
								" in favor of " + existing.getAbsolutePath());
					}
				}
				else
				{
					if (verbose) {
						m_logger.finer("adding " + file.getAbsolutePath());
					}
					m_fileMap.put(filename, file);
				}
			}			
		}
	}
	
	public File[] getAllFiles()
	{
		File[] files = (File[]) m_fileMap.values().toArray(new File[m_fileMap.size()]);
		return files;
	}
	
	public File[] getFiles(FilenameFilter filter)
	{
		List fileList = new ArrayList();
		
		for (Iterator iter = m_fileMap.values().iterator(); iter.hasNext();)
		{
			File file = (File) iter.next();
			if (filter.accept(file.getParentFile(), file.getName()))
			{
				fileList.add(file);
			}
		}
		
		File[] files = (File[]) fileList.toArray(new File[fileList.size()]);
		return files;
	}
}
