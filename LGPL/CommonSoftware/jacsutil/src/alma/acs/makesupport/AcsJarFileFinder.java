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
import java.util.Iterator;
import java.util.logging.Logger;

/**
 * @author hsommer
 * created Sep 16, 2003 4:07:20 PM
 */
public class AcsJarFileFinder extends AcsFileFinder
{

	/**
	 * @param dirs
	 * @param logger
	 */
	public AcsJarFileFinder(File[] dirs, Logger logger)
	{
		super(dirs, new JarFileNameFilter(), logger);
	}

	public String getClasspath()
	{
		StringBuffer cp = new StringBuffer();
		for (Iterator iter = m_fileMap.values().iterator(); iter.hasNext();)
		{
			File jarfile = (File) iter.next();
			cp.append(jarfile.getAbsolutePath() + File.pathSeparator);
		}
		return cp.toString();
	}



	public static class JarFileNameFilter implements FilenameFilter
	{
		public boolean accept(File dir, String name)
		{
			return ( name.toLowerCase().endsWith("jar") );
		}

	}
}
