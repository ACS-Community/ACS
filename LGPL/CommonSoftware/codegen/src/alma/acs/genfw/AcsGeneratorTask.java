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
package alma.acs.genfw;

import org.apache.tools.ant.BuildException;

import genfwutil.ant.GeneratorTask;

/**
 * @author hsommer
 * created May 4, 2004 11:42:56 AM
 */
public class AcsGeneratorTask extends GeneratorTask
{
	public void execute() throws BuildException 
	{
		// some ACS linux machines have a strange codepage, which would
		// cause a really stupid error "Definition Check::Check not found for class ..."
		// in the genfw because it fails to read the '«' char; 
		// todo: improve genfw to deal with this nicer
		System.setProperty("de.bmiag.genfw.xpand.codepage", "ISO8859-1");
		
		super.execute();
	}
}
