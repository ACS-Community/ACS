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

import java.io.File;
import java.util.List;

import genfwutil.ant.plugin.GeneratorPlugin;

/**
 * @author hsommer
 * created Jul 2, 2004 3:56:23 PM
 */
public class AlmaMetamodelPlugin extends GeneratorPlugin
{
	public AlmaMetamodelPlugin()
	{
		super();
	}

	public List contributeInstantiators() {
		String xmlMap = getProperty("XMLMAP");
		String model = getProperty("MODEL");
		String metaMap = getProperty("METAMAP");
		String toolAdapter = getProperty("TOOLADAPTER");

		return makeList( new AlmaXMIInstantiator( new File(model), new File(xmlMap), new File(metaMap), toolAdapter) );
	}	

}
