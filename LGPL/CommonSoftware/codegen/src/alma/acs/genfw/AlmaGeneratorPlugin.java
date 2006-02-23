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

import genfwutil.ant.plugin.GeneratorPlugin;
import genfwutil.ant.plugin.NonAttachedTemplatesContribution;

import java.util.ArrayList;
import java.util.List;

/**
 * @author hsommer
 * created Jul 2, 2004 3:54:50 PM
 */
public class AlmaGeneratorPlugin extends GeneratorPlugin
{

	/**
	 * 
	 */
	public AlmaGeneratorPlugin()
	{
		super();
		// declare dependency on metamodel plugin
		addRequiredPluginID( AlmaMetamodelPlugin.class.getName() );
	}

	public List contributeInvokers() {
		List l = new ArrayList();
//		l.add( new UtilInvoker() );
//		l.add( new CounterInvoker() );
//		l.add( new ElementFlagInvoker() );
		return l;
	}
	
	public List contributeTemplates() {
	    String rootDir = getProperty("TEMPLATES.ROOT");
	    String rootTemplate = "Root::Root";
	    String metaSelect = "Model";
	    NonAttachedTemplatesContribution c = new NonAttachedTemplatesContribution(rootDir, rootTemplate, metaSelect);
		c.setTemplateManagerClass( genfwutil.templateManager.KeyItemDirectoryTemplateManager.class );
		return makeList(c);
	}	

}
