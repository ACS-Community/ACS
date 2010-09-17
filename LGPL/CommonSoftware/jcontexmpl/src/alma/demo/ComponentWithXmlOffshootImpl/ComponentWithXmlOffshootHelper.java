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

package alma.demo.ComponentWithXmlOffshootImpl;

import java.util.logging.Logger;

import org.omg.PortableServer.Servant;
import alma.ACS.ACSComponentOperations;
import alma.acs.component.ComponentLifecycle;
import alma.acs.container.ComponentHelper;
import alma.demo.ComponentWithXmlOffshootOperations;
import alma.demo.ComponentWithXmlOffshootPOATie;
import alma.demo.ComponentWithXmlOffshootImpl.ComponentWithXmlOffshootImpl;
import alma.demo.ComponentWithXmlOffshootJ;

/**
 * Component helper class. 
 * Generated for convenience, but can be modified by the component developer. 
 * Must therefore be treated like any other Java class (CVS, ...). 
 * <p>
 * To create an entry for your component in the Configuration Database, 
 * copy the line below into a new entry in the file $ACS_CDB/MACI/Components/Components.xml 
 * and modify the instance name of the component and the container: 
 * <p>
 * Name="COMPONENTWITHXMLOFFSHOOT_1" Code="alma.demo.ComponentWithXmlOffshootImpl.ComponentWithXmlOffshootHelper" Type="IDL:alma/demo/ComponentWithXmlOffshoot:1.0" Container="frodoContainer" ImplLang="java"
 * <p>
 * @author alma-component-helper-generator-tool
 */
public class ComponentWithXmlOffshootHelper extends ComponentHelper
{
	/**
	 * Constructor
	 * @param containerLogger logger used only by the parent class.
	 */
	public ComponentWithXmlOffshootHelper(Logger containerLogger)
	{
		super(containerLogger);
	}

	/**
	* @see alma.acs.container.ComponentHelper#_createComponentImpl()
	*/
	protected ComponentLifecycle _createComponentImpl()
	{
		return new ComponentWithXmlOffshootImpl();
	}

	/**
	* @see alma.acs.container.ComponentHelper#_getPOATieClass()
	*/
	protected Class<? extends Servant> _getPOATieClass()
	{
		return ComponentWithXmlOffshootPOATie.class;
	}

	/**
	* @see alma.acs.container.ComponentHelper#getOperationsInterface()
	*/
	protected Class<? extends ACSComponentOperations> _getOperationsInterface()
	{
		return ComponentWithXmlOffshootOperations.class;
	}

	/**
	* @see alma.acs.container.ComponentHelper#getInternalInterface()
	*/
	protected Class<?> getInternalInterface()
	{
		return ComponentWithXmlOffshootJ.class;
	}

}
