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
 
package alma.exmplCompHelpGen.SecondaryComponentImpl;

import java.util.logging.Logger;

import org.omg.PortableServer.Servant;
import alma.ACS.ACSComponentOperations;
import alma.acs.component.ComponentLifecycle;
import alma.acs.container.ComponentHelper;
import alma.exmplCompHelpGen.SecondaryComponentOperations;
import alma.exmplCompHelpGen.SecondaryComponentPOATie;
import alma.exmplCompHelpGen.SecondaryComponentImpl.SecondaryComponentImpl;

/**
 * Component helper class. 
 * Generated for convenience, but can be modified by the component developer. 
 * Must therefore be treated like any other Java class (CVS, ...). 
 * <p>
 * @author alma-component-helper-generator-tool
 */
public class SecondaryComponentComponentHelper extends ComponentHelper
{
	/**
    * Constructor
	* @param containerLogger logger used only by the parent class.
	*/
	public SecondaryComponentComponentHelper(Logger containerLogger)
	{
		super(containerLogger);
	}
	
    /**
	* @see alma.acs.container.ComponentHelper#_createComponentImpl()
	*/
	protected ComponentLifecycle _createComponentImpl()
	{
		return new SecondaryComponentImpl();
	}
	
	/**
	* @see alma.acs.container.ComponentHelper#_getPOATieClass()
	*/
	protected Class<? extends Servant> _getPOATieClass()
	{
		return SecondaryComponentPOATie.class;
	}

	/**
	* @see alma.acs.container.ComponentHelper#getOperationsInterface()
	*/
	protected Class<? extends ACSComponentOperations> _getOperationsInterface()
	{
		return SecondaryComponentOperations.class;
	}

}
