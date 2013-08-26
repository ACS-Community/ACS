/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.demo.LampCallbackImpl;

import java.util.logging.Logger;

import alma.acs.component.ComponentLifecycle;
import alma.acs.container.ComponentHelper;
import alma.demo.LampCallbackOperations;
import alma.demo.LampCallbackPOATie;

/**
 * @author alma-component-helper-generator-tool
 */
public class LampCallbackHelper extends ComponentHelper
{
	/**
	 * Passes a logger to the callback object.
	 * @param containerLogger
	 */
	public LampCallbackHelper(Logger containerLogger)
	{
		super(containerLogger);
	}

	/**
	 * Gets an instance of the implementation class of the LampCallback component.
	 * @return ComponentLifecycle
	 * @see alma.acs.container.ComponentHelper#_createComponentImpl()
	 */
	protected ComponentLifecycle _createComponentImpl()
	{
		return new LampCallbackImpl();
	}

	/**
	 * Gets an instance of the POATie class of the LampCallback component.
	 * @return Class
	 * @see alma.acs.container.ComponentHelper#_getPOATieClass()
	 */
	protected Class _getPOATieClass()
	{
		return LampCallbackPOATie.class;
	}

	/**
	 * Gets an instance of the operations of the LampAccess component.
	 * @return Class
	 * @see alma.acs.container.ComponentHelper#getOperationsInterface()
	 */
	protected Class _getOperationsInterface()
	{
		return LampCallbackOperations.class;
	}
}
