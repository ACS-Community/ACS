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
package alma.acs.component.dynwrapper;

/**
 * @author heiko
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */
public class DynWrapperException extends Exception
{

	private static final long serialVersionUID = -9024977466121170718L;

	/**
	 * Constructor for DynWrapperException.
	 */
	public DynWrapperException()
	{
		super();
	}

	/**
	 * Constructor for DynWrapperException.
	 * @param message
	 */
	public DynWrapperException(String message)
	{
		super(message);
	}

	/**
	 * Constructor for DynWrapperException.
	 * @param message
	 * @param cause
	 */
	public DynWrapperException(String message, Throwable cause)
	{
		super(message, cause);
	}

	/**
	 * Constructor for DynWrapperException.
	 * @param cause
	 */
	public DynWrapperException(Throwable cause)
	{
		super(cause);
	}

}
