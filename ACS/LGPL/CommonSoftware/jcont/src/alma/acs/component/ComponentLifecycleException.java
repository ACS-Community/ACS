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
package alma.acs.component;

/**
 * @author hsommer Jun 5, 2003 3:12:21 PM
 */
public class ComponentLifecycleException extends Exception
{

	private static final long serialVersionUID = -8872632299416257614L;

	/**
	 * 
	 */
	public ComponentLifecycleException()
	{
		super();
	}

	/**
	 * @param message
	 */
	public ComponentLifecycleException(String message)
	{
		super(message);
	}

	/**
	 * @param message
	 * @param cause
	 */
	public ComponentLifecycleException(String message, Throwable cause)
	{
		super(message, cause);
	}

	/**
	 * @param cause
	 */
	public ComponentLifecycleException(Throwable cause)
	{
		super(cause);
	}

}
