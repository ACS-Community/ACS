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
package alma.tools.entitybuilder;

/**
 * @author hsommer
 * created 31-Jul-2002 14:41:25
 * $Id: BindingException.java,v 1.7 2002/09/02 16:03:22 vltsccm Exp $
 */
public class BindingException extends Exception
{
	/**
	 * Constructor for BindingException.
	 */
	public BindingException()
	{
		super();
	}

	/**
	 * Constructor for BindingException.
	 * @param message
	 */
	public BindingException(String message)
	{
		super(message);
	}

	/**
	 * Constructor for BindingException.
	 * @param message
	 * @param cause
	 */
	public BindingException(String message, Throwable cause)
	{
		super(message, cause);
	}

	/**
	 * Constructor for BindingException.
	 * @param cause
	 */
	public BindingException(Throwable cause)
	{
		super(cause);
	}

}
