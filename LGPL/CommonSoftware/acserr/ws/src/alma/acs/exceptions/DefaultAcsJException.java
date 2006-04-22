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
package alma.acs.exceptions;

import org.omg.CORBA.UserException;

import alma.ACSErr.ErrorTrace;


/**
 * Exception used as a default where an <code>ErrorTrace</code> object
 * can't be converted to a genuine Java exception for one of the
 * following reasons:
 * <ul>
 * <li>missing Java exception classname property in <code>ErrorTrace</code>, 
 * 		because the original exception was not thrown in Java.
 * <li>original Java exception can't be reconstructed in our VM, e.g.
 * 		because its class is not on our classpath.
 * <li>original Java exception was not a subclass of <code>AcsJException</code>,
 * 		e.g. any of the <code>java.lang</code> exceptions.
 *    	In order to not loose the information contained in
 * 		<code>ErrorTrace</code> or <code>AcsJException</code>, we reconstruct
 * 		the non-ACS exception as a <code>DefaultAcsJException</code>,
 * 		stating the original exception class in the message string.
 * </ul>  
 * 
 * @author hsommer Jun 20, 2003 5:47:52 PM
 */
public class DefaultAcsJException extends AcsJException
{
	/**
	 * @param message
	 */
	public DefaultAcsJException(String message)
	{
		super(message);
	}

	/**
	 * @param etCause
	 */
	DefaultAcsJException(ErrorTrace etCause) {
		super(etCause);
	}

	/**
	 * Unlike other exception classes, this class does not correspond directly
	 * to an IDL defined type safe exception.
	 * Therefore, the returned UserException is an instance of 
	 * <code>alma.ACSErr.ACSException</code>.
	 * 
	 * @see alma.acs.exceptions.AcsJException#toCorbaException()
	 */
	public UserException toCorbaException()
	{
		return getACSException();
	}

	/* (non-Javadoc)
	 * @see alma.acs.exceptions.AcsJException#getErrorType()
	 */
	protected int getErrorType()
	{
		// todo or null or some better type...?
		return 0;
	}

	/* (non-Javadoc)
	 * @see alma.acs.exceptions.AcsJException#getErrorCode()
	 */
	protected int getErrorCode()
	{
		return 0;
	}

}
