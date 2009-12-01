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
package alma.acs.genfw.runtime.sm;

/**
 * @author hsommer
 * @deprecated since ACS 6.0, will be removed with 6.1 or 7.0. Use the generated alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx instead.
 * created Apr 15, 2004 4:45:45 PM
 */
public class AcsStateIllegalEventException extends Exception
{

	private static final long serialVersionUID = 7796250422838730080L;

	/**
	 * 
	 */
	public AcsStateIllegalEventException() {
		super();
	}

	/**
	 * @param message
	 */
	public AcsStateIllegalEventException(String message) {
		super(message);
	}

	/**
	 * @param cause
	 */
	public AcsStateIllegalEventException(Throwable cause) {
		super(cause);
	}

	/**
	 * @param message
	 * @param cause
	 */
	public AcsStateIllegalEventException(String message, Throwable cause) {
		super(message, cause);
	}

}
