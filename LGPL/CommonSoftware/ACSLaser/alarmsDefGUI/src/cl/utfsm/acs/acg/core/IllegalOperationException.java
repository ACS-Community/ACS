/*
 *    ALMA - Atacama Large Millimeter Array
 *    (c) Universidad Tecnica Federico Santa Maria, 2009
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
package cl.utfsm.acs.acg.core;

/**
 * The <code>IllegalOperationException<code> exception should be used
 * when illegal actions are going to be taken over a set of
 * objects of the Alarm System configuration.
 * 
 * The following are the constraints for the Alarm System configuration:
 * <ul>
 *  <li>A Category cannot be deleted if it is being currently used by a Fault Family</li>
 *  <li>A Source cannot be deleted if it is being currently used by a Fault Family</li>
 *  <li>A Fault Family cannot be deleted if it is part of a Reduction Rule,
 *      actions as the parent of the rule, or as any kind of child.</li>
 *  <li>A Fault Member cannot be deleted if it is part of a Reduction Rule,
 *      actions as the parent of the rule, or as any kind of child.</li>
 *  <li>A Fault Code cannot be deleted if it is part of a Reduction Rule,
 *      actions as the parent of the rule, or as any kind of child.</li>
 * </ul>
 * 
 * When a new unique element is added to a object group (Category, Source, Fault Family and so),
 * and it already exists, this exception should also be used. For instance,
 * if a source "SOURCE1" already exists in the configuration, and it is added
 * through the <code>SourceManager</code>, this exception should be risen.
 * 
 * @author rtobar
 *
 */
public class IllegalOperationException extends Exception {

	private static final long serialVersionUID = -6131937452105181320L;

	/**
	 * Default constructor
	 */
	public IllegalOperationException() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * @param message
	 */
	public IllegalOperationException(String message) {
		super(message);
		// TODO Auto-generated constructor stub
	}

	/**
	 * @param cause
	 */
	public IllegalOperationException(Throwable cause) {
		super(cause);
		// TODO Auto-generated constructor stub
	}

	/**
	 * @param message
	 * @param cause
	 */
	public IllegalOperationException(String message, Throwable cause) {
		super(message, cause);
		// TODO Auto-generated constructor stub
	}

}
