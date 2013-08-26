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
/**
 *      @author Julio Araya (jaray[at]alumnos.inf.utfsm.cl) &
 *      Nicolas Troncoso (ntroncos[at]alumnos.inf.utfsm.cl)
 **/

package cl.utfsm.samplingSystemUI.core;

import java.lang.Exception;

/**
* Custom exception class, not much more than that
*/
public class SamplingManagerException extends Exception {

	private static final long serialVersionUID = 2L;

	/**
	* Generic constructor, same as Exception
	* @see Exception
	*/
	public SamplingManagerException() {
		super();
	}

	/**
	* Adds a message to the Generic Exception
	* @param msg Message to be added to the regular Exception
	*/
	public SamplingManagerException(String msg) {
		super(msg+"\n");
	}

	/**
	* Another extension of Exception, now also adds a Generic Exception to the mix
	* @param msg Message to be added to the Exceotion
	* @param e Generic Exception to be attached
	*/
	public SamplingManagerException(String msg, Exception e) {
		super(msg+"\n",e);
	}
}
