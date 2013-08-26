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


public class AcsInformationException extends Exception {

	private static final long serialVersionUID = 7751725699360884525L;

	public AcsInformationException() {
		super();
	}

	/**
	* Adds a String to the generic ACSInformationException
	*
	* @param msg The String to be added
	**/
	public AcsInformationException(String msg) {
		super(msg+"\n");
	}

	/**
	* Adds a message to the given Exception, then passes it to the generic ACSInformationException
	*
	* @param msg The string to be added
	* @param e Exception to be passed to the parent
	**/
	public AcsInformationException(String msg, Exception e) {
		super(msg+"\n",e);
	}
}
