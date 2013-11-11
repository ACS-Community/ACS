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
package org.jacorb.idl;

/**
 * This class is located in the jacorb package to make protected / package private fields and methods visible with Jacorb 2.2.4 
 * that will become public in Jacorb 3.3. 
 * Therefore this adapter can be removed once we upgrade to jacorb 3.3 (http://ictjira.alma.cl/browse/ICT-1608)
 * 
 * @author hsommer
 */
public class AcsAdapterForOldJacorb
{
	public static String getParserOutDir() {
		return parser.out_dir;
	}

	public static TypeSpec getFromTypeMap(String name) {
		return TypeMap.map(name);
	}

	public static String getEnvironmentNL() {
		//     static final String NL = System.getProperty("line.separator");
		return System.getProperty("line.separator");
	}
}
