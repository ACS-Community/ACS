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

import java.io.IOException;
import java.io.PrintWriter;

import alma.tools.idlgen.AcsXmlNamingExpert;

/**
 * Prints the xml transparency layer code for an xml binding class typedef. 
 * This is just one "Holder" class. Note that the standard IDL-Java mapping does not generate any code for the typedefs.
 * 
 * @author hsommer
 */
public class AcsTypedefPrinter
{
	private final AliasTypeSpec alias;
	private final AcsXmlNamingExpert namingExpert;

	public AcsTypedefPrinter(AliasTypeSpec alias, AcsXmlNamingExpert namingExpert) {
		this.alias = alias;
		this.namingExpert = namingExpert;
	}

	/**
	 * Code copied from {@link AliasTypeSpec#print(PrintWriter)} and private method
	 * {@link AliasTypeSpec#printHolderClass(String, PrintWriter)} and adjusted for ACS xml purposes.
	 * @throws IOException 
	 */
	public void printHolderClass() throws IOException {
		
		String idlName = alias.full_name();
		String holderClassName = namingExpert.getHolderClassNameForXmlTypedef(alias);
		String heldTypeName = namingExpert.getJavaTypeForXmlTypedef(alias);

		// full_name takes care of adding "Package" for typedefs defined inside IDL interfaces
		String fullName = alias.full_name();
		String jpackage = fullName.substring(0, fullName.lastIndexOf('.'));

		AcsHolderPrinter hp = new AcsHolderPrinter();
		hp.printHolderClass("typedef", idlName, holderClassName, jpackage, heldTypeName);
	}

}
