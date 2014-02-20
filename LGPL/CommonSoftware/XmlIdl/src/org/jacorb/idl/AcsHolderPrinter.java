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

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;

import alma.tools.idlgen.XmlIdlCompiler;

/**
 * Prints the holder class for xml typedefs and structs.
 * 
 * @author hsommer
 */
public class AcsHolderPrinter
{
	/**
	 * Code copied from {@link AliasTypeSpec#print(PrintWriter)} and private method
	 * {@link AliasTypeSpec#printHolderClass(String, PrintWriter)} and adjusted for ACS xml purposes.
	 * @throws IOException 
	 */
	public void printHolderClass(String structOrTypedef, String idlName, 
			String holderClassName, String jpackage, String heldTypeName) throws IOException 
	{
		File f = XmlIdlCompiler.createFile(jpackage, holderClassName);
		
		if (!GlobalInputStream.isMoreRecentThan(f)) {
			return;
		}
		
		PrintWriter ps = new PrintWriter(new java.io.FileWriter(f));

		
		if (!jpackage.equals("")) {
			ps.println("package " + jpackage + ";");
			ps.println();
		}

		ps.println("/**");
		ps.println(" * Holder class for IDL " + structOrTypedef + " " + idlName);
		ps.println(" * ");
		ps.println(" * @author ACS xml transparency layer generator");
		ps.println(" */");

		ps.println("public" + parser.getFinalString() + " class " + holderClassName);
		ps.println("{");

		ps.println("\tpublic " + heldTypeName + " value;" + Environment.NL);

		ps.println("}");
		
		ps.close();
	}


}
