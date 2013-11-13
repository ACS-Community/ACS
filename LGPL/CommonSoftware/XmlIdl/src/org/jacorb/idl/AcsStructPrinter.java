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
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import alma.tools.idlgen.AcsXmlNamingExpert;
import alma.tools.idlgen.JacorbVisitor;
import alma.tools.idlgen.XmlIdlCompiler;

/**
 * Prints the xml transparency layer code for an IDL struct affected by xml binding. 
 * This is one class for the struct, and one corresponding "Holder" class.
 * 
 * @author hsommer
 */
public class AcsStructPrinter
{
	private final StructType struct;
	private final AcsXmlNamingExpert namingExpert;
	
	/**
	 * This set is used to check whether a typedef'd struct member deals with xml.
	 */
	private final Set<AliasTypeSpec> xmlTypedefs;

	/**
	 * This set is used to check whether a nested struct deals with xml.
	 */
	private final Set<StructType> xmlAwareStructs;
	
	/**
	 * This set is used to check whether an interface (offshoot etc) member of a struct deals with xml.
	 */
	private final Set<Interface> xmlAwareIFs;

	public AcsStructPrinter(StructType struct, 
			Set<AliasTypeSpec> xmlTypedefs, Set<StructType> xmlAwareStructs, Set<Interface> xmlAwareIFs,
			AcsXmlNamingExpert namingExpert) {
		this.struct = struct;
		this.namingExpert = namingExpert;
		this.xmlTypedefs = xmlTypedefs;
		this.xmlAwareStructs = xmlAwareStructs;
		this.xmlAwareIFs = xmlAwareIFs;
	}

	/**
	 * Exposes {@link StructType#javaName()} to other packages,
	 * to be used by {@link AcsXmlNamingExpert}.
	 */
	public static String java_name(StructType struct) {
		return struct.javaName();
	}
	
	
	/**
	 * @throws IOException 
	 * @see StructType#printStructClass(String, PrintWriter)
	 */
	public void printStructClass() throws IOException {
		String idlName = struct.full_name();
		String className = namingExpert.getClassNameForStruct(struct);

		// full_name takes care of adding "Package" for typedefs defined inside IDL interfaces
		String fullName = struct.full_name();
		String jpackage = fullName.substring(0, fullName.lastIndexOf('.'));

		File f = XmlIdlCompiler.createFile(jpackage, className);
		
		if (!GlobalInputStream.isMoreRecentThan(f)) {
			return;
		}
		
		PrintWriter ps = new PrintWriter(new java.io.FileWriter(f));

		if (!jpackage.equals("")) {
			ps.println("package " + jpackage + ";");
			ps.println();
		}

		ps.println("/**");
		ps.println(" * Class for IDL struct " + idlName);
		ps.println(" * ");
		ps.println(" * @author ACS xml transparency layer generator");
		ps.println(" */");

		ps.println("public" + parser.getFinalString() + " class " + className);
		ps.println("{");

		if (struct.memberlist != null) {

			// collect member types and names
			
			class StructMember {
				String typeName;
				String memberName;
				public StructMember(String typeName, String memberName) {
					this.typeName = typeName;
					this.memberName = memberName;
				}
			}
			List<StructMember> memberList = new ArrayList<StructMember>();
			for (Enumeration e = struct.memberlist.v.elements(); e.hasMoreElements();) {
				Member m = (Member) e.nextElement();
				TypeSpec tspec = m.type_spec;
				String acsTypeName = null;
				if (tspec instanceof AliasTypeSpec) {
					if (xmlTypedefs.contains(tspec)) {
						acsTypeName = namingExpert.getJavaTypeForXmlTypedef((AliasTypeSpec)tspec);
					}
				}
				else if (tspec instanceof ConstrTypeSpec) { 
					TypeDeclaration decl = ((ConstrTypeSpec)tspec).c_type_spec;
					
					if (decl instanceof StructType && xmlAwareStructs.contains(decl)) {
						acsTypeName = namingExpert.getJavaTypeForXmlStruct((StructType)((ConstrTypeSpec)tspec).c_type_spec.declaration());
					}
					else if (decl instanceof Interface) {
						Interface interfce = JacorbVisitor.resolveForwardDecl((Interface)decl);
						if (xmlAwareIFs.contains(interfce)) {
							acsTypeName = namingExpert.getJavaTypeForXmlInterface(interfce);
						}
					}
				}
				// default member type output:
				if (acsTypeName == null) {
					// Note that Member#member_print is fancier, but its fanciness apparently does not apply 
					// to our case because StructType#printStructClass later gets the data for the constructors 
					// in the same simple way that we do here.
					acsTypeName = tspec.toString();
				}
				memberList.add(new StructMember(acsTypeName, m.declarator.name()));
			}

			// print member declarations
			for (StructMember structMember : memberList) {
				ps.print("\tpublic " + structMember.typeName + " " + structMember.memberName + ";");
				ps.println();
			}
			ps.println();

			// Default constructor
			ps.println("\tpublic " + className + "() {}");

			// All-arg constructor
			ps.print("\tpublic " + className + "(");
			for (Iterator<StructMember> iterator = memberList.iterator(); iterator.hasNext();) {
				StructMember structMember = iterator.next();
				ps.print(structMember.typeName + " " + structMember.memberName);
				if (iterator.hasNext()) {
					ps.print(", ");
				}
			}
			ps.println(")");

			// All-arg constructor body 
			ps.println("\t{");
			for (Iterator<StructMember> iterator = memberList.iterator(); iterator.hasNext();) {
				StructMember structMember = iterator.next();
				ps.print("\t\tthis.");
				ps.print(structMember.memberName);
				ps.print(" = ");
				ps.println(structMember.memberName + ";");
			}
			ps.println("\t}");
		}
		ps.println("}");

		ps.close();
	}
	
	
	public void printHolderClass() throws IOException {
		
		String idlName = struct.full_name();
		String holderClassName = namingExpert.getHolderClassNameForStruct(struct);
		String heldTypeName = namingExpert.getJavaTypeForXmlStruct(struct);

		// full_name takes care of adding "Package" for typedefs defined inside IDL interfaces
		String fullName = struct.full_name();
		String jpackage = fullName.substring(0, fullName.lastIndexOf('.'));

		AcsHolderPrinter hp = new AcsHolderPrinter();
		hp.printHolderClass("struct", idlName, holderClassName, jpackage, heldTypeName);
	}

}
