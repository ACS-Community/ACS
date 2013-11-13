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
package alma.tools.idlgen;

import java.util.HashMap;
import java.util.Map;

import org.jacorb.idl.AcsInterfacePrinter;
import org.jacorb.idl.AcsStructPrinter;
import org.jacorb.idl.AliasTypeSpec;
import org.jacorb.idl.ConstrTypeSpec;
import org.jacorb.idl.Interface;
import org.jacorb.idl.StructType;
import org.jacorb.idl.TypeDeclaration;
import org.jacorb.idl.TypeSpec;
import org.jacorb.idl.VectorType;

/**
 * See old alma.tools.idlgen.NamingConventions
 * 
 * @author hsommer
 */
public class AcsXmlNamingExpert
{
	private Map<String, String> struct2JavaMap = new HashMap<String, String>();
	
	public static final String suffix = "J";
	
	////////////////////////////////////////////////////////////////////////////////////
	///////////////////////// Java binding classes /////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Sets the mappings from <code>XmlEntityStruct</code> typedefs to 
	 * Java binding classes.
	 * For example, <code>ObsProposal=alma.xmljbind.test.obsproposal.ObsProposal</code>
	 * would substitute the Java class on the right side for the IDL type ObsProposal, 
	 * which is defined as <code>typedef xmlentity::XmlEntityStruct ObsProposal;</code> 
	 * in the IDL file. 
	 * @param mappings  concatenated mappings, separated by ';'
	 */
	public void setIdlStruct2JavaBindingClassMappings(String mappings)
	{
		if (mappings == null) {
			return;
		}
		
		for (String mapping : mappings.split(";")) {
			String trimmedMapping = mapping.trim();
			int sepPos = trimmedMapping.indexOf('=');
			if (sepPos > 0) {
				String structName = trimmedMapping.substring(0, sepPos).trim();
				String bindingClassName = trimmedMapping.substring(sepPos + 1).trim();
				
				struct2JavaMap.put(structName, bindingClassName);
			}
		}
	}
	
	public String getBindingClassName(String idlTypedefName) {
		return struct2JavaMap.get(idlTypedefName);
	}

	
	////////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////// Structs /////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////

	public String getClassNameForStruct(StructType struct) {
		return ( struct.name() + suffix );
	}
	
	public String getHolderClassNameForStruct(StructType struct) {
		String className = getClassNameForStruct(struct);
		className += "Holder";
		return className;
	}
	
	public String getJavaPackageForStruct(StructType struct) {
		// java_name or full_name takes care of adding "Package" for typedefs defined inside IDL interfaces
		// These methods are package private for jacorb so that we call them through our own class in that package.
		String fullName = AcsStructPrinter.java_name(struct);// full_name();
		int index = fullName.lastIndexOf('.');
		String jpackage = null;
		if (index > 0) {
			jpackage = fullName.substring(0, fullName.lastIndexOf('.'));
		}
		else {
			jpackage = fullName;
		}
		return jpackage;
	}
	
	/**
	 * Gets the fully qualified name of the java class that represents the struct
	 * in the world of "J" classes.
	 */
	public String getJavaTypeForXmlStruct(StructType struct) {
		String ret = getJavaPackageForStruct(struct) + '.' + getClassNameForStruct(struct);
		return ret;
	}

	
	////////////////////////////////////////////////////////////////////////////////////
	///////////////////////////////// Interfaces ///////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Currently assumes that the trailing "J" has already been appended 
	 * to the name of the provided <code>interfce</code>.
	 * @TODO Change the logic to not rename interfaces in the parse tree and add suffix here to the interface name.
	 */
	public String getClassNameForInterface(Interface interfce) {
//		return ( interfce.name() + suffix );
		return ( interfce.name());
	}
	
	public String getJavaPackageForInterface(Interface interfce) {
		String fullName = AcsInterfacePrinter.java_name(interfce);
		int index = fullName.lastIndexOf('.');
		String jpackage = null;
		if (index > 0) {
			jpackage = fullName.substring(0, fullName.lastIndexOf('.'));
		}
		else {
			jpackage = fullName;
		}
		return jpackage;
	}
	

	public String getJavaTypeForXmlInterface(Interface interfce) {
		String ret = getJavaPackageForInterface(interfce) + '.' + getClassNameForInterface(interfce);
		return ret;
	}


	
	////////////////////////////////////////////////////////////////////////////////////
	////////////////////////////////// Typedefs ////////////////////////////////////////
	////////////////////////////////////////////////////////////////////////////////////
	
	/**
	 * TODO: Try to reuse this code for the interface and struct code generation.
	 *       Also remove code duplication with JacorbVisitor#isOrHasXmlEntityStruct
	 */
	public String getClassNameForXmlTypedef(AliasTypeSpec alias) {
		
		String className = alias.name();
		
		XmlTypedefInfo info = new XmlTypedefInfo();
		checkXmlTypedef(alias, info);

		if (!info.isXmlEntityStruct || info.isSequence) {
			className += suffix;
		}
		
		return className;
	}
	
	/**
	 * TODO: Try to reuse this code for the interface and struct code generation.
	 *       Also remove code duplication with JacorbVisitor#isOrHasXmlEntityStruct
	 */
	public String getHolderClassNameForXmlTypedef(AliasTypeSpec alias) {
		
		String className = getClassNameForXmlTypedef(alias);
		className += "Holder";
		return className;
	}
	
	/**
	 * Gets the fully qualified name of the java class that represents the typedef
	 * in the world of "J" classes, that is, either a binding class or a class
	 * that represents an xml affected struct or interface.
	 */
	public String getJavaTypeForXmlTypedef(AliasTypeSpec alias) {
		
		XmlTypedefInfo info = new XmlTypedefInfo();
		checkXmlTypedef(alias, info);

		if (info.isSequence) {
			info.javaType += "[]";
		}
		return info.javaType;
	}
	
	
	/**
	 * Auxiliary class to return multiple fields from {@link #checkXmlTypedef}.
	 */
	private static class XmlTypedefInfo {
		boolean isXmlEntityStruct = false;
		boolean isSequence = false;
		
		/**
		 * Fully qualified name of the "J" class.
		 */
		String javaType;
	}
	
	/**
	 * TODO: Try to reuse this type printing code for the interface and struct code generation.
	 *       Also remove code duplication with JacorbVisitor#isOrHasXmlEntityStruct
	 */
	private void checkXmlTypedef(AliasTypeSpec alias, XmlTypedefInfo info) {

		TypeSpec orgType = alias.originalType();
		if (orgType instanceof VectorType) {
			info.isSequence = true;
			// Continue by checking the type that we have a sequence of
			VectorType vector = (VectorType) orgType;
			orgType = vector.elementTypeSpec();
		}
		
		if (orgType instanceof AliasTypeSpec) {
			checkXmlTypedef((AliasTypeSpec)orgType, info);
		}
		else if (orgType instanceof ConstrTypeSpec) {
			TypeDeclaration decl = ((ConstrTypeSpec) orgType.typeSpec()).c_type_spec.declaration();
			if (decl instanceof StructType && ((StructType) decl).name().equals(JacorbVisitor.XML_ENTITY_STRUCT_NAME)) {
				// alias is directly a typedef'd XmlEntityStruct. We use the xml binding class name.
				info.isXmlEntityStruct = true;
				info.javaType = getBindingClassName(alias.name());
			}
			else {
				// It is some user-defined struct or interface that contains xml types.
				TypeDeclaration decl2 = ((ConstrTypeSpec) orgType).c_type_spec;
				if (decl2 instanceof StructType) {
					StructType struct = (StructType) decl2;
					info.javaType = getJavaTypeForXmlStruct(struct);
				}
				else if (decl2 instanceof Interface) {
					Interface interfce = (Interface) decl2;
					info.javaType = decl2.typeName(); // TODO: convert to "J", prepend package
				}
			}
		}
	}

}
