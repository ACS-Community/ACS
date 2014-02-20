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

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Enumeration;
import java.util.LinkedHashSet;
import java.util.Set;

import org.jacorb.idl.AcsAdapterForOldJacorb;
import org.jacorb.idl.AliasTypeSpec;
import org.jacorb.idl.ConstrTypeSpec;
import org.jacorb.idl.Declaration;
import org.jacorb.idl.Definition;
import org.jacorb.idl.EnumType;
import org.jacorb.idl.IdlSymbol;
import org.jacorb.idl.Interface;
import org.jacorb.idl.InterfaceBody;
import org.jacorb.idl.Member;
import org.jacorb.idl.MemberList;
import org.jacorb.idl.Module;
import org.jacorb.idl.OpDecl;
import org.jacorb.idl.ParamDecl;
import org.jacorb.idl.Spec;
import org.jacorb.idl.StructType;
import org.jacorb.idl.TypeDeclaration;
import org.jacorb.idl.TypeSpec;
import org.jacorb.idl.VectorType;
import org.jacorb.idl.parser;
import org.jacorb.idl.javamapping.JavaMappingGeneratingVisitor;

/**
 * This class gets instantiated by {@link XmlIdlCompiler} and gets called by 
 * {@link org.jacorb.idl.parser#compile(String[], StringWriter)}.
 * It walks the tree of IDL parser nodes and collects information about ACS interfaces and 
 * typedefs, structs and those interfaces that are affected by XML.
 * <p> 
 * It does not rename any tree nodes nor does it generate code.
 * 
 * @author jschwarz
 * @author hsommer
 */
public class JacorbVisitor extends JavaMappingGeneratingVisitor
{
	public static final String XML_ENTITY_STRUCT_NAME = "XmlEntityStruct";
	public static final String ACSCOMPONENT_IDLTYPE = "IDL:alma/ACS/ACSComponent:1.0";
	public static final String ACSOFFSHOOT_IDLTYPE = "IDL:alma/ACS/OffShoot:1.0";
	
	/**
	 * Contains the typedef nodes from the currently parsed IDL file
	 * that unwind to an XmlEntityStruct or a sequence of that.
	 * We use this set only to generate code for the typedefs of this IDL file. 
	 * <p>
	 * This set should not be used elsewhere to determine whether a parameter or struct member is of an xml type.
	 * If those typedefs come from included IDL files, they will not be contained in xmlTypedefs, 
	 * so that we must unwind and check them separately.
	 * @see #visitAlias(AliasTypeSpec) 
	 */
	final Set<AliasTypeSpec> xmlTypedefs = new LinkedHashSet<AliasTypeSpec>();
	
	final Set<StructType> xmlAwareStructs = new LinkedHashSet<StructType>();

	final Set<Interface> xmlAwareIFs = new LinkedHashSet<Interface>();
	
	final Set<Interface> acsCompIFs = new LinkedHashSet<Interface>();
	
	/**
	 * This stack contains the interfaces we are currently visiting, 
	 * which can be more than one interface if an interfaces references another one
	 * and then back to itself, either directly or via a struct that has an interface member. 
	 * This stack is used to break cyclic references.
	 * Note that only interfaces can be forward declared in IDL, so that no other types 
	 * can have circular dependencies.
	 */
	private final Set<Interface> interfaceStack = new LinkedHashSet<Interface>();

	public JacorbVisitor() {
		super();
//		System.out.println("DEBUG: JacorbVisitor created.");
	}

	/** 
	 * The top-level call, from which we start walking the IDL parse tree
	 * using mostly the base class implementation of the tree descend.
	 */
	@Override
	public void visitSpec(Spec spec) {
//		System.out.println("DEBUG: visitSpec called.");
		super.visitSpec(spec);
	}

	@Override
	public void visitModule(Module module) {
		// Skip modules for which we anyway don't generate code.
		// There is no noticeable performance gain, but it helps with debugging.
		// Note that the jacorb idl print methods only skip included interfaces, not modules, unsure why.
		// 
		// TODO: Try out included xml entity typedefs and structs etc in our IDL, 
		// whether visiting them only when processing their own IDL is good enough. It should be.
		
		if (module.is_included() && !generateIncluded()) {
//			System.out.println("Skipping included module " + module.pack_name);
			return;
		}
		super.visitModule(module);
	}

	/** 
	 * We override this to detect all XmlEntityStruct typedefs, even if they do not get used 
	 * as parameters or struct members inside our own IDL file, be it directly or through other typedefs.
	 * The reason is that these typedefs may get included in other IDL files, where the 
	 * respective Holder class is then expected to be found in the jar generated from this IDL.
	 * (Note that in contrast to the normal Idl2Java mapping, we always must generate a Holder class for an Xml typedef, 
	 * e.g. SchedBlockHolder with the castor class field "mycastorpackage.SchedBlock value".)
	 * <p>
	 * This method corresponds to {@link alma.tools.idlgen.IdlTreeManipulator#findXmlTypedefNodes(IdlObject, Set)}
	 * in the OpenORB based implementation. 
	 */
	@Override
	public void visitAlias(AliasTypeSpec alias) {
		if (isOrHasXmlEntityStruct(alias, 0)) {
			xmlTypedefs.add(alias);
		}
	}
	
	@Override
	public void visitStruct(StructType struct) {
		
//		System.out.println("visitStruct called with struct name=" + struct.name());
		
		if (hasXmlEntityStruct(struct, 0)) {
			xmlAwareStructs.add(struct);
		}
	}

	
	@Override
	public void visitInterface(Interface interfce) {
		
//		System.out.println("visitInterface called with interfce.name=" + interfce.name() + ", body=" + interfce.body);

		// Don't need to check if it's an included interface, 
		// because this we do already on the module level.
		
		// Skip interfce if it's not an ACS interface or offshoot.
		// This automatically skips forward declarations of interfaces.
		// (This code corresponds to XmlIdl's class IDLComponentTester.
		//  With JacORB it's much less code because we get all inherited interfaces in one call
		//  instead of only getting the direct parent generation
		//  and also having to collect all interfaces first in an extra step, because of an OpenORB bug.)
		
		String[] superInts = interfce.get_ids();
		boolean isAcsInterface = false;
		
		for (int i = 1; i < superInts.length; i++) {
			if (superInts[i].equals(ACSCOMPONENT_IDLTYPE)) {
				isAcsInterface = true;
				acsCompIFs.add(interfce);
//				System.out.println("I/F "+interfce+" is an ACS component");
				break;
			} 
			else if (superInts[i].equals(ACSOFFSHOOT_IDLTYPE)) {
				isAcsInterface = true;
//				System.out.println("I/F "+interfce+" is an ACS offshoot");
				break;
			}
		}

		if (isAcsInterface) {
			
			// avoid cyclic recursion
			if (interfaceStack.contains(interfce)) {
//				System.out.println("Breaking interface recursion for " + interfce.name());
			}
			else {
				interfaceStack.add(interfce);
				
				// Let the base class do the tree descending via InterfaceBody to OpDecl,
				// where it will be decided if our ACS interface is affected by XML.
				super.visitInterface(interfce);
				
				interfaceStack.remove(interfce);
			}
		}
	}


	/**
	 * We override this method in order to descend not only to operations 
	 * but also to inner structs and typedefs.
	 * @see org.jacorb.idl.InterfaceBody#print(PrintWriter)
	 */
	@Override
	public void visitInterfaceBody(InterfaceBody body) {
		
		// The base impl (only) takes care of descending to the interface's operations
		super.visitInterfaceBody(body);
		
		// Visit structs defined inside the interface.
		// TODO: Can there be the case that none of the operations contain XML and yet we find XML in the inner structs etc,
		//       and if so, must we add the interface to xmlAwareIFs?
		for (Definition def : body.v) {
			Declaration dec = def.get_declaration();
			if (!(dec instanceof OpDecl)) {
				def.accept(this);
			}
		}
	}

	@Override
	public void visitOpDecl(OpDecl op) {
		
		// The interface this operation belongs to 
		Interface interfce = (Interface)op.myInterface;
		
		// Optimization: Skip the operation if the interface has already been marked.
		if (xmlAwareIFs.contains(interfce)) {
			return;
		}
		
		// The operation's return type
		TypeSpec retType = op.opTypeSpec;
		
		// Check if the return type brings in XML
		boolean hasXML = isOrHasXmlEntityStruct(retType, 0);

		if (hasXML) {
			xmlAwareIFs.add(interfce);
			// The operation's return data is a typedef'd XmlEntityStruct (or sequence, nested structs etc, of it).
			hasXML = true;
//			System.out.println("Operation " + interfce.name() + "#" + op.name() + " returns XML-affected data " + retType.name()+ ".");
		}
		else {
			// Descend to the op parameters to check if they bring in XML.
			// As a little optimization that could make debugging easier, we skip checking of parameters if we already found XML in the return type.
			// The next few lines are largely copied from the base class. However, we don't simply call super.visitOpDecl(op) 
			// because in addition we want to link the ParamDecls back to our OpDecl using "setEnclosingSymbol" 
			// (which jacorb for whatever reason has not called during parsing).
			for (Enumeration<?> e = op.paramDecls.elements(); e.hasMoreElements();) {
				ParamDecl param = (ParamDecl) e.nextElement();
				param.setEnclosingSymbol(op);
				param.accept(this);
			}
		}
	}

	
	/**
	 * We override this method to check if a parameter is of an XML-related type
	 * and to mark the respective interface. 
	 * <p>
	 * The check is simple enough so that we could inline it in {@link #visitOpDecl(OpDecl)}, 
	 * but by using visitParamDecl we cleanly follow jacorb's visitor pattern.
	 */
	@Override
	public void visitParamDecl(ParamDecl param) {
		
		OpDecl op = (OpDecl) param.getEnclosingSymbol();
		Interface interfce = (Interface) op.myInterface;
		
		TypeSpec ts = param.paramTypeSpec;
		if (isOrHasXmlEntityStruct(ts, 0)) {
			xmlAwareIFs.add(interfce);
//			System.out.println("Operation " + interfce.name() + "#" + op.name() + " has XML-affected parameter " + param.simple_declarator + ".");
			
			// TODO: Remove this setpackage call because it taints the tree walking with code generation.
			// hard-wired for now to see whether this will work, but it applies only to the typedef, apparently
//			ts.setPackage("dummyentities"); 
//			// and it shows up as "dummyentities.xmltest" in the debugger.
		}
	}

	
	////////////////////////

	/**
	 * Recursively checks if the given type is a typedef'd XmlEntityStruct or sequence of that, 
	 * or if it is a struct that contains such a typedef or a nested struct that contains the XML struct,
	 * or if it is an interface that contains xml types.
	 * <p>
	 * This method descends into interfaces and struct children while resolving sequences and typedefs.
	 * @param depth 0-based counter for recursion depth, to control log output.
	 * @return true if <code>type</code> is an XmlEntityStruct or uses one.
	 */
	private boolean isOrHasXmlEntityStruct(TypeSpec type, int depth) {
		boolean ret = false;
		
		if (type instanceof AliasTypeSpec) {
			// check underlying (aliased) type
			AliasTypeSpec alias = (AliasTypeSpec) type;
			TypeSpec otype = alias.originalType();
			ret = isOrHasXmlEntityStruct(otype, depth + 1);
		}
		else if (type instanceof VectorType) { 
			// check the type that we have a sequence of
			VectorType vector = (VectorType) type;
			ret = isOrHasXmlEntityStruct(vector.elementTypeSpec(), depth + 1);
		}
		else if (type instanceof ConstrTypeSpec) {
			TypeDeclaration decl = ((ConstrTypeSpec) type.typeSpec()).c_type_spec.declaration();
			if (decl instanceof StructType) {
				StructType struct = (StructType) decl;
				if (struct.name().equals(XML_ENTITY_STRUCT_NAME)) {
					// TODO: Consider IDL package as well, not just the struct name "XmlEntityStruct".
					// Allow a cheating property for packages such as "xmltest", but otherwise insist on "xmlentity".
					// String packageName = struct.pack_name;
					
					// End of recursion... Our struct is an XmlEntityStruct
					ret = true;
				}
				else {
					// Iterate over struct children and recurse as needed.
					ret = hasXmlEntityStruct(struct, depth);
				}
			}
			else if (decl instanceof Interface) {
				// Examples: Offshoot used as a struct member, in a typedef, or as an operation parameter
				Interface interfce = (Interface) decl;
				if (interfce.body == null) {
					// A forward declared interface stays in the parse tree as a separate Interface object. 
					// We must resolve the corresponding Interface definition.
					interfce = resolveForwardDecl(interfce);
					// Our forward declared interface may not have been processed yet. 
					// We do it now so that we can rely on the information from "xmlAwareIFs".
					visitInterface(interfce);
				}
				// Now after the call to visitInterface we can trust xmlAwareIFs with regard to our interfce
				if (xmlAwareIFs.contains(interfce)) {
					ret = true;
				}
			}
			else if (decl instanceof EnumType) {
				// enums are not associated with XML. Just leave ret=false
			}
			else {
				System.out.println("*** isOrHasXmlEntityStruct: Unexpected ConstrTypeSpec with TypeDeclaration subtype " + decl.getClass().getSimpleName());
			}
		}

//		if (depth == 0) {
//			System.out.println("isOrHasXmlEntityStruct(" + type.toString() + ") returns " + ret);
//		}
		
		return ret;
	}
	
	/**
	 * Iterates over and recurses a struct's members to check if they are or have XmlEntityStruct types.
	 * <p>
	 * We need this as an auxiliary method to be called from {@link #visitStruct(StructType)}
	 * because there we do not seem to have a TypeSpec available for our struct that we could 
	 * directly pass to {@link #isOrHasXmlEntityStruct(TypeSpec, int)}.
	 * Thus we call this loop of struct members from both methods to avoid code duplication.
	 */
	private boolean hasXmlEntityStruct(StructType struct, int depth) {
		MemberList members = struct.memberlist;
		if (members != null) {
			for (Enumeration<?> e = members.elements(); e.hasMoreElements();) {
				TypeSpec memberType = ((Member) e.nextElement()).type_spec;
				if (isOrHasXmlEntityStruct(memberType, depth + 1)) {
					return true;
				}
			}
		}
		return false;
	}


	/**
	 * If the given interface is instantiated from a forward declaration then
	 * this method returns the corresponding Interface object that contains the actual definition.
	 * Otherwise the same object is returned. 
	 */
	public static Interface resolveForwardDecl(Interface interfce) {
		Interface ret = interfce;
		
		if (interfce.body == null) {
			String fullTypeName = interfce.pack_name + "." + interfce.name(); // method Interface#full_name is not visible
			ret = (Interface) ((ConstrTypeSpec) AcsAdapterForOldJacorb.getFromTypeMap(fullTypeName)).c_type_spec; 
		}
		
		return ret;
	}
	
	
	////////////////////////

	/**
	 * We must "repeat" this method here because {@link IdlSymbol#generateIncluded()}
	 * is not publicly visible. 
	 */
	private boolean generateIncluded() {
		return ( parser.generateIncluded() && !parser.getInhibitionState() );
	}
	

	/**
	 * Overridden to avoid the annoying log that the base impl produces.
	 */
	@Override
	public void visitDeclaration(Declaration declaration) {
//		System.out.println("visitDeclaration: skipping " + declaration.getClass().getName());
	}

	/**
	 * We override this method only to fix a bug with an endless loop in the base class.
	 * <p>
	 * Note that the normal jacorb run does not call EnumType#accept which calls the broken "visitEnum", 
	 * but instead it calls EnumType#print, so that the bug in JavaMappingGeneratingVisitor goes unnoticed.
	 */
	@Override
	public void visitEnum(EnumType enumType) {
//		ps.println("DEBUG: skipping visitEnum(" + enumType.typeName() + ").");
	}
	
}
