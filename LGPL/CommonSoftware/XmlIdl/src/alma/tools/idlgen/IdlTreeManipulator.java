/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
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
package alma.tools.idlgen;

import java.util.HashSet;
import java.util.Set;
import java.util.Vector;

import org.openorb.compiler.IdlCompiler;
import org.openorb.compiler.object.IdlIdent;
import org.openorb.compiler.object.IdlInterface;
import org.openorb.compiler.object.IdlObject;
import org.openorb.compiler.object.IdlStruct;
import org.openorb.compiler.object.IdlTypeDef;
import org.openorb.compiler.parser.IdlType;


/**
 * The IDL parse tree gets manipulated here, so that later the original 
 * OpenORB Java code output routines 
 * from <code>IdlToJava</code> are used as much as possible.
 * 
 * @author hsommer 
 */
public class IdlTreeManipulator 
{
	public static final String XML_WRAPPER_STRUCT_NAME = "XmlEntityStruct";
	
	private NamingConventions m_namingConventions;
	private Set<IdlObject> translatedObjects = new HashSet<IdlObject>();

	public IdlTreeManipulator(NamingConventions namingConventions)
	{
		m_namingConventions = namingConventions;
	}
	
	
	
	/**
	 * Searches the tree underneath <code>node</code> for typedefs 
	 * based on XML_WRAPPER_STRUCT_NAME.
	 * @param node				tree root
	 * @param entityTypes		gets filled with <code>IdlTypeDef</code> nodes.
	 */
	public void findXmlTypedefNodes(IdlObject node, Set entityTypes) 
	{
		 
		if (node.kind() == IdlType.e_typedef)
		{
			if ( ((IdlTypeDef)node).type().kind() == IdlType.e_ident )
			{
				IdlIdent idlIdentNode = (IdlIdent) ((IdlTypeDef)node).type();
				if (idlIdentNode.internalObject().kind() == IdlType.e_struct)
				{
					IdlStruct idlStructNode = (IdlStruct) idlIdentNode.internalObject();
					if (idlStructNode.name().equalsIgnoreCase(XML_WRAPPER_STRUCT_NAME))
					{
						entityTypes.add(node);
					}
				}					
			}
		}
		
		// descend
		node.reset();
		while (!node.end())
		{
			IdlObject child = node.current();
			findXmlTypedefNodes(child, entityTypes);
			node.next();
		}
	}
	
	/**
	 * Walks recursively down the tree and sorts out
	 * nodes that are directly or through their children affected by XML entities 
	 * and therefore will need later code generation. 
	 * Such nodes will be renamed using {@link #renameXmlBindingNode}.
	 * 
	 * @param node  the node from which to start 
	 * @param affectedIdentifiers	
	 * [id,<code>IdlIdent</code>] read-only map with identifiers that refer to
	 * something which is involved with xml entity objects.
	 * @param nodesToBeGenerated		
	 * gathers all nodes for which code generation must be run to properly
	 * handle xml entity objects. The selection might depend on the IDL->PL
	 * mapping, so check this (todo) when PLs other than Java are generated from this.
	 * For Java, currently idl interfaces and typedefs are considered.
	 * @return true if the node requires its parent node to be "xml entity aware".
	 */
	public boolean findXmlEntityNodes(IdlObject node, Set affectedIdentifiers, Set nodesToBeGenerated)
	{
		boolean isXmlAffected = false;

		// since ACS 3.0, ignore interfaces that don't inherit from ACS::ACSComponent
		// since ACS 8.0.1, include also ACS::OffShoot interfaces
		if (node.kind() == IdlType.e_interface) {
			IdlInterface idlInterface = (IdlInterface) node;
			if (!IDLComponentTester.isACSComponent(idlInterface) && !IDLComponentTester.isACSOffshoot(idlInterface)) {
				return false;
			}
		}

		// identifier node?
		if (node.kind() == IdlType.e_ident)
		{
			IdlObject internalO = ((IdlIdent)node).internalObject();
			
			// is this a direct xml entity typedef?
			if (affectedIdentifiers.contains(internalO))
			{
				isXmlAffected = true;
			}
			// or maybe an indirect xml entity typedef (sequence of, typedef of...)?
			else if (internalO.kind() == IdlType.e_typedef)
			{
				IdlObject typeNode = ((IdlTypeDef) internalO).type();
				
				if (typeNode.kind() == IdlType.e_sequence)
				{
					typeNode.reset();
					typeNode = typeNode.current(); 
				}
				// recursion
				isXmlAffected = findXmlEntityNodes(typeNode, affectedIdentifiers, nodesToBeGenerated);
			}
			else if (internalO.kind() == IdlType.e_interface && IDLComponentTester.isACSOffshoot((IdlInterface)internalO) ) {
				isXmlAffected = findXmlEntityNodes(internalO, affectedIdentifiers, nodesToBeGenerated);
			}
		}
		else  // not an identifier node
		{
			// recursively descend
			node.reset();
			while (!node.end())
			{
				IdlObject child = node.current();
				// affected through any subnode?
				if (findXmlEntityNodes(child, affectedIdentifiers, nodesToBeGenerated))
				{
					isXmlAffected = true;
				}
				node.next();
			}
		}
				
		if (isXmlAffected)
		{		
			renameXmlBindingNode(node);
			nodesToBeGenerated.add(node);
			// make sure all child nodes will be generated as well if necessary
			if (isPLGenerationRoot(node))
			{
				node.reset();
				while (!node.end())
				{
					IdlObject child = node.current();
					nodesToBeGenerated.add(child); // doesn't matter if child has been there already
					node.next();
				}
			}
		}
		
		return isXmlAffected;		
	}
	
	
	/**
	 * Might need to adjust this for languages other than Java
	 */
	public boolean isPLGenerationRoot(IdlObject node)
	{
		return (node.kind() == IdlType.e_interface ||
			      node.kind() == IdlType.e_typedef );
	}	
			
	/**
	 * Renames a node using the selected {@link NamingConventions}.
	 */
	public void renameXmlBindingNode(IdlObject node)
	{
		if (node.name() == null)
			return;
			
		switch (node.kind())
		{
			case IdlType.e_struct : 
			case IdlType.e_union :
			case IdlType.e_sequence :
			case IdlType.e_ident :
			//case IdlType.e_struct_member :
			case IdlType.e_union_member : 
			case IdlType.e_typedef : 
			case IdlType.e_interface : 
			case IdlType.e_forward_interface : 
			//case IdlType.e_param : 
			case IdlType.e_array : 
			case IdlType.e_value_box : 
			case IdlType.e_value :
				String newName = node.name() + m_namingConventions.getSuffix();

				// Offshoots are referenced not only once in the tree, but several times
				// Therefore, we must know which ones we already translated
				if( !translatedObjects.contains(node) ) {
					if (IdlCompiler.verbose)
						System.out.println("changing name from " + node.name() + " to " + newName);
					node.name(newName);
					translatedObjects.add(node);
				}
		}
	}

	/**
	 * Calls <code>reset()</code> and <code>setId(null)</code> on all nodes below and including <code>node</code>.
	 * 
	 * <code>setId(null)</code> is called to invalidate the id cache inside <code>IdlObject</code>. Otherwise 
	 * <code>getId()</code> produces results that are inconsistent with the names of the parent nodes, if 
	 * a parent node was renamed after getId was called. 
	 * This could be fixed in the implementation of <code>IdlObject</code> if <code>IdlObject#name()</code> would 
	 * not only invalidate the id cache of the node itself, but also of all child nodes.
	 */
	public void resetGraph(IdlObject node)
	{
		// descend
		node.reset();
		while (!node.end())
		{
			IdlObject child = node.current();
			resetGraph(child);
			node.next(); 
		}
		// do the reset
		node.reset();
		node.setId(null);
	}
	
			


	public void recursivePrint(IdlObject node, int depth)
	{
		System.out.print(getIndentation(depth));		
		System.out.print(getTypeName(node.kind()));
		if (node.name() != null)//otherwise getId() throws a nullPointerEx
			System.out.print("{" + node.getId() + "}");//name());
		if (node.kind() == IdlType.e_interface)
		{
			IdlInterface idlInterface = (IdlInterface) node;
			Vector inheritance = idlInterface.getInheritance();
			if (!inheritance.isEmpty())
				System.out.print(" :: " + ((IdlInterface)inheritance.get(0)).name());
		}
		if (node.kind() == IdlType.e_ident)
		{
			IdlObject internalO = ((IdlIdent)node).internalObject();
			if (IdlCompiler.verbose)
			{
				try
				{
//					System.out.print(" (internal/final: " + ((IdlIdent)node).internalObjectName() + "/" + 
//							internalO.final_object().name() + ")");
					
					
					String internalOId = ( internalO.name() != null ? internalO.getId() : "NotAvailable" );
					IdlObject finalO = internalO.final_object();
					String finalOId = ( finalO != null && finalO.name() != null ? finalO.getId() : "NotAvailable" );
					
					System.out.print(" (internal/final: " + internalOId + "/" + finalOId);
				}
				catch (RuntimeException e)
				{
					e.printStackTrace();
				}
			}
		}
		if (IdlCompiler.verbose)
		{
			System.out.println();
		}
		node.reset();
		while (!node.end())
		{
			IdlObject child = node.current();
			recursivePrint(child, depth+1);
			node.next();
		}
		
	}

	private static String[] s_indentations = {"", "  ", "    ", "      ", "        ", "          "};
	private static String getIndentation(int depth) {
		if (depth > 5) 
			depth = 5;
		return  s_indentations[depth];
	}


	/**
	 * For printing the parse tree, needed since OpenORB uses bare int values as enums.
	 */
	public static String getTypeName(int type)
	{
		switch (type)
		{
			case IdlType.e_root : return "root";
			case IdlType.e_module : return "module";
			case IdlType.e_enum : return "enum";
			case IdlType.e_struct : return "struct";
			case IdlType.e_union : return "union";
			case IdlType.e_string : return "string";
			case IdlType.e_wstring : return "wstring";
			case IdlType.e_const : return "const";
			case IdlType.e_simple : return "simple";
			case IdlType.e_sequence : return "sequence";
			case IdlType.e_ident : return "identifier";
			case IdlType.e_struct_member : return "structmember";
			case IdlType.e_union_member : return "unionmember";
			case IdlType.e_typedef : return "typedef";
			case IdlType.e_exception : return "exeption";
			case IdlType.e_interface : return "interface";
			case IdlType.e_operation : return "operation";
			case IdlType.e_attribute : return "attribute";
			case IdlType.e_forward_interface : return "forward_interface";
			case IdlType.e_param : return "parameter";
			case IdlType.e_raises : return "raises";
			case IdlType.e_context : return "context";
			case IdlType.e_enum_member : return "enum_member";
			case IdlType.e_any : return "any";
			case IdlType.e_array : return "array";
			case IdlType.e_native : return "native";
			case IdlType.e_fixed : return "fixed";
			case IdlType.e_value_box : return "value_box";
			case IdlType.e_value : return "value";
			case IdlType.e_state_member : return "state_member";
			case IdlType.e_factory : return "factory";
			case IdlType.e_factory_member : return "factory_member";
			case IdlType.e_value_inheritance : return "value_inheritance";
			case IdlType.e_forward_value : return "forward_value";
			case IdlType.e_include : return "include";
			case IdlType.e_import : return "import";
			default : return "???";
		}	
	}
	

}
