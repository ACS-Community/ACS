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

import java.util.HashMap;
import java.util.Iterator;
import java.util.Vector;

import org.openorb.compiler.object.IdlInterface;
import org.openorb.compiler.object.IdlObject;
import org.openorb.compiler.parser.IdlType;

/**
 * @author hsommer
 * created Nov 10, 2003 3:34:51 PM
 */
public class IDLComponentTester
{

	public static final String ACSCOMPONENT_IDLTYPE = "IDL:alma/ACS/ACSComponent:1.0";
	public static final String ACSOFFSHOOT_IDLTYPE = "IDL:alma/ACS/OffShoot:1.0";
	
	private static HashMap<String, IdlInterface> s_allInterfaces;
	
	
	/**
	 * Collecting all interfaces prior to calling {@link #isACSComponent} is part
	 * of a workaround for an OpenORB bug or feature.
	 * It is not possible to directly navigate up the graph of base interfaces using
	 * {@link IdlInterface#getInheritance()}.
	 * @param root
	 */
	public static void collectInterfaces(IdlObject root)
	{
		s_allInterfaces = new HashMap<String, IdlInterface>(); 
		collectInterfaces(root, s_allInterfaces);
	}
	
	/**
	 * Traverses the tree under <code>node</code> and puts all interface nodes into the map.
	 * @param node  IDL parse tree node
	 * @param interfaceMap  will get interfaces (key=IdlInterface#name, value=IdlInterface)
	 */	
	private static void collectInterfaces(IdlObject node, HashMap<String, IdlInterface> interfaceMap)
	{
		if (node.kind() == IdlType.e_interface)
		{
			// keep the name separate because it is "volatile"
			interfaceMap.put(node.name(), (IdlInterface)node);
		}
		else
		{
			node.reset();
			while (!node.end())
			{
				IdlObject child = node.current();
				collectInterfaces(child, interfaceMap);
				node.next();
			}
			// important to reset -- otherwise we'll get an ArrayIndexOutOfBounds ex later 
			// while traversing the tree in JavaPackageScout# 
			node.reset();
		}
	}
	
	public static boolean isACSComponent(IdlInterface interfaceNode) {
		return isACSInterface(interfaceNode, ACSCOMPONENT_IDLTYPE);
	}
	
	public static boolean isACSOffshoot(IdlInterface interfaceNode) {
		return isACSInterface(interfaceNode, ACSOFFSHOOT_IDLTYPE);
	}

	/**
	 * Calls {@link IdlInterface#getInheritance()} recursively on <code>interfaceNode</code>
	 * and returns true if <code>IDL:alma/ACS/ACSComponent:1.0</code> 
	 * is among the inherited interfaces.
	 * <p>
	 * The method {@link #collectInterfaces(IdlObject)} must be called before this method.  
	 */
	protected static boolean isACSInterface(IdlInterface interfaceNode, String acsInterfaceId) 
	{
		if (s_allInterfaces == null)
		{
			throw new NullPointerException("must first call #collectInterfaces before calling #isACSComponent");
		}
		
		boolean isACSInterface = false;
		
		if (interfaceNode.getId().equals(acsInterfaceId))
		{
			isACSInterface = true;
		}
		else
		{
			// getInheritance() yields only parent interfaces (not grandparents etc), so needs traversal
			Vector inheritance = interfaceNode.getInheritance();
			
			for (Iterator iter = inheritance.iterator(); iter.hasNext();)
			{
				IdlInterface inheritedIF = (IdlInterface) iter.next();

				// because of some bug or ugly feature of OpenORB, we can't just ask the 
				// base interface for its base interfaces (would claim to not have any...)
				// As a workaround, we take the IdlInterface object from the map.
				IdlInterface realInheritedIF = s_allInterfaces.get(inheritedIF.name());
				
				if (realInheritedIF == null)
				{
					throw new NullPointerException("failed to retrieve IdlInterface object for interface " + inheritedIF.name());
				}
				
				// recursion
				if (isACSInterface(realInheritedIF, acsInterfaceId))
				{
					isACSInterface = true;
					break;
				}
			}
		}
		return isACSInterface;
	}
}
