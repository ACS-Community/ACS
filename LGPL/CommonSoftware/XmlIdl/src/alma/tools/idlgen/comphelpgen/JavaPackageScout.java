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
package alma.tools.idlgen.comphelpgen;

import java.io.File;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;
//import java.util.Set;

import org.openorb.compiler.generator.IdlToJava;
import org.openorb.compiler.object.IdlInterface;
import org.openorb.compiler.object.IdlObject;
import org.openorb.compiler.parser.IdlType;


/**
 * Computes Java package information for IDL interfaces.
 * 
 * To save some programming effort, the packages are computed during a full run of 
 * {@link org.openorb.compiler.generator.IdlToJava}. 
 * Therefore, a full IDL-to-Java translation happens in memory, with the only used
 * result being the Java packages. The rest gets lost in memory. 
 * This implementation is certainly not the best choice for performance,
 * but as long as that's good enough, the gain in robustness should justify it.
 *  
 * @author hsommer
 */ 
public class JavaPackageScout extends IdlToJava 
{
//	private Set m_xmlTypedefNodes;
	
	// key = interface node , value = java package
	private Map m_ifPkgMap;
		

	JavaPackageScout()  
	{
		super();
		m_ifPkgMap = new HashMap();
	}

	void collectInterfacePackages(IdlObject root, String rootPackage)
	{
		translateData(root, rootPackage);
	}

	String getPackage(IdlInterface idlInterface)
	{
		String pckg = (String) m_ifPkgMap.get(idlInterface);
		return pckg;
	}


	/**
	 * @see IdlToJava#translate_object_content(IdlObject, File, int)
	 */
	public void translate_object_content(
		IdlObject obj,
		File writeInto,
		int translateType)
	{
		// since super.translate_object_content only cares about obj.current() and not obj itself
		IdlObject current = obj.current();

		// store the Java package that corresponds to the (component-) interface 
		if (current.kind() == IdlType.e_interface)
		{
			m_ifPkgMap.put(current, current_pkg);
		}
		
		super.translate_object_content(obj, writeInto, translateType);
	}

	////////////////////////////////////////////////
	// suppression of unnecessary tree traversal
	////////////////////////////////////////////////

	
	/**
	 * Cutting off the tree traversal here is not only a performance optimization, 
	 * but also a workaround for a bug (ArrayIndexOutOfBoundsEx)
	 * that otherwise would occur when translating sequence typedefs.
	 * TODO: better fix that bug
	 * 
	 * @see org.openorb.compiler.generator.IdlToJava#translate_operation(org.openorb.compiler.object.IdlObject, java.io.PrintWriter)
	 */
	public void translate_operation(
		IdlObject obj,
		PrintWriter output)
	{
		// ignore
	}


	/**
	 * @see org.openorb.compiler.generator.IdlToJava#write_helper(org.openorb.compiler.object.IdlObject, java.io.File)
	 */
	public void write_helper(IdlObject obj, File writeInto)
	{
		// ignore
	}

	/**
	 * @see org.openorb.compiler.generator.IdlToJava#write_holder(org.openorb.compiler.object.IdlObject, java.io.File)
	 */
	public void write_holder(IdlObject obj, File writeInto)
	{
		// ignore
	}


	////////////////////////////////////
	// suppression of file access
	////////////////////////////////////

	/**
	 * Prevents file output (all will be lost in memory...).
	 * 
	 * @see IdlToJava#fileAccess(File)
	 */
	public PrintWriter fileAccess(File writeInto)
	{
		StringWriter sw = new StringWriter();
		PrintWriter pw = new PrintWriter(sw);
		return pw;
	}

	/**
	 * @see org.openorb.compiler.generator.IdlToJava#createDirectory(java.lang.String, java.io.File)
	 */
	public File createDirectory(String name, File writeInto)
	{
		return writeInto;
	}

	/**
	 * @see org.openorb.compiler.generator.IdlToJava#createPrefixDirectories(java.lang.String, java.io.File)
	 */
	public File createPrefixDirectories(
		String prefix,
		File writeInto)
	{
		return createDirectory(prefix, writeInto);
	}

	/**
	 * @see org.openorb.compiler.generator.IdlToJava#getDirectory(java.lang.String, java.io.File)
	 */
	public File getDirectory(String name, File writeInto)
	{
		return writeInto;
	}

}


