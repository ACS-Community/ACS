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

import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.regex.Pattern;

import org.openorb.compiler.IdlCompiler;
import org.openorb.compiler.generator.IdlToJava;
import org.openorb.compiler.object.IdlComment;
import org.openorb.compiler.object.IdlInterface;
import org.openorb.compiler.object.IdlObject;
import org.openorb.compiler.object.IdlTypeDef;
import org.openorb.compiler.object.IdlValueBox;
import org.openorb.compiler.parser.IdlType;


/**
 * @author hsommer
 */ 
public class JavaGenerator extends IdlToJava 
{
	private Set m_xmlTypedefNodes;
	
	private Map m_struct2JavaMap;
	
	/** used as a filter to exclude idl nodes from generation, e.g. interfaces without xml entity objects */
	private Set m_nodesToBeGenerated;
	
	/**
	 * Constructor for JavaGenerator.
	 */
	public JavaGenerator(Set xmlTypedefNodes, Set nodesToBeGenerated)  
	{
		super();
		m_xmlTypedefNodes = xmlTypedefNodes;
		m_nodesToBeGenerated = nodesToBeGenerated;
		m_struct2JavaMap = new HashMap();
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


		if (current.name() == null || m_nodesToBeGenerated.contains(current))
		{
			if (IdlCompiler.verbose)
			{
				System.out.println("generate " + current.getId());
			}
			super.translate_object_content(obj, writeInto, translateType);
		}
		else
		{
			if (IdlCompiler.verbose)
			{
				System.out.println("reject   " + current.getId() );
			}
		}
	}

	/**
	 * @see IdlToJava#translate_interface(IdlObject, File)
	 * Method body copied with slight modifications from IdlToJava#translate_interface_operations.
	 */
	public void translate_interface(IdlObject obj, File writeInto)
	{
		if (((IdlInterface)obj).abstract_interface())
		{
			// this is mainly because we don't want to test it... 
			// should not be too hard to implement though...
			System.err.println("xml layer generation is not supported for abstract interfaces!");
		}
		
		String iname = obj.name();
		PrintWriter output = newFile(writeInto, iname);

		// -- Translate the internal definitions --
		
		String old_pkg = current_pkg;

		addToPkg(obj, obj.name() + "Package");

		File intoMe = null;
		if (!isEmptyInterface(obj))
			intoMe = createDirectory(obj.name() + "Package", writeInto);
		else
			intoMe = writeInto;

		obj.reset();
		while (obj.end() != true)
		{
			switch (obj.current().kind())
			{
				case IdlType.e_enum :
					translate_enum(obj.current(), intoMe);
					break;
				case IdlType.e_struct :
					translate_struct(obj.current(), intoMe);
					break;
				case IdlType.e_union :
					translate_union(obj.current(), intoMe);
					break;
				case IdlType.e_typedef :
					translate_typedef(obj.current(), intoMe);
					break;
				case IdlType.e_exception :
					translate_exception(obj.current(), intoMe);
					break;
				case IdlType.e_native :
					translate_native(obj.current(), intoMe);
					break;
				case IdlType.e_const :
					// Modification by Jojakim Stahl
					if (!((IdlInterface) obj).abstract_interface())
					{
						translate_constant(obj.current(), null, output);
					}
					break;
			}

			obj.next();
		}
		current_pkg = old_pkg;

		        
		java.util.Vector list = ((IdlInterface)obj).getInheritance();
        
		addDescriptiveHeader(output,obj);
        
		// Interface header
		output.print("public interface " + iname);
		
		if ( list.size() != 0 )
		{
			output.print(" extends ");
			for ( int i=0; i<list.size(); i++ )
			{
				IdlObject node = (IdlObject)list.elementAt(i);
				String fullname = fullname(node);
				// if the base interface is not generated, we must inherit from the base *operations* interface
				if (!m_nodesToBeGenerated.contains(node))
				{
					fullname = fullname + "Operations";
				}
				output.print(fullname);
				if ( i != (list.size()-1) )
					output.print(", ");
			}
			output.println("");
		}
		else
			output.println("");
        
		output.println("{");
        
		obj.reset();
		while ( obj.end() != true )
		{
			switch ( obj.current().kind() )
			{
			case IdlType.e_operation :
				translate_operation(obj.current(),output);
				break;
			case IdlType.e_attribute :
				translate_attribute(obj.current(),output);
				break;
			}
            
			obj.next();
		}
        
		output.println("}");
		output.close();




//		java.io.PrintWriter output;
//
//		if (!((IdlInterface) obj).abstract_interface())
//			output = newFile(writeInto, obj.name() + "Operations");
//		else
//			output = newFile(writeInto, obj.name());
//
//		String old_pkg;
//		java.util.Vector list = ((IdlInterface) obj).getInheritance();
//
//		cartouche(output, obj);
//
//		// Interface header
//		if (!((IdlInterface) obj).abstract_interface())
//		{
//			output.print("public interface " + obj.name() + "Operations");
//			if (list.size() != 0)
//				output.print(" extends ");
//		}
//		else
//		{
//			output.print(
//				"public interface "
//					+ obj.name()
//					+ " extends org.omg.CORBA.portable.IDLEntity");
//			if (list.size() != 0)
//				output.print(", ");
//		}
//		if (list.size() != 0)
//		{
//			for (int i = 0; i < list.size(); i++)
//			{
//				if (!((IdlInterface) list.elementAt(i)).abstract_interface())
//					output.print(
//						fullname(((IdlObject) list.elementAt(i))) + "Operations");
//				else
//					output.print(fullname(((IdlObject) list.elementAt(i))));
//				if (i != (list.size() - 1))
//					output.print(", ");
//			}
//			output.println("");
//		}
//		else
//			output.println("");
//
//		output.println("{");
//
//		obj.reset();
//		while (obj.end() != true)
//		{
//			switch (obj.current().kind())
//			{
//				case IdlType.e_operation :
//					translate_operation(obj.current(), output);
//					break;
//				case IdlType.e_attribute :
//					translate_attribute(obj.current(), output);
//					break;
//				case IdlType.e_const :
//					// Modification by Jojakim Stahl
//					if (((IdlInterface) obj).abstract_interface())
//					{
//						translate_constant(obj.current(), null, output);
//					}
//					break;
//			}
//
//			obj.next();
//		}
//
//		output.println("}");
//		output.close();
	}

	/**
	 * Suppresses generation of helper classes and acts as a dirty shortcut to generate holder
	 * classes for typedefs.
	 * 
	 * According to the OMG IDL2Java mapping, <i>Typedefs for types that are neither arrays nor sequences 
	 * are unwound to their original type until a simple IDL type or user-defined IDL type 
	 * (of the non typedef variety) is encountered. Holder classes are generated for 
	 * sequence and array typedefs only. The unwound type's Holder class is used for the other cases.</i>
	 * <p>
	 * A <code>SchedBlock</code> typedef would not get its own SchedBlockHolder according to the mapping rules,
	 * but <code>EntityStructHolder</code> would be used instead.
	 * For the XML-binding interface, we do need a typesafe holder for the Java binding class.
	 * Therefore, an additional holder class must be constructed, outside of the course of the 
	 * OpenORB compiler, which only attempts to generate the standard mappings.
	 * This class generation is conveniently triggered from the <code>write_helper</code> method
	 * because it already has the right package info. Sorry for the confusion... 
	 * 
	 * @see IdlToJava#write_helper(IdlObject, File)
	 */
	public void write_helper(IdlObject obj, File writeInto)
	{		
		if (obj.kind() == IdlType.e_typedef && m_xmlTypedefNodes.contains(obj))
		{
//			IdlObject internalO = ((IdlIdent)obj.current()).internalObject();
			
			write_holder(obj, writeInto);
		}

		if (IdlCompiler.verbose)
		{
			System.out.println("suppressing helper class generation for " + obj.getId());
		}
	}


	/**
	 * @see IdlToJava#write_holder(IdlObject, File)
	 */
	public void write_holder(IdlObject obj, File writeInto)
	{
		java.io.PrintWriter output = newFile(writeInto, obj.name() + "Holder");

		if (current_pkg != null)
		{
			if (current_pkg.equals("generated"))
			{
				if (org.openorb.compiler.IdlCompiler.use_package == true)
				{
					output.println("package " + current_pkg + ";");
					output.println("");
				}
			}
			else if (!current_pkg.equals(""))
			{
				output.println("package " + current_pkg + ";");
				output.println("");
			}
		}

		output.println("//");
		output.println("// Holder class for : " + obj.name());
		output.println("//");
		output.println("// @author ACS xml transparency layer generator");
		output.println("//");

		output.println("final public class " + obj.name() + "Holder");
		output.println("{");

		// The internal value
		output.println("\t//");
		output.println("\t// Internal " + obj.name() + " value");
		output.println("\t//");

		output.print("\tpublic ");

		if (obj.kind() == IdlType.e_value_box)
		{
			if (((IdlValueBox) obj).simple())
			{
				obj.reset();
				translate_type(obj.current(), output);
			}
			else
				translate_type(obj, output);
		}
		else
			translate_type(obj, output);

		output.println(" value;");


		output.println("}");
		output.close();
	}

	
	public static NamingConventions getNamingConventions()
	{
		return new NamingConventions()
		{
			/**
			 * @see NamingConventions#getSuffix()
			 */
			public String getSuffix()
			{
				return "J";
			}
		};
	}

	/**
	 * Overloaded to use our StrippingPrintWriter instead of a normal PrintWriter
	 * @see IdlToJava#fileAccess(File)
	 */
	public PrintWriter fileAccess(File writeInto)
	{
		PrintWriter printout = null;

		try
		{
			FileWriter fileWriter = new FileWriter(writeInto);
			printout = new StrippingPrintWriter(fileWriter, true);
		}
		catch (java.io.IOException e)
		{
			e.printStackTrace();
		}

		return printout;
	}

	/**
	 * @see IdlToJava#translate_type(IdlObject, PrintWriter)
	 */
	public void translate_type(IdlObject obj, PrintWriter output)
	{
		//System.out.println("translate_type " + IdlTreeManipulator.getTypeName(obj.kind()) + "::" + obj.name());
		if (obj.kind() == IdlType.e_typedef && m_xmlTypedefNodes.contains(obj))
		{
			String returnType = getXmlBindingClass( ((IdlTypeDef)obj).name());
			output.print(returnType);
		} 
		else
		{
			super.translate_type(obj, output);
		}						
	}

	/**
	 * @see IdlToJava#translate_parameter(IdlObject, PrintWriter, int)
	 */
	public void translate_parameter(IdlObject obj, PrintWriter output, int attr)
	{
		if (obj.kind() == IdlType.e_typedef && m_xmlTypedefNodes.contains(obj) )
		{
			if (attr == 0)
			{
				// in-parameter
				String paramType = getXmlBindingClass( ((IdlTypeDef)obj).name());
				output.print(paramType);
			}
			else
			{		
				// out/inout
				output.print(fullname(obj) + "Holder");
			}
		} 
		else
		{
			super.translate_parameter(obj, output, attr);
		}
	}


	/**
	 * Sets the mappings from <code>XmlEntityStruct</code> typedefs to 
	 * Java binding classes.
	 * For example, <code>ObsProposal=alma.xmljbind.test.obsproposal.ObsProposal</code>
	 * would substitute the Java class on the right side for the IDL type ObsProposal, 
	 * which is defined as <code>typedef xmlentity::XmlEntityStruct ObsProposal;</code> 
	 * in the IDL file. 
	 * @param mappings  concatenated mappings, separated by ';'
	 */
	void setIdlStruct2JavaBindingClassMappings(String mappings)
	{
		if (mappings == null) return;
		
		StringTokenizer tok = new StringTokenizer(mappings, ";");
		while (tok.hasMoreTokens())
		{
			String mapping = tok.nextToken().trim();
			
			int sepPos = mapping.indexOf('=');
			if (sepPos > 0)
			{
				String structName = mapping.substring(0, sepPos).trim();
				String bindingClassName = mapping.substring(sepPos + 1).trim();
				
				m_struct2JavaMap.put(structName, bindingClassName);
			}
		}
	}
	
	/**
	 * Translates an IDL xml wrapper struct to the qualified name of the Java binding class 
	 * that should appear in the "inner" interface.
	 */
	String getXmlBindingClass(String structTypeName) 
	{		
		// get it from the map
		String className = (String) m_struct2JavaMap.get(structTypeName);
		
		if (className == null)
		{
			String msg = "unknown struct type " + structTypeName + 
							" could not be mapped to a binding class.";
			System.err.println(msg);
			// can't propagate user exception in OpenORB method signatures, thus Runtime ex 
			throw new RuntimeException(msg);
		}
		
		return className;
	}
	

//	/**
//   * uncomment only to trace this...
//	 * @see org.openorb.compiler.generator.IdlToJava#fullname(IdlObject)
//	 */
//	public String fullname(IdlObject obj)
//	{
//		String fn = super.fullname(obj);
//		if (IdlCompiler.verbose)
//		{
//			System.out.println("super-fullname=" + fn);
//		}
//		return fn;
//	}
//

	/**
	 * @see org.openorb.compiler.generator.IdlToJava#addDescriptiveHeader(java.io.PrintWriter, org.openorb.compiler.object.IdlObject)
	 */
	public void addDescriptiveHeader(PrintWriter output, IdlObject obj)
	{
		if (obj.kind() == IdlType.e_interface)
		{
			IdlComment comment = new IdlComment();
			comment.add_description("XML binding class aware ACS component interface " + 
									obj.name());// + System.getProperty("line.separator"));
			obj.attach_comment(comment);
		}
		
		super.addDescriptiveHeader(output, obj);
	}

//	/**
//	 * @see org.openorb.compiler.generator.IdlToJava#translate_typedef(org.openorb.compiler.object.IdlObject, java.io.File)
//	 */
//	public void translate_typedef(IdlObject obj, File writeInto)
//	{
//		if (obj.current().kind() == IdlType.e_ident)
//		{
//			IdlObject internalO = ((IdlIdent)obj.current()).internalObject();
//			
//			System.out.println("oops, check for xml binding class and make a holder if so... -- " + 
//								obj.getId() + "  internal:" + internalO.getId());
//		}
//
//		super.translate_typedef(obj, writeInto);
//	}


	/* (non-Javadoc)
	 * @see org.openorb.compiler.generator.IdlToJava#translate_exception(org.openorb.compiler.object.IdlObject, java.io.File)
	 */
	public void translate_exception(IdlObject obj, File writeInto)
	{
		// leave it to the regular IDL compiler...
	}

	/* (non-Javadoc)
	 * @see org.openorb.compiler.generator.IdlToJava#createDirectory(java.lang.String, java.io.File)
	 */
	public File createDirectory(String name, File writeInto)
	{
		// to make sure that xxxPackage directories will not use the appended "J"
		// like the manipulated interface name
		name = OutputStringManipulator.modify(name);
		
		return super.createDirectory(name, writeInto);
	}

}




/**
 * This class is used as a somewhat dirty, but very efficient way to avoid overloading 
 * and copying 95% of the implementation of a number of methods like 
 * IdlToJava#translate_struct(IdlObject obj, File writeInto).
 * 
 * In these methods, only very few known Strings must be suppressed or modified; 
 * the strings are hopefully unique enough to not provoke unforeseen damage with the modification.
 * 
 * @see OutputStringManipulator
 */
class StrippingPrintWriter extends PrintWriter
{
	public StrippingPrintWriter(Writer out, boolean autoFlush) 
	{
		super(out, autoFlush);
	}
				
	/**
	 * @see Writer#write(String)
	 */
	public void write(String s) 
	{
		s = OutputStringManipulator.modify(s);
		super.write(s);
	}
}


/**
 * Modifies strings for our lean-and-mean approach to reusing OpenORB code.
 * <ul>
 * <li>according to the OMG spec, declarations of structs, exceptions, etc. inside 
 * 		an interface will be mapped to a Java sub-package interfaceName+'Package'.
 * 		For the special purposes of the ACS IDL compiler, the interface name 
 * 		gets appended with 'J'; 
 * 		this would lead to undesireable sub-packages interfaceName+'J'+'Package'.
 * 		Therefore, we replace occurences of 'JPackage' with 'Package'</li> 
 * <li><code>implements org.omg.CORBA.portable.IDLEntity</code> is stripped off
 * 		so that generated Java classes for interfaces, structs etc. don't 
 * 		implement that interface.</li>
 * </ul> 
 * @author hsommer
 */
class OutputStringManipulator
{
	private static Map s_patternReplacementMap;
	
	static 
	{
		s_patternReplacementMap = new LinkedHashMap();
		
		// set up pre-compiled patterns 
		
		String reg = JavaGenerator.getNamingConventions().getSuffix() + "Package";
		Pattern pat = Pattern.compile(reg);
		s_patternReplacementMap.put(pat, "Package");
		
		reg = "implements org.omg.CORBA.portable.IDLEntity";
		pat = Pattern.compile(reg);
		s_patternReplacementMap.put(pat, "");
	}
		
	static String modify(String input)
	{
		String output = input;
		
		for (Iterator iter = s_patternReplacementMap.keySet().iterator(); iter.hasNext();)
		{
			Pattern pat = (Pattern) iter.next();
			String repl = (String) s_patternReplacementMap.get(pat);
			output = pat.matcher(output).replaceAll(repl);
		}

		return output;
	}
}
