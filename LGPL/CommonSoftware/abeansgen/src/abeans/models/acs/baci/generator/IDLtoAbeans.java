/*
 * @@COPYRIGHT@@
 */

/*
 * Copyright (C) The Community OpenORB Project. All rights reserved.
 *
 * This software is published under the terms of The OpenORB Community Software
 * License version 1.0, a copy of which has been included with this distribution
 * in the LICENSE.txt file.
 */

package abeans.models.acs.baci.generator;

import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.openorb.compiler.IdlCompiler;
import org.openorb.compiler.object.IdlArray;
import org.openorb.compiler.object.IdlAttribute;
import org.openorb.compiler.object.IdlComment;
import org.openorb.compiler.object.IdlCommentField;
import org.openorb.compiler.object.IdlCommentSection;
import org.openorb.compiler.object.IdlConst;
import org.openorb.compiler.object.IdlContext;
import org.openorb.compiler.object.IdlEnumMember;
import org.openorb.compiler.object.IdlFactoryMember;
import org.openorb.compiler.object.IdlIdent;
import org.openorb.compiler.object.IdlInterface;
import org.openorb.compiler.object.IdlObject;
import org.openorb.compiler.object.IdlOp;
import org.openorb.compiler.object.IdlParam;
import org.openorb.compiler.object.IdlRaises;
import org.openorb.compiler.object.IdlSimple;
import org.openorb.compiler.object.IdlStateMember;
import org.openorb.compiler.object.IdlStructMember;
import org.openorb.compiler.object.IdlTypeDef;
import org.openorb.compiler.object.IdlUnion;
import org.openorb.compiler.object.IdlUnionMember;
import org.openorb.compiler.object.IdlValue;
import org.openorb.compiler.object.IdlValueBox;
import org.openorb.compiler.object.IdlValueInheritance;
import org.openorb.compiler.parser.IdlType;
import org.openorb.compiler.parser.Token;
import org.openorb.util.CharacterCache;

/**
 * This class generates all mapping for IDL descriptions.
 * Restrictions:
 * 	o) Only single inheritance is supported.
 *  o) Nested arrays/sequences are only supported for primitives
 * 		
 *
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public class IDLtoAbeans
{
	/**
	 * Javadoc author tag.
	 */
	private static final String JAVADOC_AUTHOR = IDLtoAbeansCompiler.PRODUCT_NAME + " v" + IDLtoAbeansCompiler.VERSION;
	
	// PACKAGE HANDLING
	private static final String ABEANS_POSTFIX_DOTLESS = "abeans";
	private static final String ABEANS_POSTFIX = "." + ABEANS_POSTFIX_DOTLESS;
	// added to ABEANS_POSTFIX
	private static final String PROXY_POSTFIX_DOTLESS = "proxy";	
	private static final String PROXY_POSTFIX = "." + PROXY_POSTFIX_DOTLESS;	

	/**
	 * Component inteface ID.
	 */
	private static final String BASE_CONNECTABLE_ID = "IDL:alma/ACS/CharacteristicComponent:1.0";
	private static final String BASE_LINKABLE_ID = "IDL:alma/ACS/TypelessProperty:1.0";

	private static final String BASE_PROPERTY_ID = "IDL:alma/ACS/TypelessProperty:1.0";

	private static final String BASE_CALLBACK_ID = "IDL:alma/ACS/Callback:1.0";

	/**
	 * GENERATED string.
	 */
	private static final String GENERATED = "generated";

	/**
	 * Empty string.
	 */
	private static final String EMPTY_STRING = "";

	// Indents...
    private static final String tab = "\t";
    private static final String tab1 = tab;
    private static final String tab2 = tab + tab;
    private static final String tab3 = tab + tab + tab;
    private static final String tab4 = tab + tab + tab + tab;
    private static final String tab5 = tab + tab + tab + tab + tab;

    /**
     * Current package
     */
    private String current_pkg = adaptToDot(IdlCompiler.packageName);

    /**
     * Reference to the compilation graph
     */
    private IdlObject _root = null;

    /**
     * Reference to the initial directory
     */
    private File initial = null;

    /**
     * Default constructor
     */
    public IDLtoAbeans()
    {
    	// PACKAGE HANDLING
    	if (current_pkg.length() == 0)
    		current_pkg = ABEANS_POSTFIX_DOTLESS;
    	else
    		current_pkg += ABEANS_POSTFIX;
    }

    /**
     * Retur true if a definition exists for a native type
     *
     * @param obj native object
     * @return true if a definition exists
     */
    private static boolean isNativeDefinition(IdlObject obj)
    {
        for (int i = 0; i < IdlCompiler.nativeDefinition.size(); i++)
        {
            String s = (String) IdlCompiler.nativeDefinition.get(i);

            int index = s.indexOf(':');

            String word = s.substring(0, index);

            if (obj.name().equals(word))
                return true;
        }

        return false;
    }

    /**
     * Print the type translated corresponding to a native type definition
     *
     * @param obj  native object
     * @param output file where definition is added
     * @return true if a definition exists
     */
    private static void printNativeDefinition(IdlObject obj, PrintWriter output)
    {
        for (int i = 0; i < IdlCompiler.nativeDefinition.size(); i++)
        {
            String s = (String) IdlCompiler.nativeDefinition.get(i);

            int index = s.indexOf(':');

            String word = s.substring(0, index);

            if (obj.name().equals(word))
            {
                index = s.lastIndexOf(':');

                word = s.substring(index + 1, s.length());

                output.print(word);
            }
        }
    }

    /**
     * Allows to get an access on write to a target file
     *
     * @param writeInto Target file descriptor
     * @return write access
     */
    private static PrintWriter fileAccess(File writeInto)
    {
        // Deprecated
        // PrintStream printout = null;
        PrintWriter printout = null;

        try
        {
            FileOutputStream output = new FileOutputStream(writeInto);
            DataOutputStream dataout = new DataOutputStream(output);
            printout = new PrintWriter(dataout, true);
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }

        return printout;
    }

    /**
     * Change a prefix order : omg.org -> org.omg
     *
     * @param prefix  the prefix to inverse
     * @return the inversed prefix
     */
    private static String inversedPrefix (String prefix)
    {
        int index = 0;
        int previous_index = 0;
        List seq = new ArrayList();

        try
        {
            while (index != -1)
            {
                index = prefix.indexOf('.', previous_index);

                if (index != -1)
                {
                    seq.add(new String(prefix.substring(previous_index, index)));
                    previous_index = index + 1;
                }
            }
        }
        catch (StringIndexOutOfBoundsException ex)
        { }

        seq.add(new String(prefix.substring(previous_index, prefix.length())));

		StringBuffer inversed = new StringBuffer();

        for (int i = seq.size() - 1; i >= 0; i--)
        {
            if (inversed.length()!=0)
                inversed.append('.');

            inversed.append((String)seq.get(i));
        }

        return new String(inversed);
    }

    /**
     * Creates a new Java file
     *
     * @param writeInto the directory where the file must be created
     * @param name  file name (without .java extension)
     * @return write access
     */
    private static PrintWriter newFile(File writeInto, String name)
    {
        String path;

        if (!writeInto.exists())
            writeInto.mkdirs();

        path = new String(writeInto.getPath() + File.separator + name + ".java");

        File file = new File(path);

        return fileAccess(file);
    }

    /**
     * Creates a directory
     *
     * @param name the directory name to create
     * @param writeInto the directory in which the search has to appear
     */
    private static File createDirectory(String name, File writeInto)
    {
        String path;
        String fname;
        boolean init = false;

        char [] tab = new char [ name.length() + 20 ];

        int j = 0;

        for (int i = 0; i < name.length(); i++)
        {
            if (name.charAt(i) == '.')
            {
                tab[ j++ ] = File.separatorChar;
                init = true;
            }
            else
                tab[ j++ ] = name.charAt(i);
        }

        fname = new String(tab, 0, j);

        if (writeInto != null)
        {
            path = new String(writeInto.getPath() + File.separator + fname);
        }
        else
            path = fname;

        File file = new File(path);

        if (file.exists() == false)
            file.mkdirs();

        if (init == true)
            IdlCompiler.packageName = fname;

        return file;
    }

    /**
     * Creates the directories corresponding to a CORBA ID prefix
     *
     * @param prefix  the prefix
     * @param writeInto the directory in which the search has to appear
     */
    private static File createPrefixDirectories(String prefix, File writeInto)
    {
        String path;

        char [] tab = new char [ prefix.length() + 20 ];

        String name = null;

        if (IdlCompiler.reversePrefix)
            name = new String(inversedPrefix(prefix));
        else
            name = prefix;

        int j = 0;

        for (int i = 0; i < name.length(); i++)
        {
            if (name.charAt(i) == '.')
                tab[ j++ ] = File.separatorChar;
            else
                tab[ j++ ] = name.charAt(i);
        }

        name = new String(tab, 0, j);

        if (writeInto != null)
        {
            path = new String(writeInto.getPath() + File.separator + name);
        }
        else
            path = name;

        File file = new File(path);

        if (file.exists() == false)
            file.mkdirs();

        return file;
    }

    /**
     * Get a write access in a directory
     *
     * @param name the directory name
     * @param writeInto the directory in which the search has to appear
     */
    private static File getDirectory(String name, File writeInto)
    {
        String path;

        if (writeInto != null)
        {
            path = new String(writeInto.getPath() + File.separator + name);
        }
        else
            path = name;

        File file = new File(path);

        return file;
    }

    /**
     * Get a writing access to a directory corresponding to a CORBA ID prefix
     *
     * @param prefix a object prefix
     * @param writeInto the directory in which the search has to appear
     */
    private static File getPrefixDirectories(String prefix, File writeInto)
    {
        String path;

        char [] tab = new char [ prefix.length() + 20 ];

        String name = null;

        if (IdlCompiler.reversePrefix)
            name = new String(inversedPrefix(prefix));
        else
            name = prefix;

        int j = 0;

        for (int i = 0; i < name.length(); i++)
        {
            if (name.charAt(i) == '.')
                tab[ j++ ] = File.separatorChar;
            else
                tab[ j++ ] = name.charAt(i);
        }

        name = new String(tab, 0, j);

        if (writeInto != null)
        {
            path = new String(writeInto.getPath() + File.separator + name);
        }
        else
            path = name;

        File file = new File(path);

        return file;
    }

    /**
     * Construct a package name
     *
     * @param name name of the superior level
     */
    /*
    private void addToPkg(IdlObject obj, String name)
    {
        if (IdlCompiler.use_package == false)
        {
            if (!current_pkg.equals(GENERATED))
            {
                if (!current_pkg.equals(EMPTY_STRING))
                    current_pkg = current_pkg + "." + name;
                else
                    current_pkg = name;
            }
            else
                current_pkg = name;
        }
        else
        {
            if (!current_pkg.equals(EMPTY_STRING))
                current_pkg = current_pkg + "." + name;
            else
                current_pkg = name;
        }
    }
	*/
	// PACKAGE HANDLING
	private String removeAbeansPostfix(String packageName)
	{
		if (packageName.endsWith(ABEANS_POSTFIX))
			return packageName.substring(0, packageName.length()-ABEANS_POSTFIX.length());
		else if (packageName.equals(ABEANS_POSTFIX_DOTLESS))
			return EMPTY_STRING;
		else
			return packageName;
	}

	// PACKAGE HANDLING
	private void addToPkg(IdlObject obj, String name)
	{
		if (IdlCompiler.use_package == false)
		{
			if (!current_pkg.equals(GENERATED))
			{
				if (!current_pkg.equals(ABEANS_POSTFIX_DOTLESS))
					current_pkg = removeAbeansPostfix(current_pkg) + "." + name + ABEANS_POSTFIX;
				else
					current_pkg = name + ABEANS_POSTFIX;
			}
			else
				current_pkg = name + ABEANS_POSTFIX;
		}
		else
		{
			if (!current_pkg.equals(ABEANS_POSTFIX_DOTLESS))
				current_pkg = removeAbeansPostfix(current_pkg) + "." + name + ABEANS_POSTFIX;
			else
				current_pkg = name + ABEANS_POSTFIX;
		}
	}

    /**
	 * @param output
	 */
	public void addPackageName(final PrintWriter output)
    {
        if (current_pkg != null)
        {
            if (current_pkg.equals(GENERATED))
            {
                if (IdlCompiler.use_package)
                {
                    output.println("package " + current_pkg + ";");
                    output.println();
                }
            }
            else
            {
                if (!current_pkg.equals(EMPTY_STRING))
                {
                    output.println("package " + current_pkg + ";");
                    output.println();
                }
            }
        }
    }

    /**
     * Add a descriptive header in a Java file
     *
     * @param output  the target file
     * @param obj object which descriptive header has to be added
     */
    private void addDescriptiveHeader (PrintWriter output, IdlObject obj)
    {
        addPackageName(output);

        switch (obj.kind())
        {

        case IdlType.e_const :

            if (obj.hasComment())
                javadoc(output, obj);
            else
            {
                output.println("/**");
                output.println(" * Constant definition : " + obj.name());
                output.println(" *");
                output.println(" * @author " + JAVADOC_AUTHOR);
                output.println(" */");
            }

            break;

        case IdlType.e_enum :

            if (obj.hasComment())
                javadoc(output, obj);
            else
            {
                output.println("/**");
                output.println(" * Enum definition : " + obj.name());
                output.println(" *");
                output.println(" * @author " + JAVADOC_AUTHOR);
                output.println(" */");
            }

            break;

        case IdlType.e_struct :

            if (obj.hasComment())
                javadoc(output, obj);
            else
            {
                output.println("/**");
                output.println(" * Struct definition : " + obj.name());
                output.println(" * ");
                output.println(" * @author " + JAVADOC_AUTHOR);
                output.println(" */");
            }

            break;

        case IdlType.e_union :

            if (obj.hasComment())
                javadoc(output, obj);
            else
            {
                output.println("/**");
                output.println(" * Union definition : " + obj.name());
                output.println(" * ");
                output.println(" * @author " + JAVADOC_AUTHOR);
                output.println(" */");
            }

            break;

        case IdlType.e_exception :

            if (obj.hasComment())
                javadoc(output, obj);
            else
            {
                output.println("/**");
                output.println(" * Exception definition : " + obj.name());
                output.println(" * ");
                output.println(" * @author " + JAVADOC_AUTHOR);
                output.println(" */");
            }

            break;

        case IdlType.e_interface :

            if (obj.hasComment())
                javadoc(output, obj);
            else
            {
                output.println("/**");
                output.println(" * Interface definition : " + obj.name());
                output.println(" * ");
                output.println(" * @author " + JAVADOC_AUTHOR);
                output.println(" */");
            }

            break;

        case IdlType.e_value_box :

            if (obj.hasComment())
                javadoc(output, obj);
            else
            {
                output.println("/**");
                output.println(" * Value box definition : " + obj.name());
                output.println(" * ");
                output.println(" * @author " + JAVADOC_AUTHOR);
                output.println(" */");
            }

            break;

        case IdlType.e_value :

            if (obj.hasComment())
                javadoc(output, obj);
            else
            {
                output.println("/**");
                output.println(" * Value Type definition : " + obj.name());
                output.println(" * ");
                output.println(" * @author " + JAVADOC_AUTHOR);
                output.println(" */");
            }

            break;

        case IdlType.e_factory :

            if (obj.hasComment())
                javadoc(output, obj);
            else
            {
                output.println("/**");
                output.println(" * Factory definition : " + obj.name());
                output.println(" * ");
                output.println(" * @author " + JAVADOC_AUTHOR);
                output.println(" */");
            }

            break;
        }

    }

    /**
     * Translate a JavaDoc comments section
     */
    private static void translate_comment_section(PrintWriter output, String description, IdlObject obj)
    {
        int i = 0;

        while (i < description.length())
        {
            if (description.charAt(i) == '\n')
            {
                if (i != description.length() - 1)
                {
                    output.println();

                    if ((obj.kind() == IdlType.e_attribute) || (obj.kind() == IdlType.e_operation) || (obj.kind() == IdlType.e_state_member))
                        output.print(tab + EMPTY_STRING);

                    output.print(" * ");
                }
                else
                {
                    output.println();
                    return ;
                }
            }
            else
                output.print(description.charAt(i));

            i++;
        }
    }

    /**
     * Add a JavaDoc comment
     *
     * @param output  the target file
     * @param obj   the object the header has to be added
     */
    private static void javadoc (PrintWriter output, IdlObject obj)
    {
        IdlComment comment = obj.getComment();
        String description = null;

        if (comment != null)
        {
            description = comment.get_description();

            if ((obj.kind() == IdlType.e_attribute) || (obj.kind() == IdlType.e_operation) || (obj.kind() == IdlType.e_state_member))
                output.print(tab + EMPTY_STRING);

            output.println("/**");

            if ((obj.kind() == IdlType.e_attribute) || (obj.kind() == IdlType.e_operation) || (obj.kind() == IdlType.e_state_member))
                output.print(tab + EMPTY_STRING);

            output.print(" * ");

            translate_comment_section(output, description, obj);

            IdlCommentSection [] sections = comment.get_sections();

            for (int i = 0; i < sections.length; i++)
            {
                switch (sections[ i ].kind().value())
                {

                case IdlCommentField._author_field :

                    if ((obj.kind() == IdlType.e_attribute) || (obj.kind() == IdlType.e_operation))
                        output.print(tab + EMPTY_STRING);

                    output.print(" * @author ");

                    break;

                case IdlCommentField._deprecated_field :
                    if ((obj.kind() == IdlType.e_attribute) || (obj.kind() == IdlType.e_operation))
                        output.print(tab + EMPTY_STRING);

                    output.print(" * @deprecated ");

                    break;

                case IdlCommentField._exception_field :
                    if ((obj.kind() == IdlType.e_attribute) || (obj.kind() == IdlType.e_operation))
                        output.print(tab + EMPTY_STRING);

                    output.print(" * @exception ");

                    break;

                case IdlCommentField._return_field :
                    if ((obj.kind() == IdlType.e_attribute) || (obj.kind() == IdlType.e_operation))
                        output.print(tab + EMPTY_STRING);

                    output.print(" * @return ");

                    break;

                case IdlCommentField._param_field :
                    if ((obj.kind() == IdlType.e_attribute) || (obj.kind() == IdlType.e_operation))
                        output.print(tab + EMPTY_STRING);

                    output.print(" * @param ");

                    break;

                case IdlCommentField._see_field :
                    if ((obj.kind() == IdlType.e_attribute) || (obj.kind() == IdlType.e_operation))
                        output.print(tab + EMPTY_STRING);

                    output.print(" * @see ");

                    break;

                case IdlCommentField._version_field :
                    if ((obj.kind() == IdlType.e_attribute) || (obj.kind() == IdlType.e_operation))
                        output.print(tab + EMPTY_STRING);

                    output.print(" * @version ");

                    break;

                case IdlCommentField._unknown_field :
                    if ((obj.kind() == IdlType.e_attribute) || (obj.kind() == IdlType.e_operation))
                        output.print(tab + EMPTY_STRING);

                    output.print(" * @" + sections[ i ].get_title() + " ");

                    break;
                }

                description = sections[ i ].get_description();
                translate_comment_section(output, description, obj);
            }

            if ((obj.kind() == IdlType.e_attribute) || (obj.kind() == IdlType.e_operation) || (obj.kind() == IdlType.e_state_member))
                output.print(tab + EMPTY_STRING);

            output.println(" */");
        }
    }

    /**
     * Returns the final type of a data type
     *
     * @param obj the object
     * @return the final type
     */
    private static int final_kind(IdlObject obj)
    {
        switch (obj.kind())
        {

        case IdlType.e_ident :
            return final_kind(((IdlIdent) obj).internalObject());

        case IdlType.e_typedef :

        case IdlType.e_union_member :

        case IdlType.e_struct_member :

        case IdlType.e_param :
            return final_kind(obj.current());

        default :
            return obj.kind();
        }
    }

    /**
     * Returns the final definition of a data type
     *
     * @param obj the object
     * @return the final definition
     */
    private static IdlObject final_type(IdlObject obj)
    {
        switch (obj.kind())
        {

        case IdlType.e_ident :
            return final_type(((IdlIdent) obj).internalObject());

        case IdlType.e_typedef :

        case IdlType.e_union_member :

        case IdlType.e_struct_member :

        case IdlType.e_param :
            return final_type(obj.current());

        default :
            return obj;
        }
    }

    /**
     * This method replaces in a path the file separator by '.'
     */
    private static String adaptToDot(String path)
    {
        char [] tmp = new char[ path.length() ];

        for (int i = 0; i < path.length(); i++)
        {
            if ((path.charAt(i) == '/') || (path.charAt(i) == '\\'))
                tmp[ i ] = '.';
            else
                tmp[ i ] = path.charAt(i);
        }

        return new String(tmp);
    }

	/**
	* Returns the complete name of a CORBA object
	*
	* @param obj the object the name has to be retrieved
	* @param prefix	prefix to be added
	* @return the complete name
	*/
	// PACKAGE HANDLING
	private static String fullname_CORBA (IdlObject obj)
	{
		return fullname(obj, null);
	}

	/**
	* Returns the complete name of an Abeans object
	*
	* @param obj the object the name has to be retrieved
	* @param prefix	prefix to be added
	* @return the complete name
	*/
	// PACKAGE HANDLING
	private static String fullname (IdlObject obj)
	{
		return fullname(obj, ABEANS_POSTFIX_DOTLESS);
	}

    /**
    * Returns the complete name of an object
    *
    * @param obj the object the name has to be retrieved
    * @return the complete name
    */
    private static String fullname (IdlObject obj, String postfix)
    {
        List v = new ArrayList();
        IdlObject obj2 = obj;
        String name = new String(EMPTY_STRING);
        String s;
        boolean first = false;

		// PACKAGE HANDLING
		boolean addedPostfix = false;
		boolean forceAddPostfixNext = false;

        while (obj2 != null)
        {
            if (first)
            {
                if ((obj2.kind() == IdlType.e_interface) ||
                        (obj2.kind() == IdlType.e_value) ||
                        (obj2.kind() == IdlType.e_struct) ||
                        (obj2.kind() == IdlType.e_union) ||
                        (obj2.kind() == IdlType.e_exception))
                    v.add((obj2.name() + "Package"));
                else
                    if (obj2.kind() != IdlType.e_union_member)
                        v.add(obj2.name());
            }
            else
                if (obj2.kind() != IdlType.e_union_member)
                    v.add(obj2.name());

			// PACKAGE HANDLING
			if (!addedPostfix)
			{
				if ((postfix != null && v.size() == 1) || forceAddPostfixNext)
				{
					if (obj2.kind() == IdlType.e_union_member || obj2.kind() == IdlType.e_enum_member)
					{
						forceAddPostfixNext = true;
					}
					else
					{
						//v.add(ABEANS_POSTFIX_DOTLESS);
						v.add(postfix);
						addedPostfix = true;
					}
				}
			}

            if (obj2.upper() != null)
                if (obj2.upper().kind() == IdlType.e_root)
                {
					// PACKAGE HANDLING
					if (postfix != null && !addedPostfix)
						//v.add(ABEANS_POSTFIX_DOTLESS);
						v.add(postfix);

                    break;
				}

            obj2 = obj2.upper();

            first = true;
        }

        if (IdlCompiler.packageName != null)
        {
            if (!obj.included())
            {
                if (!IdlCompiler.packageName.equals(EMPTY_STRING))
                {
                    if (!((IdlCompiler.packageName.equals(GENERATED)) && (IdlCompiler.use_package == false)))
                        name = adaptToDot(IdlCompiler.packageName);
                }
            }
        }

        if (IdlCompiler.usePrefix)
            if (obj.getPrefix() != null)
            {
                if (!name.equals(EMPTY_STRING))
                    name = name + ".";

                if (IdlCompiler.reversePrefix)
                    name = name + inversedPrefix(obj.getPrefix());
                else
                    name = name + obj.getPrefix();
            }

        for (int i = v.size() - 1; i >= 0; i--)
        {
            s = (String) v.get(i);

            if (s != null)
            {
                if (!name.equals(EMPTY_STRING))
                    name = name + ".";

                name = name + s;
            }
        }

        return name;
    }

    /**
     * Check if the the id is an enum member
     */
    private static boolean isEnumCase(String expr)
    {
        boolean isEnum = false;

        if (expr.indexOf("@") != -1)
            isEnum = true;

        return isEnum;
    }

    /**
     * Returns true if the element passed as argument is in the same
     * scope as the second argument.
     */
    private static boolean isSameScope(String ident, IdlObject obj)
    {
        IdlObject obj2 = obj.upper().returnVisibleObject(ident, false);

        if (obj2 == null)
            return false;

        if (obj2.upper().equals(obj.upper()))
            return true;

        return false;
    }

    /**
     * Check if obj constant is needing "l" qualifier.
     * @param obj
     * @return
     */
    private static boolean queryUseLongLiteral(final IdlObject obj) {
        switch (obj.kind()) {
            case IdlType.e_simple :
                switch (((IdlSimple)obj).internal()) {
                    case Token.t_float :
                    case Token.t_double :
                        return false;
                    default :
                        return true;
                }
            case IdlType.e_ident :
                return queryUseLongLiteral(((IdlIdent)obj).internalObject());
            case IdlType.e_typedef :
                obj.reset();
                return queryUseLongLiteral(obj.current());
            default :
                return false;
        }
    }

    /**
     * Translate a Scope IDL Ident::Ident into Scoped Java Ident.Ident.value
     *
     * @param expr the IDL expression
     * @return the equivalent Java expression
     */
    private static String IdlScopeToJavaScope(String expr, boolean complete, boolean fixed, IdlObject obj)
    {
        List s = new ArrayList();
        String mot = new String();
        int last = 0;
        boolean isEnum = false;

        for (int i = 0; i < expr.length(); i++)
        {
            if ((Character.isDigit(expr.charAt(i))) ||
                    (expr.charAt(i) == '-'))
            {
                if (expr.charAt(i) == '-')
                {
                    s.add(CharacterCache.getCharacter('-'));

                    if (expr.charAt(i++) == ' ')
                        continue;
                }

                while ((i != expr.length()) && (expr.charAt(i) != ' '))
                {

                    s.add(CharacterCache.getCharacter(expr.charAt(i)));

                    i++;
                }

                
                if (!fixed && queryUseLongLiteral(obj.current()))
                    s.add(CharacterCache.getCharacter('l'));
            }
            else
                if (expr.charAt(i) == '\"')
                {
                    i++;
                    s.add(CharacterCache.getCharacter('\"'));
                    boolean prev = false;
                    boolean stop = false;

                    while ((i != expr.length()) && (stop == false))
                    {
                        if ((expr.charAt(i) == '\"') && (prev == false))
                            stop = true;
                        else
                        {
                            prev = false;

                            if (expr.charAt(i) == '\\')
                                prev = true;

                            s.add(CharacterCache.getCharacter(expr.charAt(i)));

                            i++;
                        }
                    }

                    s.add(CharacterCache.getCharacter('\"'));
                }
                else
                    if (expr.charAt(i) == '\'')
                    {
                        i++;
                        s.add(CharacterCache.getCharacter('\''));
                        boolean prev = false;
                        boolean stop = false;

                        while ((i != expr.length()) && (stop == false))
                        {
                            if ((expr.charAt(i) == '\'') && (prev == false))
                                stop = true;
                            else
                            {
                                prev = false;

                                if (expr.charAt(i) == '\\')
                                    prev = true;

                                s.add(CharacterCache.getCharacter(expr.charAt(i)));

                                i++;
                            }
                        }

                        s.add(CharacterCache.getCharacter('\''));
                    }
                    else
                        if (Character.isLetter(expr.charAt(i)))
                        {
                            mot = EMPTY_STRING;

                            while ((i != expr.length()) && (expr.charAt(i) != ' '))
                            {
                                if (expr.charAt(i) == ':')
                                {
                                    i++;

                                    if (i < expr.length())
                                        if (expr.charAt(i) == ':')
                                        {
                                            s.add(CharacterCache.getCharacter('.'));
                                            last = s.size();
                                            i++;
                                        }
                                        else
                                            s.add(CharacterCache.getCharacter(':'));
                                }

                                if (expr.charAt(i) == '.')
                                    last = s.size() + 1;

                                s.add(CharacterCache.getCharacter(expr.charAt(i)));

                                mot = mot + expr.charAt(i);

                                i++;
                            }

                            if (!((mot.equals("true")) || (mot.equals("false"))))
                            {
                                isEnum = isEnumCase(expr);
                                boolean isInSameScope = isSameScope(mot, obj);

                                if (isEnum)
                                    s.remove(s.size() - 1);

                                if ((isEnum == false) && (isInSameScope == false))
                                {
                                    if (!mot.endsWith(".value"))
                                    {
                                        s.add(CharacterCache.getCharacter('.'));
                                        s.add(CharacterCache.getCharacter('v'));
                                        s.add(CharacterCache.getCharacter('a'));
                                        s.add(CharacterCache.getCharacter('l'));
                                        s.add(CharacterCache.getCharacter('u'));
                                        s.add(CharacterCache.getCharacter('e'));
                                    }
                                }
                                else
                                    if ((complete == true) && (isInSameScope == false))
                                    {
                                        s.add(last, CharacterCache.getCharacter('_'));
                                    }

                            }
                        }
                        else
                            if (expr.charAt(i) != ' ')
                                s.add (CharacterCache.getCharacter(expr.charAt(i)));
        }

        String newExpr = new String();

        for (int i = 0; i < s.size(); i++)
            newExpr = newExpr + ((Character) s.get(i)).charValue();

        return newExpr;
    }

    /**
     * Allow to test if the character is an hexa number
     *
     * @param c the character to test
     * @return true if the character is an hexa number
     */
    private static boolean isHexChar(char c)
    {
        switch (c)
        {

        case '0' :
        case '1' :
        case '2' :
        case '3' :
        case '4' :
        case '5' :
        case '6' :
        case '7' :
        case '8' :
		case '9' :
        case 'a' :
        case 'b' :
        case 'c' :
        case 'd' :
        case 'e' :
        case 'f' :
		case 'A' :
		case 'B' :
		case 'C' :
		case 'D' :
		case 'E' :
		case 'F' :
            return true;
        }

        return false;
    }

    /**
     * Change the IDL escape characters into CORBA escape characters
     *
     * @param expr the IDL expression
     * @return the equivalent Java expression
     */
    private static String IdlEspaceCharToJavaEscapeChar(String expr)
    {
        List s = new ArrayList();

        for (int i = 0; i < expr.length(); i++)
        {

            if (expr.charAt(i) == '\\')
            {
                s.add (CharacterCache.getCharacter('\\'));
                i++;

                switch (expr.charAt(i))
                {

                case '\\':
                    break;

                case 'a' :
                    s.add(CharacterCache.getCharacter('0'));
                    s.add(CharacterCache.getCharacter('0'));
                    s.add(CharacterCache.getCharacter('7'));
                    break;

                case 'v' :
                    s.add(CharacterCache.getCharacter('0'));
                    s.add(CharacterCache.getCharacter('1'));
                    s.add(CharacterCache.getCharacter('3'));
                    break;

                case 'x' :
                    i++;

                    while (isHexChar(expr.charAt(i)))
                    {
                        i++;
                    }

                    s.add(CharacterCache.getCharacter('3'));
                    s.add(CharacterCache.getCharacter('7'));
                    s.add(CharacterCache.getCharacter('7'));

                default :
                    s.add(CharacterCache.getCharacter(expr.charAt(i)));
                }
            }
            else
                s.add (CharacterCache.getCharacter(expr.charAt(i)));
        }

        String newExpr = new String();

        for (int i = 0; i < s.size(); i++)
            newExpr = newExpr + ((Character) s.get(i)).charValue();

        return newExpr;
    }

    /**
     * Translate an IDL expression into a Java expression
     *
     * @param expr the IDL expression
     * @return the equivalent Java expression
     */
    private static String translate_to_java_expression(String expr, boolean fixed, IdlObject obj)
    {

        String newExpr = IdlScopeToJavaScope(expr, true, fixed, obj);

        newExpr = IdlEspaceCharToJavaEscapeChar(newExpr);

        if (fixed)
            newExpr = "new java.math.BigDecimal(\"" + newExpr + "\")";

        return newExpr;
    }

    /**
     * Translate an IDL expression into an union expression
     *
     * @param expr the IDL expression
     * @return the equivalent Java expression
     */
    private static String translate_to_union_case_expression(IdlUnionMember disc, String expr)
    {
        String header = EMPTY_STRING;
        String newExpr = IdlScopeToJavaScope(expr, false, false, disc);
        newExpr = IdlEspaceCharToJavaEscapeChar(newExpr);

        disc.reset();

        switch (final_type(disc.current()).kind())
        {

        case IdlType.e_simple :
            IdlSimple simple = (IdlSimple) final_type(disc.current());

            switch (simple.internal())
            {

            case Token.t_short :

            case Token.t_ushort :
                header = "short)";
                break;

            case Token.t_long :

            case Token.t_ulong :
                header = "int)";
                break;

            case Token.t_longlong :

            case Token.t_ulonglong :
                header = "long)";
                break;

            case Token.t_char :

            case Token.t_wchar :
                header = "char)";
                break;

            case Token.t_boolean :
                header = "boolean)";
                break;

            case Token.t_octet :
                header = "byte)";
                break;
            }

            break;

        default :
            break;
        }

        if (header.equals(EMPTY_STRING))
            return newExpr;
        else
            return "(" + header + "(" + newExpr + ")";
    }

    /**
     * Translate a data type
     *
     * @param obj the object to translate
     * @param expression
     */
    private static String translate_type(IdlObject obj)
    {
        IdlSimple simple = null;

        switch (obj.kind())
        {

        case IdlType.e_simple :
            simple = (IdlSimple) obj;

            switch (simple.internal())
            {

            case Token.t_void :
                return "void";

            case Token.t_float :
                return "float";

            case Token.t_double :
                return "double";

            case Token.t_short :
            case Token.t_ushort :
                return "short";

            case Token.t_long :
            case Token.t_ulong :
                return "int";

            case Token.t_longlong :
            case Token.t_ulonglong :
                return "long";

            case Token.t_char :
            case Token.t_wchar :
                return "char";

            case Token.t_boolean :
                return "boolean";

            case Token.t_octet :
                return "byte";

            case Token.t_any :
            case Token.t_typecode :
            case Token.t_object :
                return "java.lang.Object";

            case Token.t_ValueBase :
                return "Serializable";
            }

            break;

        case IdlType.e_fixed :
            return "java.math.BigDecimal";

        case IdlType.e_string :
        case IdlType.e_wstring :
	        return "String";

        case IdlType.e_struct :
        case IdlType.e_union :
        case IdlType.e_enum :

        case IdlType.e_interface :
        case IdlType.e_forward_interface :

        case IdlType.e_exception :

        case IdlType.e_value :
        case IdlType.e_forward_value :
            return fullname(obj);

		case IdlType.e_typedef :
			obj.reset();
			return translate_type(obj.current());

		case IdlType.e_sequence :
		case IdlType.e_array :
			return translate_type(obj.current()) + "[]";

		case IdlType.e_ident :
			return translate_type(((IdlIdent) obj).internalObject());

		// TODO check
        case IdlType.e_native :
        
            if (isNativeDefinition(obj))
                //printNativeDefinition(obj, output);
				return "null /* dont now how to handle */";
            else
                return fullname(obj);

        case IdlType.e_value_box :
            if (((IdlValueBox) obj).simple())
                return fullname(obj);
            else
            {
                obj.reset();
                return translate_type(obj.current());
            }

        }

		return "null /* dont now how to handle */";
    }

	/**
	 * Translate a data type
	 *
	 * @param obj the object to translate
	 * @param output the write access
	 */
	private static void translate_type(IdlObject obj, PrintWriter output)
	{
		output.print(translate_type(obj));
	}

	/**
	 * Translate a data type
	 *
	 * @param obj the object to translate
	 * @retun expression
	 */
	private static String translate_type_CORBA(IdlObject obj)
	{
		IdlSimple simple = null;

		switch (obj.kind())
		{

		case IdlType.e_simple :
			simple = (IdlSimple) obj;

			switch (simple.internal())
			{

			case Token.t_void :
				return "void";

			case Token.t_float :
				return "float";

			case Token.t_double :
				return "double";

			case Token.t_short :
			case Token.t_ushort :
				return "short";

			case Token.t_long :
			case Token.t_ulong :
				return "int";

			case Token.t_longlong :
			case Token.t_ulonglong :
				return "long";

			case Token.t_char :
			case Token.t_wchar :
				return "char";

			case Token.t_boolean :
				return "boolean";

			case Token.t_octet :
				return "byte";

			case Token.t_any :
				return "org.omg.CORBA.Any";

			case Token.t_typecode :
				return "org.omg.CORBA.TypeCode";

			case Token.t_object :
				return "org.omg.CORBA.Object";

			case Token.t_ValueBase :
				return "Serializable";
			}

			break;

		case IdlType.e_fixed :
			return "java.math.BigDecimal";

		case IdlType.e_string :
		case IdlType.e_wstring :
			return "String";

		case IdlType.e_struct :
		case IdlType.e_union :
		case IdlType.e_enum :

		case IdlType.e_interface :
		case IdlType.e_forward_interface :

		case IdlType.e_exception :
	
		case IdlType.e_value :
		case IdlType.e_forward_value :
			return fullname_CORBA(obj);

		case IdlType.e_typedef :
			obj.reset();
			return translate_type_CORBA(obj.current());

		case IdlType.e_sequence :
		case IdlType.e_array :
			return translate_type_CORBA(obj.current()) + "[]";

		case IdlType.e_ident :
			return translate_type_CORBA(((IdlIdent) obj).internalObject());

		//
		// TODO check 
		//
		case IdlType.e_native :

			if (isNativeDefinition(obj))
				//printNativeDefinition(obj, output);
				return "null /* dont now how to handle */";
			else
				return fullname_CORBA(obj);

		case IdlType.e_value_box :
			if (((IdlValueBox) obj).simple())
				return fullname_CORBA(obj);
			else
			{
				obj.reset();
				return translate_type_CORBA(obj.current());
			}

		}
		
		return "null /* dont now how to handle */";
	}

	/**
	 * Translate a data type
	 *
	 * @param obj the object to translate
	 * @param output the write access
	 */
	private static void translate_type_CORBA(IdlObject obj, PrintWriter output)
	{
		output.print(translate_type_CORBA(obj));
	}

    /**
     * Translate a parameter
     *
     * @param obj  param object to translate
     * @param attr parameter attribute
     * @return expression
     */
    private static String translate_parameter(IdlObject obj, int attr)
    {
        IdlSimple simple = null;

        switch (obj.kind())
        {

        case IdlType.e_simple :
            simple = (IdlSimple) obj;

            switch (simple.internal())
            {

            case Token.t_float :
                if (attr == 0)
                    return "float";
                else
                    return "abeans.models.acs.baci.util.FloatHolder";

            case Token.t_double :
                if (attr == 0)
                    return "double";
                else
                    return "abeans.models.acs.baci.util.DoubleHolder";

            case Token.t_short :
            case Token.t_ushort :
                if (attr == 0)
                    return "short";
                else
                    return "abeans.models.acs.baci.util.ShortHolder";

            case Token.t_long :
            case Token.t_ulong :
                if (attr == 0)
                    return "int";
                else
                    return "abeans.models.acs.baci.util.IntHolder";

            case Token.t_longlong :
            case Token.t_ulonglong :
                if (attr == 0)
                    return "long";
                else
                    return "abeans.models.acs.baci.util.LongHolder";

            case Token.t_char :
            case Token.t_wchar :
                if (attr == 0)
                    return "char";
                else
                    return "abeans.models.acs.baci.util.CharHolder";

            case Token.t_boolean :
                if (attr == 0)
                    return "boolean";
                else
                    return "abeans.models.acs.baci.util.BooleanHolder";

            case Token.t_octet :
                if (attr == 0)
                    return "byte";
                else
                    return "abeans.models.acs.baci.util.ByteHolder";
			
			case Token.t_any :
			case Token.t_typecode :
			case Token.t_object :
				if (attr == 0)
					return "java.lang.Object";
				else
					return "abeans.models.acs.baci.util.ObjectHolder";

			case Token.t_ValueBase :
				if (attr == 0)
					return "Serializable";
				else
					return "abeans.models.acs.baci.util.ObjectHolder";
            }

            break;
		
        case IdlType.e_fixed :
            if (attr == 0)
                return "java.math.BigDecimal";
            else
                return "abeans.models.acs.baci.util.FixedHolder";

        case IdlType.e_string :
        case IdlType.e_wstring :
            if (attr == 0)
                return "String";
            else
                return "abeans.models.acs.baci.util.StringHolder";

        case IdlType.e_struct :
        case IdlType.e_union :
        case IdlType.e_enum :
        case IdlType.e_interface :
        case IdlType.e_forward_interface :
        // TODO check
        case IdlType.e_value :
        case IdlType.e_forward_value :
            if (attr == 0)
                return fullname(obj);
            else
                return fullname(obj) + "Holder";

		case IdlType.e_sequence :
		case IdlType.e_array :
			if (attr == 0)
			{
				return translate_parameter(obj.current(), 0) + "[]";
			}
			else
			{
				return fullname(obj.upper()) + "Holder";
			}

		case IdlType.e_typedef :
			obj.reset();

			if (attr != 0)
			{
				/*
				if ((final_type(obj).kind() == IdlType.e_simple) ||
				(final_type(obj).kind() == IdlType.e_string) ||
				(final_type(obj).kind() == IdlType.e_wstring))
				translate_parameter(obj.current(), output, attr);
				else
				return fullname(obj)+"Holder");*/

				if ((final_type(obj).kind() == IdlType.e_sequence) ||
						(final_type(obj).kind() == IdlType.e_array))
					return fullname(obj) + "Holder";
				else
					return translate_parameter(obj.current(), attr);
			}
			else
				return translate_parameter(obj.current(), 0);

		// TODO check
		case IdlType.e_value_box :
			if (attr == 0)
			{
				if (((IdlValueBox) obj).simple())
					return fullname(obj);
				else
				{
					obj.reset();
					return translate_parameter(obj.current() , attr);
				}
			}
			else
				return fullname(obj) + "Holder";

        case IdlType.e_native :
            if (attr != 0)
            {
                /*
                if (isNativeDefinition(obj))
            {
                printNativeDefinition(obj,output);
                return "Holder");
            }
                else*/
                return fullname(obj) + "Holder";
            }
            else
            {
                if (isNativeDefinition(obj))
                    //printNativeDefinition(obj, output);
					return "null /* dont now how to handle */";
                else
                    return fullname(obj);
            }

        case IdlType.e_ident :
            return translate_parameter(((IdlIdent) obj).internalObject(), attr);
        }

		return "null /* dont now how to handle */";
    }

	/**
	 * Translate a parameter
	 *
	 * @param obj  param object to translate
	 * @param output write access
	 * @param attr parameter attribute
	 */
	private static void translate_parameter(IdlObject obj, PrintWriter output, int attr)
	{
		output.print(translate_parameter(obj, attr));
		/*
	    IdlSimple simple = null;
	
	    switch (obj.kind())
	    {
	
	    case IdlType.e_simple :
	        simple = (IdlSimple) obj;
	
	        switch (simple.internal())
	        {
	
	        case Token.t_float :
	
	            if (attr == 0)
	                output.print("float");
	            else
	                output.print("abeans.models.acs.baci.util.FloatHolder");
	
	            break;
	
	        case Token.t_double :
	            if (attr == 0)
	                output.print("double");
	            else
	                output.print("abeans.models.acs.baci.util.DoubleHolder");
	
	            break;
	
	        case Token.t_short :
	
	        case Token.t_ushort :
	            if (attr == 0)
	                output.print("short");
	            else
	                output.print("abeans.models.acs.baci.util.ShortHolder");
	
	            break;
	
	        case Token.t_long :
	
	        case Token.t_ulong :
	            if (attr == 0)
	                output.print("int");
	            else
	                output.print("abeans.models.acs.baci.util.IntHolder");
	
	            break;
	
	        case Token.t_longlong :
	
	        case Token.t_ulonglong :
	            if (attr == 0)
	                output.print("long");
	            else
	                output.print("abeans.models.acs.baci.util.LongHolder");
	
	            break;
	
	        case Token.t_char :
	
	        case Token.t_wchar :
	            if (attr == 0)
	                output.print("char");
	            else
	                output.print("abeans.models.acs.baci.util.CharHolder");
	
	            break;
	
	        case Token.t_boolean :
	            if (attr == 0)
	                output.print("boolean");
	            else
	                output.print("abeans.models.acs.baci.util.BooleanHolder");
	
	            break;
	
	        case Token.t_octet :
	            if (attr == 0)
	                output.print("byte");
	            else
	                output.print("abeans.models.acs.baci.util.ByteHolder");
	
	            break;
			
			case Token.t_any :
			case Token.t_typecode :
			case Token.t_object :
				if (attr == 0)
					output.print("java.lang.Object");
				else
					output.print("abeans.models.acs.baci.util.ObjectHolder");
	
				break;
	
			case Token.t_ValueBase :
				if (attr == 0)
					output.print("Serializable");
				else
					output.print("abeans.models.acs.baci.util.ObjectHolder");
	
				break;
	        }
	
	        break;
		
	    case IdlType.e_fixed :
	
	        if (attr == 0)
	            output.print("java.math.BigDecimal");
	        else
	            output.print("abeans.models.acs.baci.util.FixedHolder");
	
	        break;
	
	    case IdlType.e_string :
	
	    case IdlType.e_wstring :
	        if (attr == 0)
	            output.print("String");
	        else
	            output.print("abeans.models.acs.baci.util.StringHolder");
	
	        break;
	
	    case IdlType.e_value_box :
	        if (attr == 0)
	        {
	            if (((IdlValueBox) obj).simple())
	                output.print(fullname(obj));
	            else
	            {
	                obj.reset();
	                translate_parameter(obj.current() , output, attr);
	            }
	        }
	        else
	            output.print(fullname(obj) + "Holder");
	
	        break;
	
	    case IdlType.e_struct :
	
	    case IdlType.e_union :
	
	    case IdlType.e_enum :
	
	    case IdlType.e_interface :
	
	    case IdlType.e_forward_interface :
	
	    case IdlType.e_value :
	
	    case IdlType.e_forward_value :
	        if (attr == 0)
	            output.print(fullname(obj));
	        else
	            output.print(fullname(obj) + "Holder");
	
	        break;
	
	    case IdlType.e_native :
	        if (attr != 0)
	        {
	            /*
	            if (isNativeDefinition(obj))
	        {
	            printNativeDefinition(obj,output);
	            output.print("Holder");
	        }
	            else*/
	      /*      output.print(fullname(obj) + "Holder");
	        }
	        else
	        {
	            if (isNativeDefinition(obj))
	                printNativeDefinition(obj, output);
	            else
	                output.print(fullname(obj));
	        }
	
	        break;
	
	    case IdlType.e_typedef :
	        obj.reset();
	
	        if (attr != 0)
	        {
	            /*
	            if ((final_type(obj).kind() == IdlType.e_simple) ||
	            (final_type(obj).kind() == IdlType.e_string) ||
	            (final_type(obj).kind() == IdlType.e_wstring))
	            translate_parameter(obj.current(), output, attr);
	            else
	            output.print(fullname(obj)+"Holder");*/
	/*
	            if ((final_type(obj).kind() == IdlType.e_sequence) ||
	                    (final_type(obj).kind() == IdlType.e_array))
	                output.print(fullname(obj) + "Holder");
	            else
	                translate_parameter(obj.current(), output, attr);
	        }
	        else
	            translate_parameter(obj.current(), output, attr);
	
	        break;
	
	    case IdlType.e_sequence :
	
	    case IdlType.e_array :
	        if (attr == 0)
	        {
	            translate_parameter(obj.current(), output, attr);
	            output.print("[]");
	        }
	        else
	        {
	            output.print(fullname(obj.upper()) + "Holder");
	        }
	
	        break;
	
	    case IdlType.e_ident :
	        translate_parameter(((IdlIdent) obj).internalObject(), output, attr);
	        break;
	    }*/
	}

	/**
	 * Translate a parameter as CORBA does
	 *
	 * @param obj  param object to translate
	 * @param output write access
	 * @param attr parameter attribute
	 */
	private static void translate_parameter_CORBA(IdlObject obj, PrintWriter output, int attr)
	{
		IdlSimple simple = null;

		switch (obj.kind())
		{

		case IdlType.e_simple :
			simple = (IdlSimple) obj;

			switch (simple.internal())
			{

			case Token.t_float :

				if (attr == 0)
					output.print("float");
				else
					output.print("org.omg.CORBA.FloatHolder");

				break;

			case Token.t_double :
				if (attr == 0)
					output.print("double");
				else
					output.print("org.omg.CORBA.DoubleHolder");

				break;

			case Token.t_short :

			case Token.t_ushort :
				if (attr == 0)
					output.print("short");
				else
					output.print("org.omg.CORBA.ShortHolder");

				break;

			case Token.t_long :

			case Token.t_ulong :
				if (attr == 0)
					output.print("int");
				else
					output.print("org.omg.CORBA.IntHolder");

				break;

			case Token.t_longlong :

			case Token.t_ulonglong :
				if (attr == 0)
					output.print("long");
				else
					output.print("org.omg.CORBA.LongHolder");

				break;

			case Token.t_char :

			case Token.t_wchar :
				if (attr == 0)
					output.print("char");
				else
					output.print("org.omg.CORBA.CharHolder");

				break;

			case Token.t_boolean :
				if (attr == 0)
					output.print("boolean");
				else
					output.print("org.omg.CORBA.BooleanHolder");

				break;

			case Token.t_octet :
				if (attr == 0)
					output.print("byte");
				else
					output.print("org.omg.CORBA.ByteHolder");

				break;

			case Token.t_any :
				if (attr == 0)
					output.print("org.omg.CORBA.Any");
				else
					output.print("org.omg.CORBA.AnyHolder");

				break;

			case Token.t_typecode :
				if (attr == 0)
					output.print("org.omg.CORBA.TypeCode");
				else
					output.print("org.omg.CORBA.TypeCodeHolder");

				break;

			case Token.t_object :
				if (attr == 0)
					output.print("org.omg.CORBA.Object");
				else
					output.print("org.omg.CORBA.ObjectHolder");

				break;

			case Token.t_ValueBase :
				if (attr == 0)
					output.print("Serializable");
				else
					output.print("org.omg.CORBA.ValueBaseHolder");

				break;
			}

			break;

		case IdlType.e_fixed :

			if (attr == 0)
				output.print("java.math.BigDecimal");
			else
				output.print("org.omg.CORBA.FixedHolder");

			break;

		case IdlType.e_string :

		case IdlType.e_wstring :
			if (attr == 0)
				output.print("String");
			else
				output.print("org.omg.CORBA.StringHolder");

			break;

		// TODO check, also below
		case IdlType.e_value_box :
			if (attr == 0)
			{
				if (((IdlValueBox) obj).simple())
					output.print(fullname(obj));
				else
				{
					obj.reset();
					translate_parameter_CORBA(obj.current() , output, attr);
				}
			}
			else
				output.print(fullname(obj) + "Holder");

			break;

		case IdlType.e_struct :

		case IdlType.e_union :

		case IdlType.e_enum :

		case IdlType.e_interface :

		case IdlType.e_forward_interface :

		case IdlType.e_value :

		// TODO check
		case IdlType.e_forward_value :
			if (attr == 0)
				output.print(fullname_CORBA(obj));
			else
				output.print(fullname_CORBA(obj) + "Holder");

			break;

		case IdlType.e_native :
			if (attr != 0)
			{
				/*
				if (isNativeDefinition(obj))
			{
				printNativeDefinition(obj,output);
				output.print("Holder");
			}
				else*/
				output.print(fullname(obj) + "Holder");
			}
			else
			{
				if (isNativeDefinition(obj))
					printNativeDefinition(obj, output);
				else
					output.print(fullname(obj));
			}

			break;

		case IdlType.e_typedef :
			obj.reset();

			if (attr != 0)
			{
				/*
				if ((final_type(obj).kind() == IdlType.e_simple) ||
				(final_type(obj).kind() == IdlType.e_string) ||
				(final_type(obj).kind() == IdlType.e_wstring))
				translate_parameter(obj.current(), output, attr);
				else
				output.print(fullname(obj)+"Holder");*/

				if ((final_type(obj).kind() == IdlType.e_sequence) ||
						(final_type(obj).kind() == IdlType.e_array))
					output.print(fullname_CORBA(obj) + "Holder");
				else
					translate_parameter_CORBA(obj.current(), output, attr);
			}
			else
				translate_parameter_CORBA(obj.current(), output, attr);

			break;

		case IdlType.e_sequence :

		case IdlType.e_array :
			if (attr == 0)
			{
				translate_parameter_CORBA(obj.current(), output, attr);
				output.print("[]");
			}
			else
			{
				output.print(fullname_CORBA(obj.upper()) + "Holder");
			}

			break;

		case IdlType.e_ident :
			translate_parameter_CORBA(((IdlIdent) obj).internalObject(), output, attr);
			break;
		}
	}

    /**
     * Return simple array name or null else
     */
    private static String get_array_name(IdlObject obj)
    {
        switch (final_kind(obj))
        {

        case IdlType.e_simple :
            IdlSimple simple = (IdlSimple) final_type(obj);

            switch (simple.internal())
            {

            case Token.t_float :
                return "float";

            case Token.t_double :
                return "double";

            case Token.t_short :
                return "short";

            case Token.t_ushort :
                return "ushort";

            case Token.t_long :
                return "long";

            case Token.t_ulong :
                return "ulong";

            case Token.t_longlong :
                return "longlong";

            case Token.t_ulonglong :
                return "ulonglong";

            case Token.t_char :
                return "char";

            case Token.t_wchar :
                return "wchar";

            case Token.t_boolean :
                return "boolean";

            case Token.t_octet :
                return "octet";

            default :
                return null;
            }

        default :
            return null;
        }
    }

    /**
     * Encode a member data type
     *
     * @param obj member to encode
     * @param output write access
     * @param outname outputstream name
     * @param tname data type name
     * @param space indent space
     */
    // TODO org.omg -> translate_marshalling_member
    private static void translate_marshalling_member (IdlObject obj, PrintWriter output, String outname, String tname, String space)
    {
        IdlSimple simple = null;
        String array_name = null;
        int val;

        switch (obj.kind())
        {

        case IdlType.e_simple :
            simple = (IdlSimple) obj;

            if (simple.internal() == Token.t_ValueBase)
            {
                output.println(space + "((org.omg.CORBA_2_3.portable.OutputStream)" + outname + ").write_value(" + tname + ");");
                return ;
            }

            output.print(space + outname + ".write");

            switch (simple.internal())
            {

            case Token.t_float :
                output.println("_float(" + tname + ");");
                break;

            case Token.t_double :
                output.println("_double(" + tname + ");");
                break;

            case Token.t_short :
                output.println("_short(" + tname + ");");
                break;

            case Token.t_ushort :
                output.println("_ushort(" + tname + ");");
                break;

            case Token.t_long :
                output.println("_long(" + tname + ");");
                break;

            case Token.t_ulong :
                output.println("_ulong(" + tname + ");");
                break;

            case Token.t_longlong :
                output.println("_longlong(" + tname + ");");
                break;

            case Token.t_ulonglong :
                output.println("_ulonglong(" + tname + ");");
                break;

            case Token.t_char :
                output.println("_char(" + tname + ");");
                break;

            case Token.t_wchar :
                output.println("_wchar(" + tname + ");");
                break;

            case Token.t_boolean :
                output.println("_boolean(" + tname + ");");
                break;

            case Token.t_octet :
                output.println("_octet(" + tname + ");");
                break;

            case Token.t_any :
                output.println("_any(" + tname + ");");
                break;

            case Token.t_typecode :
                output.println("_TypeCode(" + tname + ");");
                break;

            case Token.t_object :
                output.println("_Object(" + tname + ");");
                break;
            }

            break;

        case IdlType.e_fixed :
            output.println(space + outname + ".write_fixed(" + tname + ");");
            break;

        case IdlType.e_string :
            output.println(space + outname + ".write_string(" + tname + ");");
            break;

        case IdlType.e_wstring :
            output.println(space + outname + ".write_wstring(" + tname + ");");
            break;

        case IdlType.e_native :

        case IdlType.e_interface :

        case IdlType.e_forward_interface :

        case IdlType.e_struct :

        case IdlType.e_union :

        case IdlType.e_enum :

        case IdlType.e_value_box :

        case IdlType.e_value :

        case IdlType.e_forward_value :
            output.print(space + fullname(obj));
            output.println("Helper.write(" + outname + "," + tname + ");");
            break;

        case IdlType.e_typedef :
            output.print(space + fullname(obj));
            output.println("Helper.write(" + outname + "," + tname + ");");
            break;

        case IdlType.e_array :
            array_name = get_array_name(obj.current());

            if (array_name == null)
            {
                val = space.length() - 1;
                output.println(space + "if (" + tname + ".length != " + ((IdlArray) obj).getDimension() + ") ");
                output.println(space + "   throw new org.omg.CORBA.MARSHAL();");
                output.println(space + "for (int i" + val + "=0; i" + val + "<" + tname + ".length; i" + val + "++)");
                output.println(space + "{");
                translate_marshalling_member(obj.current(), output, outname, tname + "[i" + val + "]", space + tab + EMPTY_STRING);
                output.println();
                output.println(space + "}");
            }
            else
            {
                output.println(space + outname + ".write_" + array_name + "_array(" + tname + ", 0," + tname + ".length);");
            }

            break;

        case IdlType.e_sequence :
            array_name = get_array_name(obj.current());

            if (array_name == null)
            {
                val = space.length() - 1;
                output.println(space + outname + ".write_ulong(" + tname + ".length);");
                output.println(space + "for (int i" + val + "=0; i" + val + "<" + tname + ".length; i" + val + "++)");
                output.println(space + "{");
                translate_marshalling_member(obj.current(), output, outname, tname + "[i" + val + "]", space + tab + EMPTY_STRING);
                output.println();
                output.println(space + "}");
            }
            else
            {
                output.println(space + outname + ".write_ulong(" + tname + ".length);");
                output.println(space + outname + ".write_" + array_name + "_array(" + tname + ", 0," + tname + ".length);");
            }

            break;

        case IdlType.e_ident :
            translate_marshalling_member(((IdlIdent) obj).internalObject(), output, outname, tname, space);
            break;
        }
    }

    /**
     * Encode a member data type
     *
     * @param obj the member to encode
     * @param output write access
     * @param inname inputstream name
     * @param tname data type name
     * @param space indent space
     */
    // TODO org.omg -> translate_unmarshalling_member
    private static void translate_unmarshalling_member (IdlObject obj, PrintWriter output, String inname, String tname, String space)
    {
        IdlSimple simple = null;
        String array_name = null;
        IdlObject o = null;
        int val;
        int next;

        switch (obj.kind())
        {

        case IdlType.e_simple :
            simple = (IdlSimple) obj;

            if (simple.internal() == Token.t_ValueBase)
            {
                output.println(space + tname + " = ((org.omg.CORBA_2_3.portable.InputStream)" + inname + ").read_value();");
                return ;
            }

            output.print(space + tname + " = " + inname + ".read");

            switch (simple.internal())
            {

            case Token.t_float :
                output.println("_float();");
                break;

            case Token.t_double :
                output.println("_double();");
                break;

            case Token.t_short :
                output.println("_short();");
                break;

            case Token.t_ushort :
                output.println("_ushort();");
                break;

            case Token.t_long :
                output.println("_long();");
                break;

            case Token.t_ulong :
                output.println("_ulong();");
                break;

            case Token.t_longlong :
                output.println("_longlong();");
                break;

            case Token.t_ulonglong :
                output.println("_ulonglong();");
                break;

            case Token.t_char :
                output.println("_char();");
                break;

            case Token.t_wchar :
                output.println("_wchar();");
                break;

            case Token.t_boolean :
                output.println("_boolean();");
                break;

            case Token.t_octet :
                output.println("_octet();");
                break;

            case Token.t_any :
                output.println("_any();");
                break;

            case Token.t_typecode :
                output.println("_TypeCode();");
                break;

            case Token.t_object :
                output.println("_Object();");
                break;

            case Token.t_ValueBase :
                output.println("_value();");
                break;
            }

            break;

        case IdlType.e_fixed :
            output.println(space + tname + " =" + inname + ".read_fixed();");
            break;

        case IdlType.e_string :
            output.println(space + tname + " = " + inname + ".read_string();");
            break;

        case IdlType.e_wstring :
            output.println(space + tname + " = " + inname + ".read_wstring();");
            break;

        case IdlType.e_struct :

        case IdlType.e_union :

        case IdlType.e_enum :

        case IdlType.e_native :

        case IdlType.e_interface :

        case IdlType.e_forward_interface :

        case IdlType.e_value_box :

        case IdlType.e_value :

        case IdlType.e_forward_value :
            output.print(space + tname + " = " + fullname(obj));
            output.println("Helper.read(" + inname + ");");
            break;

        case IdlType.e_typedef :
            output.print(space + tname + " = " + fullname(obj));
            output.println("Helper.read(" + inname + ");");
            break;

        case IdlType.e_array :
            val = space.length() - 1;
            output.println(space + "{");
            output.println(space + "int size" + val + " = " + ((IdlArray) obj).getDimension() + ";");
            output.print(space + tname + " = new ");

            obj.reset();
            o = final_type(obj.current());
            next = 0;

            while ((o.kind() == IdlType.e_array) ||
                    (o.kind() == IdlType.e_sequence))
            {
                o.reset();
                next++;
                o = final_type(o.current());
            }

            translate_type(o, output);

            output.print("[size" + val + "]");

            for (int i = 0; i < next; i++)
                output.print("[]");

            output.println(";");

            array_name = get_array_name(obj.current());

            if (array_name == null)
            {
                output.println(space + "for (int i" + val + "=0; i" + val + "<" + tname + ".length; i" + val + "++)");
                output.println(space + " {");
                translate_unmarshalling_member(obj.current(), output, inname, tname + "[i" + val + "]", space + tab + EMPTY_STRING);
                output.println();
                output.println(space + " }");
            }
            else
            {
                output.println(space + inname + ".read_" + array_name + "_array(" + tname + ", 0, " + tname + ".length);");
            }

            output.println(space + "}");
            break;

        case IdlType.e_sequence :
            val = space.length() - 1;
            output.println(space + "{");
            output.println(space + "int size" + val + " = " + inname + ".read_ulong();");
            output.print(space + tname + " = new ");

            obj.reset();
            o = final_type(obj.current());
            next = 0;

            while ((o.kind() == IdlType.e_array) ||
                    (o.kind() == IdlType.e_sequence))
            {
                o.reset();
                next++;
                o = final_type(o.current());
            }

            translate_type(o, output);

            output.print("[size" + val + "]");

            for (int i = 0; i < next; i++)
                output.print("[]");

            output.println(";");

            array_name = get_array_name(obj.current());

            if (array_name == null)
            {
                output.println(space + "for (int i" + val + "=0; i" + val + "<" + tname + ".length; i" + val + "++)");
                output.println(space + " {");
                translate_unmarshalling_member(obj.current(), output, inname, tname + "[i" + val + "]", space + tab + EMPTY_STRING);
                output.println();
                output.println(space + " }");
            }
            else
            {
                output.println(space + inname + ".read_" + array_name + "_array(" + tname + ", 0, " + tname + ".length);");
            }

            output.println(space + "}");
            break;

        case IdlType.e_ident :
            translate_unmarshalling_member(((IdlIdent) obj).internalObject(), output, inname, tname, space);
            break;
        }
    }

    /**
     * Map <code>obj</code> to java.lang.Object.
     * Examples: 
     * 		outname = tname;
     * 		outname = new Integer(tname);
     * 
     * @param obj the member to encode
     * @param parameter
     * @return	expression
     */
    private static String translate_to_object (IdlObject obj, String tname)
    {
        IdlSimple simple = null;

		obj = final_type(obj);

        if (obj.kind() == IdlType.e_simple)
        {
            simple = (IdlSimple) obj;
            switch (simple.internal())
            {

	            case Token.t_float :
	                return "new Float(" + tname + ")";
	
	            case Token.t_double :
					return "new Double(" + tname + ")";
	
	            case Token.t_short :
				case Token.t_ushort :
					return "new Short(" + tname + ")";
	
	            case Token.t_long :
				case Token.t_ulong :
					return "new Integer(" + tname + ")";
	
	            case Token.t_longlong :
				case Token.t_ulonglong :
					return "new Long(" + tname + ")";
	
	            case Token.t_char :
				case Token.t_wchar :
					return "new Character(" + tname + ")";

	            case Token.t_boolean :
					return "new Boolean(" + tname + ")";
	
	            case Token.t_octet :
					return "new Byte(" + tname + ")";
            }
        }
        
        // all other objects are already instances of java.lang.Object
        return tname;

    }

	/**
	 * Casts (sequence) primitive type to apripriate java.lang.Object type.
	 * Examples: 
	 * 		inname;
	 * 		((Double)inname);
	 *
	 *  
	 * @param obj the member to decode
	 * @param inname parameter
	 * @return expression
	 */
	private static String cast_to_abeans_type (IdlObject obj, String inname, boolean toObject) 
	{
		return cast_to_abeans_type(obj, inname, toObject, false);
	}
	
    /**
     * Casts (sequence) primitive type to apropriate java.lang.Object type.
     * Examples: 
     * 		inname;
     * 		((Double)inname);
     *
     *  
     * @param obj the member to decode
     * @param inname parameter
     * @param stripOffSequence
     * @return expression
     */
    private static String cast_to_abeans_type (IdlObject obj, String inname, boolean toObject, boolean stripOffSequence)
    {
        IdlSimple simple = null;

		obj = final_type(obj);

        switch (obj.kind())
        {

        case IdlType.e_simple :
            simple = (IdlSimple) obj;

            switch (simple.internal())
            {
			case Token.t_float :
				if (toObject)
					return "new Double(((Float)" + inname + ").doubleValue())";
				else
					return "((Float)" + inname + ").doubleValue()";

			case Token.t_double :
				if (toObject)
					return inname;
				else
					return "((Double)" + inname + ").doubleValue()";

			case Token.t_short :
			case Token.t_ushort :
				if (toObject)
					return "new Long(((Short)" + inname + ").longValue())";
				else
					return "((Short)" + inname + ").longValue()";

			case Token.t_long :
			case Token.t_ulong :
				if (toObject)
					return "new Long(((Integer)" + inname + ").longValue())";
				else
					return "((Integer)" + inname + ").longValue()";

			case Token.t_longlong :
			case Token.t_ulonglong :
				if (toObject)
					return inname;
				else
					return "((Long)" + inname + ").longValue()";

			case Token.t_char :
			case Token.t_wchar :
				return "((Character)" + inname + ")"; //.charValue()";

			case Token.t_boolean :
				return "((Boolean)" + inname + ")"; //.booleanValue()";

			case Token.t_octet :
				if (toObject)
					return "new Long(((Byte)" + inname + ").longValue())";
				else
					return "((Byte)" + inname + ").longValue()";

			case Token.t_any :
			case Token.t_typecode :
			case Token.t_object :
			case Token.t_ValueBase :
				return inname;
			}

			break;

		case IdlType.e_interface :
		case IdlType.e_forward_interface :

		case IdlType.e_fixed :

		case IdlType.e_string :
		case IdlType.e_wstring :

		case IdlType.e_struct :
		case IdlType.e_union :
		case IdlType.e_enum :
		
		case IdlType.e_native :
		case IdlType.e_value_box :
		case IdlType.e_value :
		case IdlType.e_forward_value :

			return inname;

		case IdlType.e_array :
		case IdlType.e_sequence :
		
			if (stripOffSequence)
				return cast_to_abeans_type(obj.current(), inname, toObject);
			else
				return "abeans.models.acs.baci.util.SequenceConverterHelper.toAbeansSequence((" + translate_type(obj.current()) + "[])" + inname + ")";
		
		case IdlType.e_typedef :
			return cast_to_abeans_type(((IdlTypeDef) obj).current(), inname, toObject);

		case IdlType.e_ident :
			return cast_to_abeans_type(((IdlIdent) obj).internalObject(), inname, toObject);
		
		}
		
		return "null";
    }

	/**
	 * Casts Abeans (sequence) primitive type to appropriate java.lang.Object type.
	 * Examples: 
	 * 		inname;
	 * 		((Double)inname);
	 *
	 *  
	 * @param obj the member to decode
	 * @param inname parameter
	 * @param stripOffSequence
	 * @return expression
	 */
	private static String cast_to_object_type (IdlObject obj, String inname, boolean fromObject)
	{
		IdlSimple simple = null;

		obj = final_type(obj);

		switch (obj.kind())
		{

		case IdlType.e_simple :
			simple = (IdlSimple) obj;

			switch (simple.internal())
			{

			case Token.t_float :
				if (fromObject)
					return "new Float(((Double)" + inname + ").floatValue())";
				else
					return "new Float((float)" + inname + ")";

			case Token.t_double :
				if (fromObject)
					return inname;
				else
					return "new Double(" + inname + ")";

			case Token.t_short :
			case Token.t_ushort :
				if (fromObject)
					return "new Short(((Long)" + inname + ").shortValue())";
				else		
					return "new Short((short)" + inname + ")";

			case Token.t_long :
			case Token.t_ulong :
				if (fromObject)
					return "new Integer(((Long)" + inname + ").intValue())";
				else
					return "new Integer((int)" + inname + ")";

			case Token.t_longlong :
			case Token.t_ulonglong :
				if (fromObject)
					return inname;
				else
					return "new Long(" + inname + ")";

			case Token.t_char :
			case Token.t_wchar :
				return inname;
				//return "new Character(" + inname + ")";

			case Token.t_boolean :
				return inname;
				//return "new Boolean(" + inname + ")";

			case Token.t_octet :
				if (fromObject)
					return "new Byte(((Long)" + inname + ").byteValue())";
				else
					return "new Byte((byte)" + inname + ")";
			
			case Token.t_any :
			case Token.t_typecode :
			case Token.t_object :
			case Token.t_ValueBase :
				return inname;
			}

			break;

		case IdlType.e_interface :
		case IdlType.e_forward_interface :

		case IdlType.e_fixed :

		case IdlType.e_string :
		case IdlType.e_wstring :

		case IdlType.e_struct :
		case IdlType.e_union :
		case IdlType.e_enum :
		
		case IdlType.e_native :
		case IdlType.e_value_box :
		case IdlType.e_value :
		case IdlType.e_forward_value :

			return inname;

		case IdlType.e_array :
		case IdlType.e_sequence :

			// TODO not nice solution
			String primitiveString = translate_type(obj.current());
			if (primitiveString.equals("float"))
				primitiveString = "double";
			else if (primitiveString.equals("double"));
			else if (primitiveString.equals("long"));
			else // all rest to long
				primitiveString = "long";
			return "abeans.models.acs.baci.util.SequenceConverterHelper." + translate_type(obj.current()) + "Sequence((" + primitiveString + "[])" +inname + ")";
		
		case IdlType.e_typedef :
			return cast_to_object_type(((IdlTypeDef) obj).current(), inname, fromObject);

		case IdlType.e_ident :
			return cast_to_object_type(((IdlIdent) obj).internalObject(), inname, fromObject);
		
		}
		
		return "null";
	}

	/**
	 * Map <code>from</code> to java.lang.Object.
	 * Examples: 
	 * 		inname;
	 * 		inname.doubleValue();
	 *
	 *  
	 * @param obj the member to decode
	 * @param inname parameter
	 * @return expression
	 */
	private static String translate_from_object (IdlObject obj, String inname)
	{
	    IdlSimple simple = null;
	
		obj = final_type(obj);
	
	    switch (obj.kind())
	    {
	
	    case IdlType.e_simple :
	        simple = (IdlSimple) obj;
	
	        switch (simple.internal())
	        {
			case Token.t_float :
				return "((Float)" + inname + ").floatValue()";
	
			case Token.t_double :
				return "((Double)" + inname + ").doubleValue()";
	
			case Token.t_short :
			case Token.t_ushort :
				return "((Short)" + inname + ").shortValue()";
	
			case Token.t_long :
			case Token.t_ulong :
				return "((Integer)" + inname + ").intValue()";
	
			case Token.t_longlong :
			case Token.t_ulonglong :
				return "((Long)" + inname + ").longValue()";
	
			case Token.t_char :
			case Token.t_wchar :
				return "((Character)" + inname + ").charValue()";
	
			case Token.t_boolean :
				return "((Boolean)" + inname + ").booleanValue()";
	
			case Token.t_octet :
				return "((Byte)" + inname + ").byteValue()";
	
			case Token.t_any :
			case Token.t_typecode :
			case Token.t_object :
			case Token.t_ValueBase :
				return "((" + translate_type(obj) + ")" + inname + ")";
			}
	
			break;
	
		case IdlType.e_interface :
		case IdlType.e_forward_interface :
	
		case IdlType.e_fixed :
	
		case IdlType.e_string :
		case IdlType.e_wstring :
	
		case IdlType.e_array :
		case IdlType.e_sequence :
	
		case IdlType.e_struct :
		case IdlType.e_union :
		case IdlType.e_enum :
		
		case IdlType.e_native :
		case IdlType.e_value_box :
		case IdlType.e_value :
		case IdlType.e_forward_value :
	
			return "((" + translate_type(obj) + ")" + inname + ")";
	
		case IdlType.e_typedef :
			return translate_from_object(((IdlTypeDef) obj).current(), inname);
	
		case IdlType.e_ident :
			return translate_from_object(((IdlIdent) obj).internalObject(), inname);
		
		}
		
		return "null /* dont now how to handle */";
	}

    /**
     * Return the typed member for an union member
     */
    private static IdlObject getAsNext(IdlObject obj)
    {
        int p = obj.pos();

        while (obj.end() != true)
        {
            IdlUnionMember member = (IdlUnionMember) obj.current();

            if (member.isAsNext() == false)
            {
                obj.pos(p);
                return member;
            }

            obj.next();
        }

        obj.pos(p);
        return null;
    }

    /**
     * Translate a constant
     *
     * @param obj the constant to translate
     * @param writeInto the directory where the constant must be defined
     */
    private void translate_constant(IdlObject obj, File writeInto, PrintWriter print)
    {
        IdlConst const_obj = (IdlConst) obj;

        PrintWriter output = null;

        if ((const_obj.into(IdlType.e_interface) == true) || (const_obj.into(IdlType.e_value)))
        {

            print.println(tab + "/**");
            print.println(tab + " * Constant value");
            print.println(tab + " */");
            print.print(tab + "public static final ");

            boolean fixed = false;

            if (final_kind(obj.current()) == IdlType.e_fixed)
                fixed = true;

            translate_type(obj.current(), print);

            print.print(" " + obj.name() + " = ");

            if (!fixed)
            {
                print.print("(");
                obj.reset();
                translate_type(obj.current(), print);
                print.print(") ");
            }

            print.println("(" + translate_to_java_expression(const_obj.expression(), fixed, obj) + ");");
            print.println();

        }
        else
        {
			// PACKAGE HANDLING
			File writeIntoAbeans = createDirectory(ABEANS_POSTFIX_DOTLESS, writeInto);
            output = newFile(writeIntoAbeans, obj.name());

            addDescriptiveHeader(output, obj);

            output.println("public interface " + obj.name());
            output.println("{");
            output.println(tab + "/**");
            output.println(tab + " * Constant value");
            output.println(tab + " */");
            output.print(tab + " public static final ");

            translate_type(obj.current(), output);

            boolean fixed = false;

            if (final_kind(obj.current()) == IdlType.e_fixed)
                fixed = true;

            output.print(" value = ");

            if (!fixed)
            {
                output.print(" (");
                obj.reset();
                translate_type(obj.current(), output);
                output.print(") ");
            }

            output.println("(" + translate_to_java_expression(const_obj.expression(), fixed, const_obj) + ");");

            output.println("}");

            output.close();
        }
    }

	/**
	 * Fixes enum expression
	 * @param expr
	 * @return
	 */
	private static String fixEnumExpression(String expr)
	{
		// fix expr
		// com.cosylab.ACS.EnumType.first@ -> com.cosylab.ACS.abeans.EnumType.first
		
		if (expr.length() < 2 || expr.charAt(expr.length()-2) != '@')
			return expr;
		
		int pos = expr.lastIndexOf('.');
		pos = expr.lastIndexOf('.', pos-1);
		if (pos == -1)
			expr = ABEANS_POSTFIX_DOTLESS + "." + expr;
		else
			expr = expr.substring(0, pos+1) + ABEANS_POSTFIX_DOTLESS + expr.substring(pos);

		return expr;
		
	}
	
	/**
	 * Generates code which converts expr from CORBA to Abeans object.
	 * @param obj
	 * @param expr
	 * @param output
	 * @param attr
	 * @param objectMapping	if true, transformed value will be an java.lang.Object
	 */
	private static void fromCORBA(IdlObject obj, String expr, PrintWriter output, int attr, boolean toObjectMapping)
	{
		IdlSimple simple = null;

		switch (obj.kind())
		{

		case IdlType.e_simple :
			simple = (IdlSimple) obj;

			switch (simple.internal())
			{

			case Token.t_float :
			case Token.t_double :
			case Token.t_short :
			case Token.t_ushort :
			case Token.t_long :
			case Token.t_ulong :
			case Token.t_longlong :
			case Token.t_ulonglong :
			case Token.t_char :
			case Token.t_wchar :
			case Token.t_boolean :
			case Token.t_octet :

				if (attr == 0)
				{
					if (!toObjectMapping)
					{
						// no mapping
						output.print(expr);
					}
					else
					{
						// wrap it to java.lang.Object
						output.print(translate_to_object(obj, expr));
					}
				}
				else
				{
					// extract value from CORBA holder and put it to Abeans holder
					output.print("new ");
					translate_parameter(obj, output, attr);
					output.print("(" + expr + ".value)");
				}
				break;


			case Token.t_any :
			case Token.t_typecode :
			case Token.t_object :
			
			// TODO check t_ValueBase
			case Token.t_ValueBase :

				if (attr == 0)
				{
					output.print(expr);
				}
				else
				{
					output.print("new ");
					translate_parameter(obj, output, attr);
					output.print("(" + expr + ".value)");
				}
				break;

			}

			break;

		case IdlType.e_fixed :
		case IdlType.e_string :
		case IdlType.e_wstring :
			if (attr == 0)
			{
				output.print(expr);
			}
			else
			{
				output.print("new ");
				translate_parameter(obj, output, attr);
				output.print("(" + expr + ".value)");
			}

			break;

		case IdlType.e_struct :
		case IdlType.e_union :
		case IdlType.e_enum :

		case IdlType.e_interface :
		case IdlType.e_forward_interface :

			// here we use helpers
			if (attr == 0)
			{
				output.print(fullname(obj) + "Helper.fromCORBA(" + expr + ")");
			}
			else
			{
				output.print("new ");
				translate_parameter(obj, output, attr);
				output.print("(" + fullname(obj) + "Helper.fromCORBA(" + expr + ".value))");
			}

			break;

		case IdlType.e_sequence :
		case IdlType.e_array :
		
			if (attr == 0)
			{
				fromCORBA(obj.current(), expr, output, 0, false);
			}
			else
			{
				output.print("new ");
				translate_parameter(obj.upper(), output, attr);
				output.print("(" + fullname(obj.upper()) + "Helper.fromCORBA(" + expr + ".value))");
			}

			break;

		case IdlType.e_typedef :

			obj.reset();
			if (attr != 0)
			{
				if ((final_type(obj).kind() == IdlType.e_sequence) ||
					(final_type(obj).kind() == IdlType.e_array))
					{
						output.print("new ");
						translate_parameter(obj, output, attr);
						output.print("(" + fullname(obj) + "Helper.fromCORBA(" + expr + ".value))");
					}
				else
					fromCORBA(obj.current(), expr, output, attr, toObjectMapping);
			}
			else
				fromCORBA(obj.current(), expr, output, 0, toObjectMapping);

			break;

		case IdlType.e_ident :

			fromCORBA(((IdlIdent) obj).internalObject(), expr, output, attr, toObjectMapping);
			break;


		//
		// TODO what to do here
		//
		case IdlType.e_value_box :
System.out.println("fromCORBA: " + obj);
			if (attr == 0)
			{
				if (((IdlValueBox) obj).simple())
					output.print(fullname(obj));
				else
				{
					obj.reset();
					translate_parameter(obj.current() , output, attr);
				}
			}
			else
				output.print(fullname(obj) + "Holder");

			break;

			
		case IdlType.e_value :
		case IdlType.e_forward_value :
System.out.println("fromCORBA: " + obj);

			output.print(expr + " /*dont know how to handle*/");
			break;

		case IdlType.e_native :
System.out.println("fromCORBA: " + obj);

			if (attr != 0)
			{
				output.print(fullname(obj) + "Holder");
			}
			else
			{
				if (isNativeDefinition(obj))
					printNativeDefinition(obj, output);
				else
					output.print(fullname(obj));
			}

			break;
		}


	}

	/**
	 * Generates code which converts expr from Abeans to CORBA object.
	 * @param obj
	 * @param expr
	 * @param output
	 * @param attr
	 * @param fromObjectMapping
	 */
	private static void toCORBA(IdlObject obj, String expr, PrintWriter output, int attr, boolean fromObjectMapping)
	{
		toCORBA(obj, expr, output, attr, fromObjectMapping, false);
	}
	
	/**
	 * Generates code which converts expr from Abeans to CORBA object.
	 * @param obj
	 * @param expr
	 * @param output
	 * @param attr
	 * @param fromObjectMapping
	 * @param parentArraySeq
	 */
	private static void toCORBA(IdlObject obj, String expr, PrintWriter output, int attr, boolean fromObjectMapping, boolean parentArraySeq)
	{
		// NOTE: do not map values for out (attr == 1) typw

		// explicit cast or extract for primitives 
		if (fromObjectMapping && attr != 1)
		{
			if (attr == 0)
				expr = translate_from_object(obj, expr);
			else
			{
				expr = "((" + translate_parameter(obj, attr) + ")" + expr + ")";
			}
		}

		switch (obj.kind())
		{

		case IdlType.e_simple :
			IdlSimple simple = (IdlSimple) obj;

			switch (simple.internal())
			{

			case Token.t_float :
			case Token.t_double :
			case Token.t_short :
			case Token.t_ushort :
			case Token.t_long :
			case Token.t_ulong :
			case Token.t_longlong :
			case Token.t_ulonglong :
			case Token.t_char :
			case Token.t_wchar :
			case Token.t_boolean :
			case Token.t_octet :

				if (attr == 0)
				{
					// no mapping
					output.print(expr);
				}
				else
				{
					// extract value from Abeans holder and put it to CORBA holder
					output.print("new ");
					translate_parameter_CORBA(obj, output, attr);
					if (attr == 1)
						output.print("()");
					else
						output.print("(" + expr + ".value)");
				}

				break;
				
			case Token.t_any :
			case Token.t_typecode :
			case Token.t_object :
			
			// TODO check t_ValueBase
			case Token.t_ValueBase :

				if (attr == 0)
				{
					// firstly do mapping to appropriate type
					output.print("((");
					translate_type_CORBA(obj, output);
					if (parentArraySeq)
						output.print("[]");
					output.print(")(" + expr + "))");
				}
				else
				{

					// extract value from Abeans holder and put it to CORBA holder
					output.print("new ");
					translate_parameter_CORBA(obj, output, attr);

					if (attr == 1)
						output.print("()");
					else
					{
						// firstly do mapping to appropriate type
						String tmp = "(" + translate_type_CORBA(obj);
						if (parentArraySeq)
							tmp += "[]";
						expr = tmp + ")" + expr + ".value";
						
						output.print("(" + expr + ")");
					}
				}

				break;

			}

			break;

			
			case IdlType.e_fixed :
			case IdlType.e_string :
			case IdlType.e_wstring :

			if (attr == 0)
			{
				output.print(expr);
			}
			else
			{
				output.print("new ");
				translate_parameter_CORBA(obj, output, attr);
				if (attr == 1)
					output.print("()");
				else
					output.print("(" + expr + ".value)");
			}

			break;



		case IdlType.e_struct :
		case IdlType.e_union :
		case IdlType.e_enum :

		case IdlType.e_interface :
		case IdlType.e_forward_interface :

			// use helpers
			if (attr == 0)
			{
				output.print(fullname(obj) + "Helper.toCORBA(" + expr + ")");
			}
			else
			{
				output.print("new ");
				translate_parameter_CORBA(obj, output, attr);
				if (attr == 1)
					output.print("()");
				else
					output.print("(" + fullname(obj) + "Helper.toCORBA(" + expr + ".value))");
			}

			break;

		case IdlType.e_sequence :
		case IdlType.e_array :

			if (attr == 0)
			{
				toCORBA(obj.current(), expr, output, 0, false, true);
			}
			else
			{
				output.print("new ");
				translate_parameter_CORBA(obj.upper(), output, attr);
				if (attr == 1)
					output.print("()");
				else
					output.print("(" + fullname(obj.upper()) + "Helper.toCORBA(" + expr + ".value))");
			}

			break;

		case IdlType.e_typedef :
	
			obj.reset();
			if (attr != 0)
			{
				IdlObject finalType = final_type(obj);
				if ((finalType.kind() == IdlType.e_sequence) ||
					(finalType.kind() == IdlType.e_array))
					{
						output.print("new ");
						translate_parameter_CORBA(obj, output, attr);
						//output.print("(" + fullname(obj) + "Helper.toCORBA(" + expr + ".value))");
						
						if (attr == 1)
							output.print("()");
						else
						{
						
							// down code is used so have base object helper
							int fk = final_kind(finalType.current());
							
							// array of array...
							while (fk == IdlType.e_array || fk == IdlType.e_sequence)
							{ 
								finalType = final_type(finalType.current()); 
								fk = final_kind(finalType.current());
							}
								
							if (fk == IdlType.e_simple)
								{
									// simples do not need helpers
									simple = (IdlSimple)final_type(finalType.current());
									switch (simple.internal())
									{
										case Token.t_any :
										case Token.t_typecode :
										case Token.t_object :
				
										// TODO nested arrays/sequences not supported
										// TODO check t_ValueBase
										case Token.t_ValueBase :
											output.print("((");
											translate_type_CORBA(finalType.current(), output);
											output.print("[])(" + expr + ".value))"); 
											break;
										default:
											output.print("(" + expr + ".value)"); 
									}
								}
							else if ((fk == IdlType.e_fixed) ||	(fk == IdlType.e_string) || (fk == IdlType.e_wstring))
								output.print("(" + expr + ".value)"); 
							else
								// TODO possible bug for value-box, native, etc. - obj w/o fullname and for nester arrays/sequences
								output.print("(" + fullname(final_type(finalType.current())) + "Helper.toCORBA(" + expr + ".value))");
						}
					}
				else
					toCORBA(obj.current(), expr, output, attr, false);
			}
			else
				toCORBA(obj.current(), expr, output, 0, false, parentArraySeq);

			break;


		case IdlType.e_ident :
			toCORBA(((IdlIdent) obj).internalObject(), expr, output, attr, false, parentArraySeq);
			break;
			

		//
		// TODO what to do here
		//
		
		case IdlType.e_value_box :
System.out.println("toCORBA: " + obj);

			if (attr == 0)
			{
				if (((IdlValueBox) obj).simple())
					output.print(fullname(obj));
				else
				{
					obj.reset();
					translate_parameter_CORBA(obj.current() , output, attr);
				}
			}
			else
				output.print(fullname(obj) + "Holder");

			break;


		case IdlType.e_value :
		case IdlType.e_forward_value :
System.out.println("toCORBA: " + obj);

				output.print(expr + " /*dont know how to handle*/");

			break;

		case IdlType.e_native :
System.out.println("toCORBA: " + obj);

			if (attr != 0)
			{
				output.print(fullname(obj) + "Holder");
			}
			else
			{
				if (isNativeDefinition(obj))
					printNativeDefinition(obj, output);
				else
					output.print(fullname(obj));
			}

			break;

		}
	}

	/**
	 * Generates array support helper methods
	 *
	 * @param obj the object to translate
	 * @param output output stream
	 */
	private void generateArrayHelperMethods(IdlObject obj, PrintWriter output)
	{
		output.println();
		
		output.println(tab  + "/** ");
		output.println(tab  + " * Converts Abeans R3 BACI modeling object to CORBA object.");
		output.println(tab  + " */ ");
		output.println(tab  + "public static " + obj.name() + "[] fromCORBA(" + fullname_CORBA(obj) + "[] corbaObject)");
		output.println(tab  + "{");
		output.println(tab2 + obj.name() + "[] retVal = new " + obj.name() + "[corbaObject.length];");
		output.println(tab2 + "for (int i = 0; i < corbaObject.length; i++)");
		output.println(tab3 + "retVal[i] = fromCORBA(corbaObject[i]);");
		output.println(tab2 + "return retVal;");
		output.println(tab  + "}");
		output.println();
	
		output.println(tab  + "/** ");
		output.println(tab  + " * Converts CORBA object to Abeans R3 BACI modeling object.");
		output.println(tab  + " */ ");
		output.println(tab  + "public static " + fullname_CORBA(obj) + "[] toCORBA(" + obj.name() + "[] abeansObject)");
		output.println(tab  + "{");
		output.println(tab2 + fullname_CORBA(obj) + "[] retVal = new " + fullname_CORBA(obj) + "[abeansObject.length];");
		output.println(tab2 + "for (int i = 0; i < abeansObject.length; i++)");
		output.println(tab3 + "retVal[i] = toCORBA(abeansObject[i]);");
		output.println(tab2 + "return retVal;");
		output.println(tab  + "}");

		//
		// 2-dimensional array helpers
		//
		output.println(tab  + "/** ");
		output.println(tab  + " * Converts Abeans R3 BACI modeling object to CORBA object.");
		output.println(tab  + " */ ");
		output.println(tab  + "public static " + obj.name() + "[][] fromCORBA(" + fullname_CORBA(obj) + "[][] corbaObject)");
		output.println(tab  + "{");
		output.println(tab2 + obj.name() + "[][] retVal = new " + obj.name() + "[corbaObject.length][corbaObject[0]==null?0:corbaObject[0].length];");
		output.println(tab2 + "for (int i = 0; i < corbaObject.length; i++)");
		output.println(tab3 + "retVal[i] = fromCORBA(corbaObject[i]);");
		output.println(tab2 + "return retVal;");
		output.println(tab  + "}");
		output.println();
	
		output.println(tab  + "/** ");
		output.println(tab  + " * Converts CORBA object to Abeans R3 BACI modeling object.");
		output.println(tab  + " */ ");
		output.println(tab  + "public static " + fullname_CORBA(obj) + "[][] toCORBA(" + obj.name() + "[][] abeansObject)");
		output.println(tab  + "{");
		output.println(tab2 + fullname_CORBA(obj) + "[][] retVal = new " + fullname_CORBA(obj) + "[abeansObject.length][abeansObject[0]==null?0:abeansObject[0].length];");
		output.println(tab2 + "for (int i = 0; i < abeansObject.length; i++)");
		output.println(tab3 + "retVal[i] = toCORBA(abeansObject[i]);");
		output.println(tab2 + "return retVal;");
		output.println(tab  + "}");

	}
	
    /**
     * Add a Helper for a data type
     *
     * @param obj the object to translate
     * @param writeInto the directory where the object must be defined
     */
    private void write_helper(IdlObject obj, File writeInto)
    {
    	// create helper only for interfaces, enums, structs and unions
    	int kind = final_kind(obj); 
    	if (kind != IdlType.e_interface && kind != IdlType.e_enum &&
    		kind != IdlType.e_struct && kind != IdlType.e_union)
    		return;
    	
		File writeIntoAbeans = createDirectory(ABEANS_POSTFIX_DOTLESS, writeInto);
        PrintWriter output = newFile(writeIntoAbeans, obj.name() + "Helper");

        if (current_pkg != null)
        {
            if (current_pkg.equals(GENERATED))
            {
                if (IdlCompiler.use_package == true)
                {
                    output.println("package " + current_pkg + ";");
                    output.println();
                }
            }
            else
                if (!current_pkg.equals(EMPTY_STRING))
                {
                    output.println("package " + current_pkg + ";");
                    output.println();
                }
        }

        output.println("/**");
        output.println(" * Helper class for : " + obj.name());
        output.println(" * ");
        output.println(" * @author " + JAVADOC_AUTHOR);
        output.println(" */");

        output.println("public final class " + obj.name() + "Helper");

        output.println("{");

        switch (kind)
        {

		case IdlType.e_interface :
		{
			output.println(tab  + "/** ");
			output.println(tab  + " * Converts Abeans R3 BACI modeling object to CORBA object.");
			output.println(tab  + " */ ");
			output.println(tab  + "public static " + obj.name() + " fromCORBA(" + fullname_CORBA(obj) + " corbaObject)");
			output.println(tab  + "{");
			String fullProxyName = fullname(obj, ABEANS_POSTFIX_DOTLESS + PROXY_POSTFIX) + "Proxy";
			output.println(tab2 + fullProxyName + " proxy = new " + fullProxyName + "(corbaObject);");
			output.println(tab2 + obj.name() + " aif = new " + obj.name() + "();");
			output.println(tab2 + "aif.setProxy(proxy);");
			output.println(tab2 + "return aif;");
			output.println(tab  + "}");
			output.println();

			output.println(tab  + "/** ");
			output.println(tab  + " * Converts CORBA object to Abeans R3 BACI modeling object.");
			output.println(tab  + " */ ");
			output.println(tab  + "public static " + fullname_CORBA(obj) + " toCORBA(" + obj.name() + " abeansObject)");
			output.println(tab  + "{");
			output.println(tab2 + "return (" + fullname_CORBA(obj) + ")((abeans.pluggable.acs.maci.NarrowCORBAProxy)(abeansObject.getProxy())).getDelegate();");
			output.println(tab  + "}");
		
			generateArrayHelperMethods(obj, output);

			break;
		}

		case IdlType.e_enum :
		{
			output.println(tab  + "/** ");
			output.println(tab  + " * Converts Abeans R3 BACI modeling object to CORBA object.");
			output.println(tab  + " */ ");
			output.println(tab  + "public static " + obj.name() + " fromCORBA(" + fullname_CORBA(obj) + " corbaObject)");
			output.println(tab  + "{");
			output.println(tab2 + "return " + obj.name() + ".from_int(corbaObject.value());");
			output.println(tab  + "}");
			output.println();

			output.println(tab  + "/** ");
			output.println(tab  + " * Converts CORBA object to Abeans R3 BACI modeling object.");
			output.println(tab  + " */ ");
			output.println(tab  + "public static " + fullname_CORBA(obj) + " toCORBA(" + obj.name() + " abeansObject)");
			output.println(tab  + "{");
			output.println(tab2 + "return " + fullname_CORBA(obj) + ".from_int(abeansObject.value());");
			output.println(tab  + "}");
			
			generateArrayHelperMethods(obj, output);

			break;
		}

        case IdlType.e_struct :
		{
			output.println(tab  + "/** ");
			output.println(tab  + " * Converts CORBA object to Abeans R3 BACI modeling object.");
			output.println(tab  + " */ ");
			output.println(tab  + "public static " + obj.name() + " fromCORBA(" + fullname_CORBA(obj) + " corbaObject)");
			output.println(tab  + "{");
			
			output.println(tab2 + "return new " + obj.name() + "(");
			obj.reset();
			while (obj.end() != true)
			{
				IdlStructMember member_obj = (IdlStructMember) obj.current();
				output.print(tab5);
				fromCORBA(member_obj.current(), "corbaObject." + member_obj.name(), output, 0, false);
				obj.next();

				if (obj.end() != true)
					output.println(", ");
			}
			output.println(");");
			
			output.println(tab  + "}");
			output.println();

			output.println(tab  + "/** ");
			output.println(tab  + " * Converts CORBA object to Abeans R3 BACI modeling object.");
			output.println(tab  + " */ ");
			output.println(tab  + "public static " + fullname_CORBA(obj) + " toCORBA(" + obj.name() + " abeansObject)");
			output.println(tab  + "{");

			output.println(tab2 + "return new " + fullname_CORBA(obj) + "(");
			obj.reset();
			while (obj.end() != true)
			{
				IdlStructMember member_obj = (IdlStructMember) obj.current();
				output.print(tab5);
				toCORBA(member_obj.current(), "abeansObject." + member_obj.name(), output, 0, false);
				obj.next();

				if (obj.end() != true)
					output.println(", ");
			}
			output.println(");");

			output.println(tab  + "}");
			
			generateArrayHelperMethods(obj, output);

			break;
		}

        case IdlType.e_union :


			{
				
			output.println(tab  + "/** ");
			output.println(tab  + " * Converts Abeans R3 BACI modeling object to CORBA object.");
			output.println(tab  + " */ ");
			output.println(tab  + "public static " + obj.name() + " fromCORBA(" + fullname_CORBA(obj) + " corbaObject)");
			output.println(tab  + "{");
			
			int idx = ((IdlUnion) obj).index();
			obj.reset();
			output.println(tab2 + obj.name() + " new_one = new " + obj.name() + "();");
			output.println();
		
			obj.current().reset();
			boolean enumeration = false;
		
			IdlObject d = obj.current().current();
	
			if (final_kind(d) == IdlType.e_enum)
				enumeration = true;
	
			obj.next();
		
			String discrim = null;
		
			if (((IdlUnionMember) obj.current()).getExpression().equals("true ") ||
					((IdlUnionMember) obj.current()).getExpression().equals("false "))
			{
				discrim = "corbaObject.toInt()";
			}
			else
			{
				if (enumeration)
					discrim = "corbaObject.discriminator().value()";
				else
					discrim = "corbaObject.discriminator()";
			}
		
			int i = 0;
		
			while (obj.end() != true)
			{
				if (i != idx)
				{
					output.print(tab2 + "if (" + discrim + " == ");
					
					String expr = ((IdlUnionMember) obj.current()).getExpression(); 
					if (expr.equals("true "))
						output.println("1)");
					else
						if (expr.equals("false "))
							output.println("0)");
						else
						{
							if (!enumeration)
							{
								output.print("(");
								translate_type(d, output);
								output.print(")");
							}
		
							output.println(translate_to_java_expression(expr, false, ((IdlUnionMember) obj.current())) + ")");
						}
		
					output.println(tab2 + "{");
		
					// isAsNext() case 
					if (((IdlUnionMember) obj.current()).isAsNext() == false)
					{
						obj.current().reset();
						//output.println(tab3 + "new_one." + obj.current().name() + "(corbaObject." + obj.current().name() + "());");
						output.print(tab3 + "new_one." + obj.current().name() + "(");
						fromCORBA(obj.current().current(), "corbaObject." + obj.current().name() + "()", output, 0, false);
						output.println(");");
					}
					else
					{
						IdlObject next = getAsNext(obj);
		
						next.reset();
	
						//output.println(tab3 + "new_one." + obj.current().name() + "(corbaObject.discriminator(), corbaObject." + obj.current().name() + "());");
						output.print(tab3 + "new_one." + obj.current().name() + "(");
						fromCORBA(d, "corbaObject.discriminator()", output, 0, false);
						output.print(", ");
						fromCORBA(obj.current().current(), "corbaObject." + obj.current().name() + "()", output, 0, false);
						output.println(");");
					}
		
					output.println(tab2 + "}");
		
				}
		
				obj.next();
		
				if ((obj.end() != true) && ((i + 1) != idx))
					output.println(tab2 + "else");
		
				i++;
			}
		
			i = 0;
			obj.reset();
			obj.next();
		
			// default value
			while (obj.end() != true)
			{
				if (i == idx)
				{
					if (obj.length() != 2)
						output.println(tab2 + "else");
		
					output.println(tab2 + "{");
		
					obj.current().reset();
		
					// TODO convert	
					//output.println(tab3 + "new_one." + obj.current().name() + "(corbaObject." + obj.current().name() + "());");
					output.print(tab3 + "new_one." + obj.current().name() + "(");
					fromCORBA(obj.current().current(), "corbaObject." + obj.current().name() + "()", output, 0, false);
					output.println(");");
		
					output.println(tab2 + "}");
		
				}
		
				obj.next();
		
				i++;
			}
		
			output.println();
			output.println(tab2 + "return new_one;");
			output.println(tab1 + "}");
			output.println();

			}



			{

			output.println(tab  + "/** ");
			output.println(tab  + " * Converts CORBA object to Abeans R3 BACI modeling object.");
			output.println(tab  + " */ ");
			output.println(tab  + "public static " + fullname_CORBA(obj) + " toCORBA(" + obj.name() + " abeansObject)");
			output.println(tab  + "{");
	
			int idx = ((IdlUnion) obj).index();
			obj.reset();
			output.println(tab2 + fullname_CORBA(obj) + " new_one = new " + fullname_CORBA(obj) + "();");
			output.println();
	
			obj.current().reset();
			boolean enumeration = false;
	
			IdlObject d = obj.current().current();
			
			if (final_kind(d) == IdlType.e_enum)
				enumeration = true;

			obj.next();
	
			String discrim = null;
	
			if (((IdlUnionMember) obj.current()).getExpression().equals("true ") ||
					((IdlUnionMember) obj.current()).getExpression().equals("false "))
			{
				discrim = "abeansObject.toInt()";
			}
			else
			{
				if (enumeration)
					discrim = "abeansObject.discriminator().value()";
				else
					discrim = "abeansObject.discriminator()";
			}

			int i = 0;
	
			while (obj.end() != true)
			{
				if (i != idx)
				{
					output.print(tab2 + "if (" + discrim + " == ");
	
					String expr = ((IdlUnionMember) obj.current()).getExpression();
					if (expr.equals("true "))
						output.println("1)");
					else
						if (expr.equals("false "))
							output.println("0)");
						else
						{
							if (!enumeration)
							{
								output.print("(");
								translate_type(d, output);
								output.print(")");
							}
							else
							{
								expr = fixEnumExpression(expr);
							}

							output.println(translate_to_java_expression(expr, false, ((IdlUnionMember) obj.current())) + ")");
						}
	
					output.println(tab2 + "{");
	
					// isAsNext() case 
					if (((IdlUnionMember) obj.current()).isAsNext() == false)
					{
						obj.current().reset();
						//output.println(tab3 + "new_one." + obj.current().name() + "(abeansObject." + obj.current().name() + "());");
						output.print(tab3 + "new_one." + obj.current().name() + "(");
						toCORBA(obj.current().current(), "abeansObject." + obj.current().name() + "()", output, 0, false);
						output.println(");");
					}
					else
					{
						IdlObject next = getAsNext(obj);
	
						next.reset();

						//output.println(tab3 + "new_one." + obj.current().name() + "(abeansObject.discriminator(), abeansObject." + obj.current().name() + "());");
						output.print(tab3 + "new_one." + obj.current().name() + "(");
						toCORBA(d, "abeansObject.discriminator()", output, 0, false);
						output.print(", ");
						toCORBA(obj.current().current(), "abeansObject." + obj.current().name() + "()", output, 0, false);
						output.println(");");
						
					}
	
					output.println(tab2 + "}");
	
				}
	
				obj.next();
	
				if ((obj.end() != true) && ((i + 1) != idx))
					output.println(tab2 + "else");
	
				i++;
			}
	
			i = 0;
			obj.reset();
			obj.next();
	
			// default value
			while (obj.end() != true)
			{
				if (i == idx)
				{
					if (obj.length() != 2)
						output.println(tab2 + "else");
	
					output.println(tab2 + "{");
	
					obj.current().reset();
	
					//output.println(tab3 + "new_one." + obj.current().name() + "(abeansObject." + obj.current().name() + "());");
					output.print(tab3 + "new_one." + obj.current().name() + "(");
					toCORBA(obj.current().current(), "abeansObject." + obj.current().name() + "()", output, 0, false);
					output.println(");");
	
					output.println(tab2 + "}");
	
				}
	
				obj.next();
	
				i++;
			}
	
			output.println();
			output.println(tab2 + "return new_one;");
			output.println(tab1 + "}");
			output.println();
			
			}
			
			generateArrayHelperMethods(obj, output);

			break;

        };
        
        output.println("}");
        output.close();
    }

    /**
     * Add an holder for a data type
     *
     * @param obj the object to translate
     * @param writeInto the directory where the object must be defined
     */
    private void write_holder(IdlObject obj, File writeInto)
    {
		File writeIntoAbeans = createDirectory(ABEANS_POSTFIX_DOTLESS, writeInto);
        PrintWriter output = newFile(writeIntoAbeans, obj.name() + "Holder");

        if (current_pkg != null)
        {
            if (current_pkg.equals(GENERATED))
            {
                if (IdlCompiler.use_package == true)
                {
                    output.println("package " + current_pkg + ";");
                    output.println();
                }
            }
            else
                if (!current_pkg.equals(EMPTY_STRING))
                {
                    output.println("package " + current_pkg + ";");
                    output.println();
                }
        }

        output.println("/**");
        output.println(" * Holder class for : " + obj.name());
        output.println(" * ");
        output.println(" * @author " + JAVADOC_AUTHOR);
        output.println(" */");

        output.println("final public class " + obj.name() + "Holder");
        output.println("{");

        // The internal value
        output.println(tab + "/**");
        output.println(tab + " * Internal " + obj.name() + " value");
        output.println(tab + " */");

        output.print(tab + "public ");

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

        output.println();

        // Default constructor
        output.println(tab + "/**");

        output.println(tab + " * Default constructor");

        output.println(tab + " */");

        output.println(tab + "public " + obj.name() + "Holder() {}");

        output.println();

        // Constructor with init
        output.println(tab + "/**");

        output.println(tab + " * Constructor with value initialisation");

        output.println(tab + " * @param initial the initial value");

        output.println(tab + " */");

        output.print(tab + "public " + obj.name() + "Holder(");

        translate_type(obj, output);

        output.println(" initial)");

        output.println(tab + "{");

        if (obj.kind() == IdlType.e_value_box)
        {
            if (((IdlValueBox) obj).simple())
                output.println(tab2 + "value = initial.value;");
            else
                output.println(tab2 + "value = initial;");
        }
        else
            output.println(tab2 + "value = initial;");

        output.println(tab + "}");

        output.println();

        output.println("}");

        output.close();
    }

    /**
     * Translate an enumeration
     *
     * @param obj the enum to be translated
     * @param writeInto the directory where the enum must be defined
     */
    private void translate_enum(IdlObject obj, File writeInto)
    {
		File writeIntoAbeans = createDirectory(ABEANS_POSTFIX_DOTLESS, writeInto);
        PrintWriter output = newFile(writeIntoAbeans, obj.name());
        IdlEnumMember member_obj;

        addDescriptiveHeader(output, obj);


        // Define the class
        output.println("public final class " + obj.name());
        output.println("{");

		// count members
		int memebers = 0;

		obj.reset();
		while ( !obj.end() )
		{
			memebers++;
			obj.next();
		}

		// Array of members
		// this array has to be created first
		output.println( tab + "/**" );
		output.println( tab + " * Array of memebers " );
		output.println( tab + " */" );
		output.println( tab + "private static final " + obj.name() + "[] values = new " + obj.name() + "[" + memebers + "];" );
		output.println();

        // Value of each member
        obj.reset();

        while ( !obj.end() )
        {
            member_obj = (IdlEnumMember) obj.current();

			output.println(tab + "/**");
            output.println(tab + " * Enum member " + member_obj.name() + " value ");
            output.println(tab + " */");
            output.print(tab + "private static final int _" + member_obj.name());
            output.println(" = " + member_obj.getValue() + ";");
            output.println();

            output.println(tab + "/**");
            output.println(tab + " * Enum member " + member_obj.name());
            output.println(tab + " */");
            output.print(tab + "public static final " + obj.name() + " " + member_obj.name());
            output.println(" = new " + obj.name() + "(_" + member_obj.name() + ");");
            output.println();

            obj.next();
        }

		// The internal member
		output.println( tab + "/**" );
		output.println( tab + " * Internal member value " );
		output.println( tab + " */" );
		output.println( tab + "private final int value;" );
		output.println();

        // The constructor
        output.println( tab1 + "/**" );
        output.println( tab1 + " * Private constructor" );
        output.println( tab1 + " * @param  the enum value for this new member" );
        output.println( tab1 + " */" );
        output.println( tab1 + "private " + obj.name() + "( final int value )" );
        output.println( tab1 + "{" );
		output.println( tab2 + "// dynamically fill array of values" );
		output.println( tab2 + "values[value] = this;" );
		output.println();
		output.println( tab2 + "this.value = value;" );
        output.println( tab1 + "}" );
        output.println();

        // the method value
        output.println( tab1 + "/**" );
        output.println( tab1 + " * Return the internal member value" );
        output.println( tab1 + " * @return the member value" );
        output.println( tab1 + " */" );
        output.println( tab1 + "public int value()");
        output.println( tab1 + "{" );
        output.println( tab2 + "return value;" );
        output.println( tab1 + "}" );
        output.println();

		// the method values
		output.println( tab1 + "/**" );
		output.println( tab1 + " * Return array of all values" );
		output.println( tab1 + " * @return the array of all values" );
		output.println( tab1 + " */" );
		output.println( tab1 + "public " + obj.name() + "[] values()");
		output.println( tab1 + "{" );
		output.println( tab2 + "return values;" );
		output.println( tab1 + "}" );
		output.println();

        // the method from_int
        output.println( tab1 + "/**" );
        output.println( tab1 + " * Return a enum member from its value" );
        output.println( tab1 + " * @param  an enum value" );
        output.println( tab1 + " * @return an enum member" );
        output.println( tab2 + " */" );

        output.println( tab1 + "public static " + obj.name() + " from_int(int value)" );
        output.println( tab1 + "{" );
        output.println( tab2 + "return values[value];" );
        output.println( tab1 + "}" );
		output.println();

        // the method toString()
        output.println(tab + "/**");
        output.println(tab + " * Return a string representation");
        output.println(tab + " * @return a string representation of the enumeration");
        output.println(tab + " */");
        output.println(tab + "public java.lang.String toString()");
        output.println(tab + "{");
        output.println(tab2 + "switch (value)");
        output.println(tab2 + "{");

        obj.reset();

        while (obj.end() != true)
        {
            member_obj = (IdlEnumMember) obj.current();

            output.println(tab2 + "case " + member_obj.getValue() + " :");
            output.println(tab3 + "return \"" + member_obj.name() + "\";");

            obj.next();
        }

        output.println(tab2 + "}");
        output.println(tab2 + "return null;");
        output.println(tab + "}");
        output.println();

        output.println("}");

        output.close();

        write_helper(obj, writeInto);
        write_holder(obj, writeInto);
    }

    /**
     * Translate the structure
     *
     * @param obj le module to be translated
     * @param writeInto the directory where the structure must be defined
     */
    private void translate_struct(IdlObject obj, File writeInto)
    {
		File sub = writeInto;
		File writeIntoAbeans = createDirectory(ABEANS_POSTFIX_DOTLESS, writeInto);
        PrintWriter output = newFile(writeIntoAbeans, obj.name());
        String old_pkg = current_pkg;

        IdlStructMember member_obj;

        addDescriptiveHeader(output, obj);

        if (isEmpty(obj) == false)
        {
            sub = createDirectory(obj.name() + "Package" , writeInto);
        }

        addToPkg(obj, obj.name() + "Package");

        // Define the sub-types
        obj.reset();

        while (obj.end() != true)
        {
            obj.current().reset();

            switch (obj.current().current().kind())
            {

            case IdlType.e_union :
                translate_union(obj.current().current(), sub);
                break;

            case IdlType.e_struct :
                translate_struct(obj.current().current(), sub);
                break;

            case IdlType.e_enum :
                translate_enum(obj.current().current(), sub);
                break;
            }

            obj.next();
        }

        current_pkg = old_pkg;

        // Define the class
        output.println("public final class " + obj.name());
        output.println("{");

        // Declare each member of the structure
        obj.reset();

        while (obj.end() != true)
        {
            member_obj = (IdlStructMember) obj.current();

            output.println(tab + "/**");
            output.println(tab + " * Struct member " + member_obj.name());
            output.println(tab + " */");
            output.print(tab + "public ");

            member_obj.reset();
            translate_type(member_obj.current(), output);

            output.println(" " + member_obj.name() + ";");
            output.println();

            obj.next();
        }

        // Default constructor
        output.println(tab + "/**");

        output.println(tab + " * Default constructor");

        output.println(tab + " */");

        output.println(tab + "public " + obj.name() + "()");

        output.println(tab + "{ }");

        output.println();


        output.println(tab + "/**");

        output.println(tab + " * Constructor with fields initialization");

        obj.reset();

        while (obj.end() != true)
        {
            member_obj = (IdlStructMember) obj.current();
            output.println(tab + " * @param " + member_obj.name() + " " + member_obj.name() + " struct member");
            obj.next();
        }

        output.println(tab + " */");
        output.print(tab + "public " + obj.name() + "(");

        obj.reset();

        while (obj.end() != true)
        {
            member_obj = (IdlStructMember) obj.current();

            translate_type(member_obj.current(), output);
            output.print(" " + member_obj.name());
            obj.next();

            if (obj.end() != true)
                output.print(", ");
        }

        output.println(")");
        output.println(tab + "{");

        obj.reset();

        while (obj.end() != true)
        {
            member_obj = (IdlStructMember) obj.current();

            output.println(tab2 + "this." + member_obj.name() + " = " + member_obj.name() + ";");

            obj.next();
        }

        output.println(tab + "}");
        output.println();

        output.println("}");

        output.close();

        write_helper(obj, writeInto);
        write_holder(obj, writeInto);
    }

    /**
     * Search the default value for an union
     *
     * @param obj the union
     * @return default value
     */
    private static String find_default_value(IdlObject obj)
    {
        IdlUnionMember disc;
        IdlUnionMember member_obj;
        IdlObject en;
        int idx = ((IdlUnion) obj).index();
        int i;
        int l;
        int p = obj.pos();
        String s;
        String ts;

        obj.reset();
        disc = (IdlUnionMember) obj.current();

        disc.reset();

        switch (final_kind(disc.current()))
        {

        case IdlType.e_enum :
            en = final_type(disc.current());
            obj.next();
            l = 0;
            i = 0;
            en.reset();
            s = fullname(en.current());
            ts = s + "@ ";

            while (obj.end() != true)
            {
                if (i != idx)
                {
                    member_obj = (IdlUnionMember) obj.current();

                    if (ts.equals(member_obj.getExpression()))
                    {
                        l++;
                        i = -1;
                        en.next();
                        s = fullname(en.current());
                        ts = s + "@ ";
                        obj.reset();
                    }
                }

                i++;
                obj.next();
            }

            obj.pos(p);
            
            return s;

        default :
            l = 0;
            i = 0;
            obj.next();

            while (obj.end() != true)
            {
                if (i != idx)
                {
                    member_obj = (IdlUnionMember) obj.current();

                    if (l == member_obj.getValue())
                    {
                        l++;
                        i = -1;
                        obj.reset();
                    }
                }

                i++;
                obj.next();
            }

            if (final_kind(disc.current()) == IdlType.e_simple)
            {
            	IdlSimple simple = (IdlSimple)disc.current();
            	if (simple.internal() == Token.t_boolean)
            		return Boolean.toString(l!=0);
            }
            
            break;
        }

        obj.pos(p);
        
        return EMPTY_STRING + l;
    }

    /**
     * Translate an union
     *
     * @param obj the union to translate
     * @param writeInto the directory where the union must be defined
     */
    private void translate_union(IdlObject obj, File writeInto)
    {
		File sub = writeInto;
		File writeIntoAbeans = createDirectory(ABEANS_POSTFIX_DOTLESS, writeInto);
        PrintWriter output = newFile(writeIntoAbeans, obj.name());
        String old_pkg = current_pkg;

        IdlUnionMember member_obj;
        IdlObject disc;
        int i;
        int idx = ((IdlUnion) obj).index();
        boolean doMap = false;

        addDescriptiveHeader(output, obj);

        if (isEmpty(obj) == false)
            sub = createDirectory(obj.name() + "Package", writeInto);

        addToPkg(obj, obj.name() + "Package");

        // Define the sub-types
        obj.reset();

        while (obj.end() != true)
        {
            obj.current().reset();

            switch (obj.current().current().kind())
            {

            case IdlType.e_union :
                translate_union(obj.current().current(), sub);
                break;

            case IdlType.e_struct :
                translate_struct(obj.current().current(), sub);
                break;

            case IdlType.e_enum :
                translate_enum(obj.current().current(), sub);
                break;
            }

            obj.next();
        }

        current_pkg = old_pkg;

        // Define the class
        output.println("public final class " + obj.name());
        output.println("{");

        // Declare union members
        obj.reset();

        while (obj.end() != true)
        {
            member_obj = (IdlUnionMember) obj.current();

            if (member_obj.isAsNext() == false)
            {
                output.println(tab + "/**");
                output.println(tab + " * Union member " + member_obj.name());
                output.println(tab + " */");
                output.print(tab + "protected ");

                member_obj.reset();
                translate_type(member_obj.current(), output);

                output.println(" _" + member_obj.name() + ";");
                output.println();

            }

            obj.next();
        }

        // Default constructor
        output.println(tab + "/**");

        output.println(tab + " * Default constructor");

        output.println(tab + " */");

        output.println(tab + "public " + obj.name() + "()");

        output.println(tab + "{");

        if (idx != -1)
        {
            output.println(tab2 + "__d = " + find_default_value(obj) + ";");
        }

        output.println(tab + "}");
        output.println();

        obj.reset();

        output.println(tab + "/**");
        output.println(tab + " * Get discriminator value");
        output.println(tab + " */");
        output.print(tab + "public ");

        member_obj = (IdlUnionMember) obj.current();

        IdlObject discri = member_obj;
        member_obj.reset();
        disc = member_obj.current();
        translate_type(member_obj.current(), output);

        output.println(" discriminator()");
        output.println(tab + "{");
        output.println(tab2 + "return __d;");
        output.println(tab + "}");
        output.println();


        obj.next();
        i = 0;

        while (obj.end() != true)
        {
            member_obj = (IdlUnionMember) obj.current();

            if (member_obj.isAsNext() == false)
            {
                doMap = false;
                output.println(tab + "/**");
                output.println(tab + " * Set " + member_obj.name() + " value");
                output.println(tab + " */");
                output.print(tab + "public void " + member_obj.name() + "(");

                member_obj.reset();
                translate_type(member_obj.current(), output);

                output.println(" value)");
                output.println(tab + "{");

                if (i != idx)
					output.println(tab2 + "__d = " + translate_to_union_case_expression((IdlUnionMember) discri, fixEnumExpression(member_obj.getExpression())) + ";");
                    //output.println(tab2 + "__d = " + translate_to_union_case_expression((IdlUnionMember) discri, member_obj.getExpression()) + ";");
                else
                    output.println(tab2 + "__d = " + find_default_value(obj) + ";");

                output.println(tab2 + "_" + member_obj.name() + " = value;");

                output.println(tab + "}");

                output.println();

                if (i == idx)
                {
                    output.println(tab + "/**");
                    output.println(tab + " * Set " + member_obj.name() + " value");
                    output.println(tab + " */");
                    output.print(tab + "public void " + member_obj.name() + "(");

                    translate_type(disc, output);

                    output.print(" dvalue, ");

                    member_obj.reset();
                    translate_type(member_obj.current(), output);

                    output.println(" value)");
                    output.println(tab + "{");
                    output.println(tab2 + "__d = dvalue;");

                    output.println(tab2 + "_" + member_obj.name() + " = value;");
                    output.println(tab + "}");
                    output.println();

                }

                output.println(tab + "/**");
                output.println(tab + " * Get " + member_obj.name() + " value");
                output.println(tab + " */");
                output.print(tab + "public ");

                member_obj.reset();
                translate_type(member_obj.current(), output);

                output.println(" " + member_obj.name() + "()");
                output.println(tab + "{");
                output.println(tab2 + "return _" + member_obj.name() + ";");
                output.println(tab + "}");
                output.println();
            }
            else
            {
                if (doMap == false)
                {
                    output.println(tab + "/**");
                    output.println(tab + " * Set " + member_obj.name() + " value");
                    output.println(tab + " */");
                    output.print(tab + "public void " + member_obj.name() + "(");

                    translate_type(disc, output);

                    output.print(" dvalue, ");

                    member_obj.reset();
                    translate_type(member_obj.current(), output);

                    output.println(" value)");
                    output.println(tab + "{");
                    output.println(tab2 + "__d = dvalue;");

                    output.println(tab2 + "_" + member_obj.name() + " = value;");
                    output.println(tab + "}");
                    output.println();

                    doMap = true;
                }

            }

            obj.next();

            i++;
        }

        /*
         // TODO: Bugtracker #515917
         Two default modifier methods, both named __default(), are
         generated if there is no explicit default case label, and the
         set of case labels does not completely cover the possible
         values of the discriminant. The simple method taking no
         arguments and returning void sets the discriminant to the
         first available default value starting from a 0 index of the
         discriminant type. The second method takes a discriminator as
         parameter and returns void. Both of these of methods shall
         leave the union with a discriminator value set, and the value
         member uninitialized.
        */
        if (idx == -1)
        {
            output.println(tab + "/**");
            output.println(tab + " * default access");
            output.println(tab + " */");
            output.println(tab + "public void __default()");
            output.println(tab + "{");
            output.println(tab + "}");

            output.println(tab + "/**");
            output.println(tab + " * default access");
            output.println(tab + " */");
            output.print(tab + "public void __default(");
            translate_type(disc, output);
            output.println(" _discriminator)");
            output.println(tab + "{");
            output.println(tab + "}");
        }

        obj.reset();
        obj.next();

        if (((IdlUnionMember) obj.current()).getExpression().equals("true ") ||
                ((IdlUnionMember) obj.current()).getExpression().equals("false "))
        {
            output.println(tab + "/**");
            output.println(tab + " * Return an int value for discriminator");
            output.println(tab + " */");
            output.println(tab + "public int toInt()");
            output.println(tab + "{");
            output.println(tab + "if (__d == true)");
            output.println(tab2 + "return 1;");
            output.println(tab + "return 0;");
            output.println(tab + "}");
        }

        output.println("}");

        output.close();

        write_helper(obj, writeInto);
        write_holder(obj, writeInto);
    }

    /**
     * Translate a typedef
     *
     * @param obj the typedef to translate
     * @param writeInto the directory where the typedef must be defined
     */
    private void translate_typedef(IdlObject obj, File writeInto)
    {
        obj.reset();

        switch (obj.current().kind())
        {

        case IdlType.e_string :

        case IdlType.e_wstring :

        case IdlType.e_simple :
            //write_helper(obj, writeInto);
            break;

        case IdlType.e_union :

        case IdlType.e_struct :

        case IdlType.e_enum :
            //write_helper(obj, writeInto);
            break;

        case IdlType.e_fixed :

        case IdlType.e_sequence :

        case IdlType.e_array :
            //write_helper(obj, writeInto);
            write_holder(obj, writeInto);
            break;

        case IdlType.e_ident :
            //write_helper(obj, writeInto);

            if ((final_type(obj.current()).kind() == IdlType.e_sequence) ||
                    (final_type(obj.current()).kind() == IdlType.e_array))
                write_holder(obj, writeInto);

            break;

        case IdlType.e_typedef :
            //write_helper(obj, writeInto);

            break;
        }
    }

    /**
     * Translate an exception
     *
     * @param obj exception to translate
     * @param writeInto the directory where the exception must be defined
     */
    private void translate_exception(IdlObject obj, File writeInto)
    {
		File sub = writeInto;
		File writeIntoAbeans = createDirectory(ABEANS_POSTFIX_DOTLESS, writeInto);
        PrintWriter output = newFile(writeIntoAbeans, obj.name());
        String old_pkg = current_pkg;

        IdlStructMember member_obj;

        addDescriptiveHeader(output, obj);

        if (isEmpty(obj) == false)
            sub = createDirectory(obj.name() + "Package" , writeInto);

        addToPkg(obj, obj.name() + "Package");

        // Define sub-types
        obj.reset();

        while (obj.end() != true)
        {
            obj.current().reset();

            switch (obj.current().current().kind())
            {

            case IdlType.e_union :
                translate_union(obj.current().current(), sub);
                break;

            case IdlType.e_struct :
                translate_struct(obj.current().current(), sub);
                break;

            case IdlType.e_enum :
                translate_enum(obj.current().current(), sub);
                break;
            }

            obj.next();
        }

        current_pkg = old_pkg;

        // Define the class
        output.println("public final class " + obj.name() + " extends abeans.pluggable.RemoteException");
        output.println("{");

        // Declare exception members
        obj.reset();

        while (obj.end() != true)
        {
            member_obj = (IdlStructMember) obj.current();

            output.println(tab + "/**");
            output.println(tab + " * Exception member " + member_obj.name());
            output.println(tab + " */");
            output.print(tab + "public ");

            member_obj.reset();
            translate_type(member_obj.current(), output);

            output.println(" " + member_obj.name() + ";");
            output.println();

            obj.next();
        }

        // Default constructors
        
		output.println(tab + "/**");
		output.println(tab + " * Creates a new instance of this exception by specifying the source of the");
		output.println(tab + " * exception (instance throwing it) and the relevant message.");
		output.println(tab + " * ");
		output.println(tab + " * @param instance 	identifier of the instance throwing this exception, non-<code>null</code>");
		output.println(tab + " * @param message		a string indicating the exception kind, non-<code>null</code>");
		output.println(tab + " */");
		output.println(tab + "public " + obj.name() + "(abeans.core.Identifiable instance, String message)"); 
		output.println(tab + "{");
		output.println(tab2 + "super(instance, message);");
		output.println(tab + "}");
		output.println();
	
		output.println(tab + "/**");
		output.println(tab + " * Creates a new instance of this exception by specifying the source, message and the");
		output.println(tab + " * exception that caused this exception to be thrown.");
		output.println(tab + " * ");
		output.println(tab + " * @param	instance 	identifier of the instance throwing this exception, non-<code>null</code>");
		output.println(tab + " * @param	message		a string indicating the exception kind, non-<code>null</code>");
		output.println(tab + " * @param t			the exception that caused this exception to be thrown");
		output.println(tab + " */");
		output.println(tab + "public " + obj.name() + "(abeans.core.Identifiable instance, String message, Throwable t)"); 
		output.println(tab + "{");
		output.println(tab2 + "super(instance, message, t);");
		output.println(tab + "}");
		output.println();

		// Constructors with fields initialization
		if (obj.length() != 0)
		{
			output.println(tab + "/**");
			output.println(tab + " * Creates a new instance of this exception by specifying the source of the");
			output.println(tab + " * exception (instance throwing it), relevant message and fields.");
			output.println(tab + " * ");
			output.println(tab + " * @param instance 	identifier of the instance throwing this exception, non-<code>null</code>");
			output.println(tab + " * @param message		a string indicating the exception kind, non-<code>null</code>");

			obj.reset();
			while (obj.end() != true)
			{
				member_obj = (IdlStructMember) obj.current();
				output.println(tab + " * @param " + member_obj.name() + " " + member_obj.name() + " exception member");
				obj.next();
			}

			output.println(tab + " */");
			
			output.print(tab + "public " + obj.name() + "(abeans.core.Identifiable instance, String message, "); 
			obj.reset();
			while (obj.end() != true)
			{
				member_obj = (IdlStructMember) obj.current();

				translate_type(member_obj.current(), output);
				output.print(" " + member_obj.name());
				obj.next();

				if (obj.end() != true)
					output.print(", ");
			}
			output.println(")");

			output.println(tab + "{");
			output.println(tab2 + "super(instance, message);");

			obj.reset();
			while (obj.end() != true)
			{
				member_obj = (IdlStructMember) obj.current();
				output.println(tab2 + "this." + member_obj.name() + " = " + member_obj.name() + ";");
				obj.next();
			}

			output.println(tab + "}");
            output.println();






			output.println(tab + "/**");
			output.println(tab + " * Creates a new instance of this exception by specifying the source, message and the");
			output.println(tab + " * exception that caused this exception to be thrown and fields.");
			output.println(tab + " * ");
			output.println(tab + " * @param instance 	identifier of the instance throwing this exception, non-<code>null</code>");
			output.println(tab + " * @param message		a string indicating the exception kind, non-<code>null</code>");
			output.println(tab + " * @param t			the exception that caused this exception to be thrown");

			obj.reset();
			while (obj.end() != true)
			{
				member_obj = (IdlStructMember) obj.current();
				output.println(tab + " * @param " + member_obj.name() + " " + member_obj.name() + " exception member");
				obj.next();
			}

			output.println(tab + " */");
			
			output.print(tab + "public " + obj.name() + "(abeans.core.Identifiable instance, String message, Throwable t, "); 
			obj.reset();
			while (obj.end() != true)
			{
				member_obj = (IdlStructMember) obj.current();

				translate_type(member_obj.current(), output);
				output.print(" " + member_obj.name());
				obj.next();

				if (obj.end() != true)
					output.print(", ");
			}
			output.println(")");

			output.println(tab + "{");
			output.println(tab2 + "super(instance, message, t);");

			obj.reset();
			while (obj.end() != true)
			{
				member_obj = (IdlStructMember) obj.current();
				output.println(tab2 + "this." + member_obj.name() + " = " + member_obj.name() + ";");
				obj.next();
			}

			output.println(tab + "}");
			output.println();

        }


        output.println("}");

        output.close();

        //write_helper(obj, writeInto);
        write_holder(obj, writeInto);
    }

    /**
     * Translate an attribute
     *
     * @param obj attribute to translate
     * @param output write access
     */
    private static void translate_attribute (IdlObject obj, PrintWriter output)
    {
        if (obj.hasComment())
            javadoc(output, obj);
        else
        {

            output.println(tab + "/**");
            output.println(tab + " * Read accessor for " + obj.name() + " attribute");
            output.println(tab + " * @return the attribute value");
            output.println(tab + " */");
        }

        output.print(tab + "public ");

        if (obj.upper().kind() == IdlType.e_value)
            output.print("abstract ");

        obj.reset();

        translate_type(obj.current(), output);

        output.println(" " + obj.name() + "();");

        output.println();

        if (((IdlAttribute) obj).readOnly() == false)
        {
            if (obj.hasComment())
                javadoc(output, obj);
            else
            {
                output.println(tab + "/**");
                output.println(tab + " * Write accessor for " + obj.name() + " attribute");
                output.println(tab + " * @param value the attribute value");
                output.println(tab + " */");
            }

            output.print(tab + "public ");

            if (obj.upper().kind() == IdlType.e_value)
                output.print("abstract ");

            output.print("void " + obj.name() + "(");

            translate_type(obj.current(), output);

            output.println(" value);");

            output.println();
        }
    }

    /**
     * Translate an attribute for the user code
     *
     * @param obj attribute to translate
     * @param output write access
     */
    private static void translate_bean_attribute (IdlObject obj, PrintWriter output, boolean isParentLinkable, boolean doesImplementInterceptor, boolean isAttributeLinkable)
    {
		if (isAttributeLinkable)
		{
			output.println(tab + "/**");
			output.println(tab + " * " + obj.name() + " linkable attribute");
			output.println(tab + " */");
			output.print(tab + "public ");
	
			obj.reset();
			translate_type(obj.current(), output);
	
			String capitalName = String.valueOf(Character.toUpperCase(obj.name().charAt(0)));
			if (obj.name().length() > 1)
			 	capitalName += obj.name().substring(1);
	
			output.println(" get"  + capitalName + "()");
			output.println(tab  + "{");
			output.println(tab2 + "return "  + obj.name() + ";");
			output.println(tab  + "}");
		}


        output.println(tab + "/**");
        output.println(tab + " * " + obj.name() + " read attribute");
        output.println(tab + " */");
		if (isAttributeLinkable)
        	output.print(tab + "protected ");
        else
			output.print(tab + "public ");

        obj.reset();
        translate_type(obj.current(), output);

		//if (isAttributeLinkable)
        //	output.println(" " + obj.name() + "()");
        //else
		output.println(" " + obj.name() + "()");
		output.println(tab2 + "throws abeans.engine.RequestException");
        output.println(tab + "{");

		// invoke
		// TODO check
		if (isParentLinkable)
		{
			if (doesImplementInterceptor)
				output.println(tab2 + "Object __retVal = abeans.models.acs.baci.InvokeUtilities.invokeSync(getRemoteInfo(), \"_get_" + obj.name() + "\", getProxy(), getDefaultTimeout(), this, this, null);");
			else
				output.println(tab2 + "Object __retVal = abeans.models.acs.baci.InvokeUtilities.invokeSync(getRemoteInfo(), \"_get_" + obj.name() + "\", getProxy(), getDefaultTimeout(), interceptor, this, null);");
		}
		else
			// bypass DB  
			output.println(tab2 + "Object __retVal = abeans.models.acs.baci.InvokeUtilities.invokeSync(null, \"_get_" + obj.name() + "\", getProxy(), 0, null, null, null);");
			
		// return
		output.println(tab2 + "return " + translate_from_object(obj.current(), "__retVal") + ";");

        output.println(tab + "}");
        output.println();

        if (((IdlAttribute) obj).readOnly() == false)
        {
            output.println(tab + "/**");
            output.println(tab + "/* " + obj.name() + " write attribute");
            output.println(tab + " */");
            output.print(tab + "public void " + obj.name() + "(");

            translate_type(obj.current(), output);

            output.println(" value)");
			output.println(tab2 + "throws abeans.engine.RequestException");
            output.println(tab + "{");

			output.println(tab2 + "Object[] parameters = { " + translate_to_object(obj.current(), "value") + " };");

			// invoke
			// TODO check
			if (isParentLinkable)
			{
				if (doesImplementInterceptor)
					output.println(tab2 + "abeans.models.acs.baci.InvokeUtilities.invokeSync(getRemoteInfo(), \"_set_" + obj.name() + "\", getProxy(), getDefaultTimeout(), this, this, parameters);");
				else
					output.println(tab2 + "abeans.models.acs.baci.InvokeUtilities.invokeSync(getRemoteInfo(), \"_set_" + obj.name() + "\", getProxy(), getDefaultTimeout(), interceptor, this, parameters);");
			}
			else
				// bypass DB  
				output.println(tab2 + "abeans.models.acs.baci.InvokeUtilities.invokeSync(null, \"_set_" + obj.name() + "\", getProxy(), 0, null, null, parameters);");

            output.println(tab + "}");
            output.println();
        }
    }

    /**
     * Translate a read attribute for a proxy
     *
     * @param obj attribute to translate
     * @param output write access
     */
    private static void translate_read_attribute_proxy (IdlObject obj, PrintWriter output)
    {
        obj.reset();

		// invoke
        output.print(tab2);
        translate_type_CORBA(obj.current(), output);
		//output.println(" value = ((" + fullname_CORBA(obj.upper()) + ")delegate)." + obj.name() + "();");
		output.println(" value = delegate." + obj.name() + "();");
		output.println();

		// map to Abeans R3 BACI model java type and to java.lang.Object
        output.print(tab2 + "return ");
		fromCORBA(obj.current(), "value", output, 0, true);
		output.println(";");
    }

    /**
     * Translate a write attribute for a proxy
     *
     * @param obj attribute to translate
     * @param output write access
     */
    private static void translate_write_attribute_proxy (IdlObject obj, PrintWriter output)
    {
        obj.reset();

		// read and map java.lang.Object (Abeans R3 model type) to CORBA type  
        output.print(tab2);
        translate_type_CORBA(obj.current(), output);
        output.print(" param = ");
		toCORBA(obj.current(), "parameters[0]", output, 0, true);
		output.println(";");

		// invoke
        //output.println(tab2 + "((" + fullname_CORBA(obj.upper()) + ")delegate)." + obj.name() + "(param);");
		output.println(tab2 + "delegate." + obj.name() + "(param);");
		output.println();

        output.println(tab2 + "return null;");
    }

    /**
     * Return the context associated with an operation
     *
     * @param obj the operation
     * @return associated context object
     */
    private static IdlContext getContext(IdlObject obj)
    {
        int p = obj.pos();
        IdlObject find = null;

        obj.reset();

        while (obj.end() != true)
        {
            if (obj.current().kind() == IdlType.e_context)
            {
                find = obj.current();
                break;
            }

            obj.next();
        }

        obj.pos(p);

        return (IdlContext) find;
    }

    /**
     * Translate an operation
     *
     * @param obj operation to translate
     * @param output write access
     */
	// TODO org.omg -> translate_operation
    private static void translate_operation(IdlObject obj, PrintWriter output)
    {
        IdlRaises r;
        IdlContext c;
        boolean someParams = false;

        if (obj.hasComment())
            javadoc(output, obj);
        else
        {
            output.println(tab + "/**");
            output.println(tab + " * Operation " + obj.name());
            output.println(tab + " */");
        }

        output.print(tab + "public ");

        if (obj.upper().kind() == IdlType.e_value)
            output.print("abstract ");

        obj.reset();

        translate_type(obj.current(), output);

        output.print(" " + obj.name() + "(");

        obj.next();

        if (obj.end() != true)
        {
            if (obj.current().kind() == IdlType.e_param)
            {
                someParams = true;

                while (obj.end() != true)
                {

                    obj.current().reset();
                    translate_parameter(obj.current().current(), output, ((IdlParam) obj.current()).param_attr());

                    output.print(" " + obj.current().name());

                    obj.next();

                    if (obj.end() != true)
                    {
                        if (obj.current().kind() == IdlType.e_param)
                            output.print(", ");
                        else
                            break;
                    }
                }
            }
        }

        c = getContext(obj);

        if (c != null)
        {
            if (someParams == true)
                output.print(", ");

            output.print("org.omg.CORBA.Context ctx");
        }

        output.print(")");

        if (obj.end() != true)
            if (obj.current().kind() == IdlType.e_raises)
            {
                output.println();
                output.print(tab2 + "throws ");
                r = (IdlRaises) obj.current();

                r.reset();

                while (r.end() != true)
                {

                    output.print(fullname(r.current()));

                    r.next();

                    if (r.end() != true)

                        output.print(", ");

                }
            }

        output.println(";");
        output.println();
    }

	/**
	 * Translate an operation for callback.
	 *
	 * @param obj operation to translate
	 * @param output write access
	 */
	private static void translate_operation_callback(IdlObject obj, PrintWriter output)
	{
		
	    if (obj.hasComment())
	        javadoc(output, obj);
	    else
	    {
	        output.println(tab + "/**");
	        output.println(tab + " * Operation " + obj.name());
	        output.println(tab + " */");
	    }
	
	    output.print(tab + "public ");
	
	    obj.reset();
	
	    translate_type_CORBA(obj.current(), output);
	
	    output.print(" " + obj.name() + "(");
	
	    obj.next();
	
	    if (obj.end() != true)
	    {
	        if (obj.current().kind() == IdlType.e_param)
	        {
	            while (obj.end() != true)
	            {
	
	                obj.current().reset();
	                translate_parameter_CORBA(obj.current().current(), output, ((IdlParam) obj.current()).param_attr());
	
	                output.print(" " + obj.current().name());
	
	                obj.next();
	
	                if (obj.end() != true)
	                {
	                    if (obj.current().kind() == IdlType.e_param)
	                        output.print(", ");
	                    else
	                        break;
	                }
	            }
	        }
	    }
	
	
	    output.print(")");
	
	    if (obj.end() != true)
	        if (obj.current().kind() == IdlType.e_raises)
	        {
	            output.println();
	            output.print(tab2 + "throws ");
				IdlRaises r = (IdlRaises) obj.current();
	
	            r.reset();
	
	            while (r.end() != true)
	            {
	
	                output.print(fullname_CORBA(r.current()));
	
	                r.next();
	
	                if (r.end() != true)
	
	                    output.print(", ");
	
	            }
	        }
	
	    output.println(" {}");
	    output.println();
	}

    /**
     * Translate an operation for the user code
     *
     * @param obj operation to translate
     * @param output write access
     */
    private static void translate_bean_operation(IdlObject obj, PrintWriter output, boolean isParentLinkable, boolean doesImplementInterceptor)
    {
        output.println(tab + "/**");
        output.println(tab + " * Operation " + obj.name());
        output.println(tab + " */");
        output.print(tab + "public ");

        obj.reset();
        
        IdlObject retObj = obj.current();
        boolean returnsValue = !((retObj.kind() == IdlType.e_simple) &&
								 (((IdlSimple)retObj).internal() == Token.t_void));
        
        translate_type(retObj, output);

        output.print(" " + obj.name() + "(");

        obj.next();

		// last 2 parameters have to be callback and CBDescIn
		boolean isBACIaction = false;

		IdlObject last = null;
		IdlObject secondLast = null;

		// BACI action does not return value
		if (!returnsValue)
		{
			if (obj.end() != true)
			{
				if (obj.current().kind() == IdlType.e_param)
				{
					while (obj.end() != true)
					{
						secondLast = last;
						last = obj.current();						
	
						obj.next();
	
						if (obj.end() != true)
						{
							if (obj.current().kind() != IdlType.e_param)
								break;
						}
					}
				}
			}
			
			// we have a last 2 parameters
			if (secondLast != null)
			{
				// in parameter check
				IdlParam param2 = (IdlParam)secondLast;
				IdlParam param = (IdlParam)last;
				if (param2.param_attr() == 0 && param.param_attr() == 0)
				{
					if (isInheritedFrom(final_type(param2.current()), BASE_CALLBACK_ID))
					{
					 	last = final_type(param.current());
					 	if (last.kind() == IdlType.e_struct && last.name().equals("CBDescIn"))
					 		isBACIaction = true; 
					}
				}
			}
	
			obj.reset();
			obj.next();
		}

		boolean hasParameters = false;
		
        if (obj.end() != true)
        {
            if (obj.current().kind() == IdlType.e_param)
            {

                while (obj.end() != true)
                {
                	// do not generate those params...
					if (isBACIaction && obj.current() == secondLast)
						break;
					
					if (hasParameters)
						output.print(", ");
					
					hasParameters = true;
					
                    obj.current().reset();
                    translate_parameter(obj.current().current(), output, ((IdlParam) obj.current()).param_attr());

                    output.print(" " + obj.current().name());

                    obj.next();

                    if (obj.end() != true)
                    {
                        if (obj.current().kind() != IdlType.e_param)
                            break;
                    }
                }
            }
        }

        output.println(")");

		output.print(tab2 + "throws abeans.engine.RequestException");


        if (obj.end() != true)
            if (obj.current().kind() == IdlType.e_raises)
            {
				output.print(", ");

				IdlRaises r = (IdlRaises) obj.current();
                r.reset();
                while (r.end() != true)
                {

                    output.print(fullname(r.current()));

                    r.next();

                    if (r.end() != true)

                        output.print(", ");

                }
            }

        output.println();
        output.println(tab + "{");
        
        
		boolean hasOutParameters = false;
		boolean notFirst = false;

        if (hasParameters)
        {
			output.print(tab2 + "Object[] parameters = { ");

			obj.reset();
			obj.next();
			if (obj.end() != true)
			{
				if (obj.current().kind() == IdlType.e_param)
				{
					while (obj.end() != true)
					{
						// do not generate those params...
						if (isBACIaction && obj.current() == secondLast)
							break;
						
						if (notFirst)
							output.print(", ");
							
						notFirst = true;

						obj.current().reset();

						if (((IdlParam)obj.current()).param_attr() == 0)
							output.print(translate_to_object(obj.current(), obj.current().name()));
						else
						{
							hasOutParameters = true;
							output.print(obj.current().name());
						}

						obj.next();

						if (obj.end() != true)
						{
							if (obj.current().kind() != IdlType.e_param)
								break;
						}
					}
				}
			}
			output.println(" };");
        }


		// return param
		output.print(tab2);
		if (returnsValue)
			 output.print("Object __retVal = ");

		// invoke
		// TODO check
		if (isParentLinkable)
		{

			if (isBACIaction)
			{
				if (doesImplementInterceptor)
					output.print("abeans.models.acs.baci.InvokeUtilities.invokeAsync(getRemoteInfo(), \"" + obj.name() + "\", getProxy(), getDefaultTimeout(), this, this, " + translate_type(secondLast.current()) + "Impl.getInstance(), ");
				else
					output.print("abeans.models.acs.baci.InvokeUtilities.invokeAsync(getRemoteInfo(), \"" + obj.name() + "\", getProxy(), getDefaultTimeout(), interceptor, this, " + translate_type(secondLast.current()) + "Impl.getInstance(), ");
			}
			else
			{
				if (doesImplementInterceptor)
					output.print("abeans.models.acs.baci.InvokeUtilities.invokeSync(getRemoteInfo(), \"" + obj.name() + "\", getProxy(), getDefaultTimeout(), this, this, ");
				else
					output.print("abeans.models.acs.baci.InvokeUtilities.invokeSync(getRemoteInfo(), \"" + obj.name() + "\", getProxy(), getDefaultTimeout(), interceptor, this, ");
			}
		}
		else
			// bypass DB  
			output.print("abeans.models.acs.baci.InvokeUtilities.invokeSync(null, \"" + obj.name() + "\", getProxy(), 0, null, null, ");
			
		if (hasParameters)
			output.println("parameters);");
		else
			output.println("null);");

		// out params
		if (hasOutParameters)
		{
			output.println();

			obj.reset();
			obj.next();
			if (obj.end() != true)
			{
				if (obj.current().kind() == IdlType.e_param)
				{
					int i = 0;
					while (obj.end() != true)
					{
						obj.current().reset();

						int attr = ((IdlParam)obj.current()).param_attr(); 
						if (attr != 0)
						{
							output.println(tab2 + obj.current().name() + " = (" + translate_parameter(obj.current().current(), attr) + ")parameters[" + i + "];");
						}

						obj.next();

						if (obj.end() != true)
						{
							if (obj.current().kind() != IdlType.e_param)
								break;
						}
						
						i++;
					}
				}
			}
			
			if (returnsValue)
				output.println();
		}
			
		// return
		if (returnsValue)
			output.println(tab2 + "return " + translate_from_object(retObj, "__retVal") + ";");

        output.println(tab + "}");
        output.println();
    }

    /**
     * Translate an operation for a proxy
     *
     * @param obj operation to translate
     * @param output write access
     */
    private static void translate_operation_proxy(String methodName, IdlObject obj, PrintWriter output)
    {
        // extract the parameters
        obj.reset();
        
        // skip return value
        obj.next();

		int i = 0;
        if (obj.end() != true)
        {
            if (obj.current().kind() == IdlType.e_param)
                while (obj.end() != true)
                {
                    obj.current().reset();

					output.println(tab2 + "// " + ((IdlParam) obj.current()).name());

                    switch (((IdlParam) obj.current()).param_attr())
                    {

                    case 0 :
                        output.print(tab2);
                        translate_type_CORBA(obj.current().current(), output);
                        output.print(" arg" + i + "_in = ");
                        toCORBA(obj.current().current(), "parameters[" + i + "]", output , 0, true);
						output.println(";");
                        break;

                    case 1 :
                        output.print(tab2);
                        translate_parameter_CORBA(obj.current().current(), output, 1);
                        output.print(" arg" + i + "_out = ");
						toCORBA(obj.current().current(), "parameters[" + i + "]", output , 1, true);
						output.println(";");
                        break;

                    case 2 :
						output.print(tab2);
						translate_parameter_CORBA(obj.current().current(), output, 2);
						output.print(" arg" + i + "_inout = ");
						toCORBA(obj.current().current(), "parameters[" + i + "]", output , 2, true);
						output.println(";");
						
						break;
                    }

                    i++;

                    obj.next();

                    if (obj.end() != true)
                    {
                        if (obj.current().kind() != IdlType.e_param)
                            break;
                    }
                }
        }

        i = 0;
        output.println();


		/*
		IdlContext c = getContext(obj);

        if (c != null)
        {
            output.println(tab2 + "org.omg.CORBA.Context arg_ctx = _is.read_Context();");
            output.println();
        }
        */

		boolean raises = false;

        // check if the exceptions have to be managed
        if (obj.end() != true)
            if (obj.current().kind() == IdlType.e_raises)
            {
                output.println(tab2 + "try");
                output.println(tab2 + "{");
                raises = true;
            }


		boolean returnsValue = false;

		obj.reset();

        // do the invoke
        if (obj.current().kind() == IdlType.e_simple)
        {
            if (((IdlSimple) obj.current()).internal() != Token.t_void)
            {
				returnsValue = true;

                if (raises == true)
                    output.print(tab);

                output.print(tab2);

                translate_type_CORBA(obj.current(), output);

                output.print(" _arg_result = ");
            }
            else
            {
                if (raises == true)
                    output.print(tab);

                output.print(tab2);
            }
        }
        else
        {
			returnsValue = true;

            if (raises == true)
                output.print(tab);

            output.print(tab2);

            translate_type_CORBA(obj.current(), output);

            output.print(" _arg_result = ");
        }

        //output.print("((" + fullname_CORBA(obj.upper()) + ")delegate)." + obj.name() + "(");
		output.print("delegate." + obj.name() + "(");

		//boolean someParams = false;

		// skip return value
        obj.next();

        if (obj.end() != true)
        {
            if (obj.current().kind() == IdlType.e_param)
            {
                //someParams = true;

                while (obj.end() != true)
                {
                    obj.current().reset();

                    switch (((IdlParam) obj.current()).param_attr())
                    {

                    case 0 :
                        output.print("arg" + i + "_in");
                        break;

                    case 1 :
                        output.print("arg" + i + "_out");
                        break;

                    case 2 :
                        output.print("arg" + i + "_inout");
                        break;
                    }

                    i++;

                    obj.next();

                    if (obj.end() != true)
                    {
                        if (obj.current().kind() != IdlType.e_param)
                            break;
                        else
                            output.print(", ");
                    }
                }
            }
        }

        i = 0;

		/*
        if (c != null)
        {
            if (someParams == true)
                output.print(", ");

            output.print("arg_ctx");
        }
		*/
		
        output.println(");");
		output.println();

        // encode return parameter
        obj.reset();

        if (obj.current().kind() == IdlType.e_simple)
        {
            if (((IdlSimple) obj.current()).internal() != Token.t_void)
            {
            	if (raises)
					output.print(tab);
					
                output.print(tab2 + "java.lang.Object retVal = ");
                fromCORBA(obj.current(), "_arg_result", output, 0, true);
				output.println(";");
            }
        }
        else
        {
			if (raises)
				output.print(tab);

			output.print(tab2 + "java.lang.Object retVal = ");
			fromCORBA(obj.current(), "_arg_result", output, 0, true);
			output.println(";");
        }

		//
		// out parameters (transfer holder values)
		//

		// skip return value
        obj.next();

        if (obj.end() != true)
        {
            if (obj.current().kind() == IdlType.e_param)
                while (obj.end() != true)
                {
                    obj.current().reset();

                    switch (((IdlParam) obj.current()).param_attr())
                    {

                    case 0 :
                        break;

                    case 1 :

                        if (raises)
                            output.print(tab);

                        output.print(tab2);

						//output.println("parameters[" + i + "].value = arg" + i + "_out.value;");
						output.print("((");
						translate_parameter(obj.current().current(), output, 1);
						output.print(")parameters[" + i + "]).value = ");
						fromCORBA(obj.current().current(), "arg" + i + "_out.value", output, 0, false);
						output.println(";");

                        break;

                    case 2 :
                        if (raises)
                            output.print(tab);

                        output.print(tab2);

						//output.println("parameters[" + i + "].value = arg" + i + "_inout.value;");
						output.print("((");
						translate_parameter(obj.current().current(), output, 2);
						output.print(")parameters[" + i + "]).value = ");
						fromCORBA(obj.current().current(), "arg" + i + "_inout.value", output, 0, false);
						output.println(";");

                        break;
                    }

                    i++;

                    obj.next();

                    if (obj.end() != true)
                    {
                        if (obj.current().kind() != IdlType.e_param)
                            break;
                    }
                }
        }


		if (raises)
			output.print(tab);

		// return value
		if (returnsValue)
			output.println(tab2 + "return retVal;");
		else
			output.println(tab2 + "return null;");
		
		// catch th exceptions
		if (obj.end() != true)
			if (obj.current().kind() == IdlType.e_raises)
			{

				IdlRaises r = (IdlRaises) obj.current();

				r.reset();

				output.println(tab2 + "}");

				while (r.end() != true)
				{

					output.println(tab2 + "catch (" + fullname_CORBA(r.current()) + " _exception)");
					output.println(tab2 + "{");

					//output.println(tab3 + "abeans.pluggable.RemoteException re = new abeans.pluggable.RemoteException(this, _exception.getMessage(), _exception);");
					output.print(tab3 + fullname(r.current()) + " re = new " + fullname(r.current()) + "(this, _exception.getMessage(), _exception");

					IdlObject rm = r.current();
					rm.reset();
					while (rm.end() != true)
					{
						IdlStructMember member_obj = (IdlStructMember) rm.current();
						output.print(", ");
						fromCORBA(member_obj.current(), "_exception." + member_obj.name(), output, 0, false);
						rm.next();

					}
					output.println(");");


					output.println(tab3 + "re.caughtIn(this, \"" + methodName + "\");");
					output.println(tab3 + "re.putValue(\"parameters\", parameters);");
					output.println(tab3 + "throw re;");
					output.println(tab2 + "}");

					r.next();
				}
			}

    }

    /**
     * This method check a value type is empty in terms of
     * type typedef, union, exception...
     */
    private static boolean isEmptyValue(IdlObject obj)
    {
        obj.reset();

        while (obj.end() != true)
        {
            switch (obj.current().kind())
            {

            case IdlType.e_enum :
                return false;

            case IdlType.e_struct :
                return false;

            case IdlType.e_union :
                return false;

            case IdlType.e_typedef :
                return false;

            case IdlType.e_exception :
                return false;

            case IdlType.e_native :
                return false;
            }

            obj.next();
        }

        return true;
    }

    /**
     * Check a type is empty in terms of
     * type union, struct or enum
     */
    private static boolean isEmpty(IdlObject obj)
    {
        obj.reset();

        while (obj.end() != true)
        {
            obj.current().reset ();

            switch (obj.current().current().kind())
            {

            case IdlType.e_enum :
                return false;

            case IdlType.e_struct :
                return false;

            case IdlType.e_union :
                return false;
            }

            obj.next();
        }

        return true;
    }

    /**
     * Translate an property interface for the user.
     * Property already has knowledge of BACI and Abeans,
     * also naming convention of P<type>, RO<type>, RW<type>.
     * Expected single inheritance: TypelessProperty -> P<type> -> RO<type> | RW<type> -> ...
     * Implementation of  P<type>, RO<type>, RW<type> properties is fixed, any user-added 
     * methods and attributes should be implemented by inheriting P<type>, RO<type> | RW<type> types.
     *
     * @param obj a property interface to translate
     * @param writeInto the directory where the interface must be defined
     * @param inheritanceList non-recursive (direct) inheritance list of the interface
     * @return	<code>true</code> if interface was handled by this method, <code>false</code> otherwise
     */
    private boolean translate_bean_interface_property(IdlObject obj, File writeInto, List inheritanceList)
    {

		// it always has at least one... 
		IdlInterface itf = (IdlInterface)inheritanceList.get(0);
		if (itf.isForward())
			itf = itf.getInterface();
	
		// 0 = P<type>, 1 = RO<type>, 2 = RW<type>, other is invalid
		int mode = -1;
		
		// determine mode 
		IdlInterface ptype = null;
		if (obj.name().charAt(0) == 'P')
		{
			if (itf.name().charAt(0) == 'P')
				return false;
			else
			{
				mode = 0;
				ptype = (IdlInterface)obj; 
			}
		}
		else if (obj.name().startsWith("RO"))
		{
			if (itf.name().startsWith("RO"))
				return false;
			else
			{
				mode = 1;
				ptype = findPtypeInterface(obj);
			}
		}
		else if (obj.name().startsWith("RW"))
		{
			if (itf.name().startsWith("RW"))
				return false;
			else
			{
				mode = 2;
				ptype = findPtypeInterface(obj);
			}
		}
		else
			return false;

		// get property type
		// find get_sync method
		IdlObject propertyType = null;
		
		if (ptype != null)
		{
			ptype.reset();
			while (ptype.end() != true)
			{
				if (ptype.current().kind() == IdlType.e_operation)
				{
					if (ptype.current().name().equals("get_sync"))
					{
						IdlObject retObj = ptype.current();
						retObj.reset();
						propertyType = final_type(retObj.current());
						break; 
					}
				}
	
				ptype.next();
			}
		}
		
		
		String historyHolder = null;
		
		// determine get_history value holder
		if (mode == 0)
		{
			if (ptype != null)
			{
				ptype.reset();
				while (ptype.end() != true)
				{
					if (ptype.current().kind() == IdlType.e_operation)
					{
						if (ptype.current().name().equals("get_history"))
						{
							IdlObject retObj = ptype.current();
							retObj.reset();
							// skip return value
							retObj.next();
							// skip length
							retObj.next();
							historyHolder = translate_parameter(retObj.current().current(), 1);
							break; 
						}
					}
		
					ptype.next();
				}
			}
		}

		// unable to handle property
		if (propertyType == null)
		{
			System.out.println("WARNING: Property " + ((IdlInterface)obj).getId() + " has no 'get_sync' method defined, unable to generate BACI property and switching to interface mode...");
			return false;
		}

		// find get_async method
		IdlObject cbType = null;
		
		if (ptype != null)
		{
			ptype.reset();
			while (ptype.end() != true)
			{
				if (ptype.current().kind() == IdlType.e_operation)
				{
					if (ptype.current().name().equals("get_async"))
					{
						IdlObject retObj = ptype.current();
						retObj.reset();
						retObj.next();
						cbType = final_type(retObj.current().current());
						break; 
					}
				}
	
				ptype.next();
			}
		}

		// unable to handle property
		if (cbType == null)
		{
			System.out.println("WARNING: Property " + ((IdlInterface)obj).getId() + " has no 'get_async' method defined, unable to generate BACI property and switching to interface mode...");
			return false;
		}

		File writeIntoAbeans = createDirectory(ABEANS_POSTFIX_DOTLESS, writeInto);
        PrintWriter output = newFile(writeIntoAbeans, obj.name());


		switch (mode)
		{
			case 0:
				translate_bean_interface_property_Ptype(obj, itf, propertyType, cbType, historyHolder, output);
				break;
			case 1:
				translate_bean_interface_property_ROtype(obj, itf, propertyType, output);
				break;
			case 2:
				translate_bean_interface_property_RWtype(obj, itf, propertyType, output);
				break;
		}
		
		output.close();

		return true;

    }

	/**
	 * Translate an property interface for the user.
	 * Property already has knowledge of BACI and Abeans,
	 * also naming convention of P<type>, RO<type>, RW<type>.
	 * Expected single inheritance: TypelessProperty -> P<type> -> RO<type> | RW<type> -> ...
	 * Implementation of  P<type>, RO<type>, RW<type> properties is fixed, any user-added 
	 * methods and attributes should be implemented by inheriting P<type>, RO<type> | RW<type> types.
	 *
	 * @param obj a property interface to translate
	 * @param writeInto the directory where the interface must be defined
	 * @param inheritanceList non-recursive (direct) inheritance list of the interface
	 * @return	<code>true</code> if interface was handled by this method, <code>false</code> otherwise
	 */
	private boolean translate_bean_interface_callback(IdlObject obj, File writeInto, List inheritanceList)
	{
	
		// it always has at least one... 
		IdlInterface itf = (IdlInterface)inheritanceList.get(0);
		if (itf.isForward())
			itf = itf.getInterface();
	
		// if null, CB implements CBvoid	
		IdlObject callbackType = findCallbackType(obj);

		int callback = INVALID_PROPERTY;
		
		if (callbackType != null)
			callback = mapProperty(callbackType, null, 0);

		String baseName = obj.name()+"Impl";

		File writeIntoAbeans = createDirectory(ABEANS_POSTFIX_DOTLESS, writeInto);
	    PrintWriter output = newFile(writeIntoAbeans, baseName);
	
		
		addDescriptiveHeader(output, obj);
		output.println();

		// imports
		output.println("import org.omg.CORBA.ORB;");
		
		output.println("import abeans.core.Root;");
		output.println("import abeans.core.UnableToInstallComponentException;");

		output.println("import abeans.engine.Request;");
		output.println("import abeans.engine.ResponseType;");
		output.println("import abeans.pluggable.acs.CORBAService;");
		output.println("import abeans.pluggable.acs.DefaultCORBAService;");

		output.println("import alma.ACS.CBDescOut;");
		output.println("import alma.ACSErr.Completion;");
		output.println("import alma.ACSErr.abeans.CompletionHelper;");

		output.println("import " + fullname_CORBA(obj)+ ";");
		output.println("import " + fullname(obj)+ "Helper;");

		output.println("import abeans.models.acs.baci.util.async.HandleDataStore;");
		output.println("import abeans.models.acs.baci.util.async.RequestInfoEntry;");
		
		output.println("import abeans.models.acs.baci.util.async.CallbackHandler;");
		output.println("import abeans.models.acs.baci.util.async.CallbackImplementation;");


		output.println();
		
		// definition
		output.print("public final class " + baseName + " extends " + fullname_CORBA(obj) + "POA implements CallbackImplementation {");
		output.println();
		

		output.println(tab1 + "/**");
		output.println(tab1 + " * This object singleton instance.");
		output.println(tab1 + " */");
		output.println(tab1 + "private static " + baseName + " instance;");
		output.println();
		output.println(tab1 + "/**");
		output.println(tab1 + " * Callback CORBA instance.");
		output.println(tab1 + " */");
		output.println(tab1 + "private " + obj.name() + " callback;");
		output.println();
		output.println(tab1 + "/**");
		output.println(tab1 + "* Callback instance.");
		output.println(tab1 + "*/");
		output.println(tab1 + "private alma.ACS.abeans.Callback abeansCallback;");
		output.println();
		
		output.println(tab1 + "/**");
		output.println(tab1 + "* Fast CB ID lookup ADT holding request listeners.");
		output.println(tab1 + "*/");
		output.println(tab1 + "private HandleDataStore requests;");
		output.println();
		
		output.println(tab1 + "/**");
		output.println(tab1 + " * Private contstructor."); 
		output.println(tab1 + " */");
		output.println(tab1 + "private " + baseName + "()");
		output.println(tab1 + "{");
		output.println(tab2 + "ORB orb = null;");
		output.println(tab2 + "try");
		output.println(tab2 + "{");
		output.println(tab3 + "if (Root.getComponentManager().getComponent(CORBAService.class) == null) Root.getComponentManager().installComponent(DefaultCORBAService.class);");
		output.println(tab3 + "orb = ((DefaultCORBAService)Root.getComponentManager().getComponent(CORBAService.class)).getORB();");
		output.println(tab2 + "}");
		output.println(tab2 + "catch (UnableToInstallComponentException uic) {}");
		output.println();
		output.println(tab2 + "// create CORBA servant");
		output.println(tab2 + "callback = this._this(orb);");
		output.println(tab2 + "abeansCallback = " + obj.name() + "Helper.fromCORBA(callback);");
		output.println();
		output.println(tab2 + "requests = new HandleDataStore();");
		output.println(tab2 + "}");
		output.println();
	
		output.println(tab1 + "/**");
		output.println(tab1 + " * Returns, if necessary also creates, instance of this class.");
		output.println(tab1 + " * This methods follows singleton pattern.");
		output.println(tab1 + " * @return instance of this class.");
		output.println(tab1 + " */");
		output.println(tab1 + "public synchronized static " + baseName + " getInstance()");
		output.println(tab1 + "{");
		output.println(tab2 + "	if (instance == null)");
		output.println(tab3 + "		instance = new " + baseName + "();");
		output.println(tab2 + "	return instance;");
		output.println(tab1 + "}");
		output.println();

		output.println(tab1 + "/**");
		output.println(tab1 + " * Returns callback instance.");
		output.println(tab1 + " * @return callback instance.");
		output.println(tab1 + " */");
		output.println(tab1 + "public alma.ACS.abeans.Callback getCallback()");
		output.println(tab1 + "{");
		output.println(tab2 + "return abeansCallback;");
		output.println(tab1 + "}");
		output.println();

		output.println(tab1 + "/**");
		output.println(tab1 + " * @see abeans.models.acs.baci.util.async.CallbackRequestManager#deregisterCallbackRequest(int)");
		output.println(tab1 + " */");
		output.println(tab1 + "public void deregisterCallbackRequest(int requestId)");
		output.println(tab1 + "{");
		output.println(tab2 + "RequestInfoEntry rie = lookup(requestId);");
		output.println(tab2 + "synchronized (requests)");
		output.println(tab2 + "{");
		output.println(tab3 + "requests.deallocate(requestId);");
		output.println(tab2 + "}");
		output.println();
		output.println(tab2 + "// notification");
		output.println(tab2 + "rie.callbackHandler.deregistered(requestId, rie.request, this);");
		output.println(tab1 + "}");
		output.println();
		
		output.println(tab1 + "/**");
		output.println(tab1 + " * @see abeans.models.acs.baci.util.async.CallbackRequestManager#registerCallbackRequest(abeans.engine.Request, abeans.models.acs.baci.util.async.CallbackHandler)");
		output.println(tab1 + " */");
		output.println(tab1 + "public int registerCallbackRequest(Request request, CallbackHandler callbackHandler)");
		output.println(tab1 + "{");
		output.println(tab2 + "int requestId;");
		output.println(tab2 + "synchronized (requests)");
		output.println(tab2 + "{");
		output.println(tab3 + "requestId = requests.allocate();");
		output.println(tab3 + "if (requestId != 0)");
		output.println(tab4 + "requests.set(requestId, new RequestInfoEntry(request, callbackHandler));");
		output.println(tab2 + "}");
		output.println(tab2 + "// notification");
		output.println(tab2 + "callbackHandler.registered(requestId, request, this);");
		output.println(tab2 + "return requestId;");
		output.println(tab1 + "}");
		output.println();

		output.println(tab1 + "/**");
		output.println(tab1 + " */");
		output.println(tab1 + "private RequestInfoEntry lookup(int requestId)");
		output.println(tab1 + "{");
		output.println(tab2 + "synchronized (requests)");
		output.println(tab2 + "{");
		output.println(tab2 + "if (requests.isAllocated(requestId))");
		output.println(tab3 + "return (RequestInfoEntry)requests.get(requestId);");
		output.println(tab2 + "else");
		output.println(tab3 + "return null;");
		output.println(tab2 + "}");
		output.println(tab1 + "}");
		output.println();

		output.println(tab1 + "/**");
		output.println(tab1 + " * NOTE: callbacks are oneway, so exceptions here will not harm the server");
		if (callbackType != null)
			output.println(tab1 + " * @see alma.ACS.CBdoubleOperations#working(" + translate_type_CORBA(callbackType) + ", alma.ACSErr.Completion, alma.ACS.CBDescOut)");
		else
			output.println(tab1 + " * @see alma.ACS.CBdoubleOperations#working(alma.ACSErr.Completion, alma.ACS.CBDescOut)");
		output.println(tab1 + " */");
		if (callbackType != null)
			output.println(tab1 + "public void working(" + translate_type_CORBA(callbackType) + " value, Completion completion, CBDescOut desc)");
		else
			output.println(tab1 + "public void working(Completion completion, CBDescOut desc)");
		output.println(tab1 + "{");
		output.println(tab2 + "RequestInfoEntry requestInfo = lookup(desc.id_tag);");
		output.println(tab2 + "if (requestInfo != null)");
		output.println(tab2 + "{");
		if (callbackType != null)
			output.println(tab3 + "requestInfo.callbackHandler.working(desc.id_tag, " + translate_to_object(callbackType, "value") + ", CompletionHelper.fromCORBA(completion),");
		else
			output.println(tab3 + "requestInfo.callbackHandler.working(desc.id_tag, null, CompletionHelper.fromCORBA(completion),");
		output.println(tab5 + "requestInfo.request, this);");
		output.println(tab2 + "}");
		output.println(tab1 + "}");
		output.println();

		output.println(tab1 + "/**");
		output.println(tab1 + " * NOTE: callbacks are oneway, so exceptions here will not harm the server");
		if (callbackType != null)
			output.println(tab1 + " * @see alma.ACS.CBdoubleOperations#done(" + translate_type_CORBA(callbackType) + ", alma.ACSErr.Completion, alma.ACS.CBDescOut)");
		else
			output.println(tab1 + " * @see alma.ACS.CBdoubleOperations#done(alma.ACSErr.Completion, alma.ACS.CBDescOut)");
		output.println(tab1 + " */");
		if (callbackType != null)
			output.println(tab1 + "public void done(" + translate_type_CORBA(callbackType) + " value, Completion completion, CBDescOut desc)");
		else
			output.println(tab1 + "public void done(Completion completion, CBDescOut desc)");
		output.println(tab1 + "{");
		output.println(tab2 + "RequestInfoEntry requestInfo = lookup(desc.id_tag);");
		output.println(tab2 + "if (requestInfo != null)");
		output.println(tab2 + "{");
		if (callbackType != null)
			output.println(tab3 + "requestInfo.callbackHandler.done(desc.id_tag, " + translate_to_object(callbackType, "value") + ", CompletionHelper.fromCORBA(completion),");
		else
			output.println(tab3 + "requestInfo.callbackHandler.done(desc.id_tag, null, CompletionHelper.fromCORBA(completion),");
		output.println(tab5 + "requestInfo.request, this);");
		output.println(tab2 + "}");
		output.println(tab1 + "}");
		output.println();

		output.println(tab1 + "/**");
		output.println(tab1 + " * @see alma.ACS.CallbackOperations#negotiate(long, alma.ACS.CBDescOut)");
		output.println(tab1 + " */");
		output.println(tab1 + "public boolean negotiate(long time, CBDescOut desc)");
		output.println(tab1 + "{");
		output.println(tab2 + "return false;");
		output.println(tab1 + "}");
		output.println();

		output.println(tab1 + "/**");
		output.println(tab1 + " * @see abeans.models.acs.baci.util.async.CallbackImplementation#getResponseType()");
		output.println(tab1 + " */");
		output.println(tab1 + "public ResponseType getResponseType() {");
		if (callbackType != null)
			output.println(tab2 + "return ResponseType." + mapPropertyToResponseType(callback) + ";");
		else
			output.println(tab2 + "return ResponseType.VOID;");
		output.println(tab1 + "}");
		output.println();


		//
		// translate the internal definitions
		// 
	
		obj.reset();
	
		while (obj.end() != true)
		{
			if (!(obj.current().name().equals("working") || obj.current().name().equals("done")))
				switch (obj.current().kind())
				{
		
				case IdlType.e_operation :
					translate_operation_callback(obj.current(), output);
					break;
		
				/*
				case IdlType.e_attribute :
					translate_attribute(obj.current(), output);
					break;
				*/
				}
	
			obj.next();
		}


		output.println("}");
		output.close();
	
		return true;
	
	}

	/**
	 * @param obj
	 * @return
	 */
	private static IdlInterface findPtypeInterface(IdlObject obj) {
		
		List fullInheritanceList = getInheritanceList(obj, new ArrayList(), true);
		
		// first find BASE_PROPERTY_ID
		int i = fullInheritanceList.size()-1;
		for (; i >=0; i--)
			if (((IdlInterface)fullInheritanceList.get(i)).getId().equals(BASE_PROPERTY_ID))
				break;
				
		// then find first P<type> property
		for (; i >=0; i--)
			if (((IdlInterface)fullInheritanceList.get(i)).name().charAt(0) == 'P')
			{
				return (IdlInterface)fullInheritanceList.get(i);
			}
			
		return null;
	}
	
	/**
	 * @param obj
	 * @return
	 */
	private static IdlObject findCallbackType(IdlObject obj) {
		
		List fullInheritanceList = getInheritanceList(obj, new ArrayList(), true);
		
		// first find BASE_CALLBACK_ID
		int i = fullInheritanceList.size()-1;
		for (; i >=0; i--)
		{
			IdlInterface cbif = (IdlInterface)fullInheritanceList.get(i);
				
			// then find done method
			cbif.reset();
			while (cbif.end() != true)
			{
				if (cbif.current().kind() == IdlType.e_operation)
				{
					// TODO it is possible that callback has 2 
					if (cbif.current().name().equals("done"))
					{
						IdlObject retObj = cbif.current();
						retObj.reset();
						// skip retVal
						retObj.next();
						retObj.current().reset();
						
						IdlObject firstParam = final_type(retObj.current().current());
						
						// "void" check 
						if (firstParam.kind() == IdlType.e_struct && firstParam.name().equals("Completion"))
							return null;
							
						return firstParam;
					}
				}
		
				cbif.next();
			}
			
		}	
			
		// should never happen	
		return null;
	}

	// property constants
	private static final int INVALID_PROPERTY = -1;
	private static final int DOUBLE_PROPERTY = 0;
	private static final int LONG_PROPERTY   = 1;
	private static final int STRING_PROPERTY = 2;
	private static final int OBJECT_PROPERTY = 3;
	private static final int DOUBLE_SEQ_PROPERTY = 4;
	private static final int LONG_SEQ_PROPERTY   = 5;
	private static final int STRING_SEQ_PROPERTY = 6;
	private static final int OBJECT_SEQ_PROPERTY = 7;

	private static final int PATTERN_PROPERTY = 8;

	/**
	 * @param obj
	 * @param output
	 * @param sequenceDepth
	 */
	private static String mapPropertyToType(int type, boolean toObject)
	{
		return mapPropertyToType(type, toObject, false, false);
	}
	
	/**
	 * @param obj
	 * @param output
	 * @param useAbeansName
	 * @param sequenceDepth
	 */
	private static String mapPropertyToType(int type, boolean toObject, boolean useAbeansName)
	{
		return mapPropertyToType(type, toObject, useAbeansName, false);
	}

	/**
	 * @param type
	 * @param toObject
	 * @param useAbeansName
	 * @param stripObject
	 * @return
	 */
	private static String mapPropertyToType(int type, boolean toObject, boolean useAbeansName, boolean stripObject)
	{
		switch (type)
		{
			case DOUBLE_PROPERTY:
				if (!toObject)
					return "double";
				else
					return "Double";
					
			case LONG_PROPERTY:
				if (!toObject)
					return "long";
				else
					return "Long";
						
			case PATTERN_PROPERTY:
				if (!useAbeansName)
				{
					if (!toObject)
						return "java.util.BitSet";
					else
						return "java.util.BitSet";
				}
				else
				{
					if (stripObject)
						return "";
					else
						return "Pattern";
				}

			case STRING_PROPERTY:
					return "String";
				
			case OBJECT_PROPERTY:
					if (!stripObject)
						return "Object";
					else
						return "";

			case DOUBLE_SEQ_PROPERTY:
					if (!useAbeansName)
						return "double[]";
					else
						return "DoubleSeq";
					
			case LONG_SEQ_PROPERTY:
					if (!useAbeansName)
						return "long[]";
					else
						return "LongSeq";
						
			case STRING_SEQ_PROPERTY:
					if (!useAbeansName)
						return "String[]";
					else
						return "StringSeq";

			case OBJECT_SEQ_PROPERTY:
					if (!useAbeansName)
						//return "Object[]";
						// arrays map to java.lang.Object
						return "Object";
					else
						if (!stripObject)
							//return "ObjectSeq";
							return "Object";
						else
							return "";
		}

		return "<invalid>";		
	}

	/**
	 * @param type
	 * @return
	 */
	private static boolean isNumbericScalarProperty(int type)
	{
		return (type == DOUBLE_PROPERTY || type == LONG_PROPERTY);	
	}

	/**
	 * @param type
	 * @return
	 */
	private static boolean isSequenceProperty(int type)
	{
		return (type == DOUBLE_SEQ_PROPERTY || type == LONG_SEQ_PROPERTY ||
				type == STRING_SEQ_PROPERTY || type == OBJECT_SEQ_PROPERTY);
	} 
	
	/**
	 * @param type
	 * @return
	 */
	private static int mapPropertyToNonSeqType(int type)
	{
		switch (type)
		{
			case DOUBLE_PROPERTY:
			case LONG_PROPERTY:
			case STRING_PROPERTY:
			case OBJECT_PROPERTY:
			case PATTERN_PROPERTY:
				return type;

			case DOUBLE_SEQ_PROPERTY:
				return DOUBLE_PROPERTY;
					
			case LONG_SEQ_PROPERTY:
				return LONG_PROPERTY;
						
			case STRING_SEQ_PROPERTY:
				return STRING_PROPERTY;

			case OBJECT_SEQ_PROPERTY:
				return OBJECT_PROPERTY;
		}

		return type;		
	}

	/**
	 * @param type
	 * @return ResponseType type
	 */
	private static String mapPropertyToResponseType(int type)
	{
		switch (type)
		{
			case DOUBLE_PROPERTY:
				return "DOUBLE";
			case LONG_PROPERTY:
				return "LONG";
			case STRING_PROPERTY:
				return "STRING";
			case OBJECT_PROPERTY:
			case PATTERN_PROPERTY:
				return "OBJECT";

			case DOUBLE_SEQ_PROPERTY:
				return "DOUBLE_SEQ";
					
			case LONG_SEQ_PROPERTY:
				return "LONG_SEQ";
						
			case STRING_SEQ_PROPERTY:
				return "STRING_SEQ";

			case OBJECT_SEQ_PROPERTY:
				//return "OBJECT_SEQ";
				return "OBJECT";
		}

		return "<invalid>";		
	}

	/**
	 * @param obj
	 * @param output
	 * @param sequenceDepth
	 */
	//TODO mapping is wrong for non-exact primitive types !!!!
	// never ie get<Double|Long|String|...>..., but use getObject adn handle casts itself...
	private static int mapProperty(IdlObject obj, PrintWriter output, int sequenceDepth)
	{
		if (sequenceDepth > 1)
		{
			if (output!=null) output.println("abeans.datatypes.ObjectProperty");
			return OBJECT_PROPERTY;
		}
		
		switch (obj.kind())
		{
			case IdlType.e_simple :
				IdlSimple simple = (IdlSimple) obj;

				switch (simple.internal())
				{

				// invalid
				case Token.t_void :
					if (output!=null) output.println("<invalid>");
					return INVALID_PROPERTY;

				case Token.t_float :
				case Token.t_double :
					if (sequenceDepth == 0)
					{
						if (output!=null) output.println("abeans.datatypes.DoubleProperty");
						return DOUBLE_PROPERTY;
					}
					else
					{ 
						if (output!=null) output.println("abeans.datatypes.DoubleSeqProperty");
						return DOUBLE_SEQ_PROPERTY;
					}

				case Token.t_short :
				case Token.t_ushort :

				case Token.t_long :
				case Token.t_ulong :

				case Token.t_longlong :
				case Token.t_ulonglong :

				case Token.t_octet :
					if (sequenceDepth == 0)
					{
						if (output!=null) output.println("abeans.datatypes.LongProperty");
						return LONG_PROPERTY;
					}
					else
					{ 
						if (output!=null) output.println("abeans.datatypes.LongSeqProperty");
						return LONG_SEQ_PROPERTY;
					}

				case Token.t_boolean :
				case Token.t_char :
				case Token.t_wchar :
					// it is supported, but only as an Object
					// <object>.<double|long>Value() should exists for easy mapping
					if (sequenceDepth == 0)
					{
						if (output!=null) output.println("abeans.datatypes.ObjectProperty");
						return OBJECT_PROPERTY;
					}
					else
					{
						//if (output!=null) output.println("abeans.datatypes.ObjectSeqProperty");
						if (output!=null) output.println("abeans.datatypes.ObjectProperty");
						return OBJECT_SEQ_PROPERTY;
					}

				/*
					if (output!=null) output.println("<invalid>");
					return INVALID_PROPERTY;
					
					// boolean
					--> long
					
					// char
					if (sequenceDepth == 0)
					{
						if (output!=null) output.println("abeans.datatypes.StringProperty");
						return STRING_PROPERTY;
					}
					else
					{
						if (output!=null) output.println("abeans.datatypes.StringSeqProperty");
						return STRING_SEQ_PROPERTY;
					}
				*/

				case Token.t_any :
				case Token.t_typecode :
				case Token.t_object :
				case Token.t_ValueBase :
					if (sequenceDepth == 0)
					{
						if (output!=null) output.println("abeans.datatypes.ObjectProperty");
						return OBJECT_PROPERTY;
					}
					else
					{
						//if (output!=null) output.println("abeans.datatypes.ObjectSeqProperty");
						if (output!=null) output.println("abeans.datatypes.ObjectProperty");
						return OBJECT_SEQ_PROPERTY;
					}
				}

				break;

			case IdlType.e_fixed :
				if (sequenceDepth == 0)
				{
					if (output!=null) output.println("abeans.datatypes.ObjectProperty");
					return OBJECT_PROPERTY;
				}
				else
				{
					//if (output!=null) output.println("abeans.datatypes.ObjectSeqProperty");
					if (output!=null) output.println("abeans.datatypes.ObjectProperty");
					return OBJECT_SEQ_PROPERTY;
				}

			case IdlType.e_string :
			case IdlType.e_wstring :
				if (sequenceDepth == 0)
				{
					if (output!=null) output.println("abeans.datatypes.StringProperty");
					return STRING_PROPERTY;
				}
				else
				{
					if (output!=null) output.println("abeans.datatypes.StringSeqProperty");
					return STRING_SEQ_PROPERTY;
				}

			case IdlType.e_struct :
			case IdlType.e_union :
			case IdlType.e_enum :

			case IdlType.e_interface :
			case IdlType.e_forward_interface :

			case IdlType.e_exception :

			case IdlType.e_value :
			case IdlType.e_forward_value :
				if (sequenceDepth == 0)
				{
					if (output!=null) output.println("abeans.datatypes.ObjectProperty");
					return OBJECT_PROPERTY;
				}
				else
				{
					//if (output!=null) output.println("abeans.datatypes.ObjectSeqProperty");
					if (output!=null) output.println("abeans.datatypes.ObjectProperty");
					return OBJECT_SEQ_PROPERTY;
				}

			case IdlType.e_sequence :
			case IdlType.e_array :
				obj.reset();
				return mapProperty(obj.current(), output, sequenceDepth+1);

			case IdlType.e_typedef :
				obj.reset();
				return mapProperty(obj.current(), output, sequenceDepth);

			case IdlType.e_ident :
				return mapProperty(((IdlIdent) obj).internalObject(), output, sequenceDepth);

			// TODO check this... well it is not supported
			case IdlType.e_native :
			case IdlType.e_value_box :
				if (sequenceDepth == 0)
				{
					if (output!=null) output.println("abeans.datatypes.ObjectProperty");
					return OBJECT_PROPERTY;
				}
				else
				{
					//if (output!=null) output.println("abeans.datatypes.ObjectSeqProperty");
					if (output!=null) output.println("abeans.datatypes.ObjectProperty");
					return OBJECT_SEQ_PROPERTY;
				}

		}

		return INVALID_PROPERTY;
	}

	/**
	 * Translates Ptype property interface.
	 * @param obj
	 * @param itf
	 * @param propertyType
	 * @param output
	 */
	private void translate_bean_interface_property_Ptype(IdlObject obj,	IdlInterface itf,
														 IdlObject propertyType, IdlObject cbType,
														 String historyHolder, PrintWriter output)
	{
			
		addDescriptiveHeader(output, obj);
		output.println();

		// imports
		output.println("import abeans.models.acs.baci.Characteristics;");
		output.println("import abeans.models.acs.baci.TypelessProperty;");

		output.println("import abeans.datatypes.AbeansDataExchangeException;");
		output.println("import abeans.datatypes.CharacteristicContextUtilities;");
		output.println("import abeans.datatypes.DynamicValueUtilities;");
		output.println("import abeans.engine.ResponseType;");
		output.println("import abeans.engine.Request;");

		output.println("import com.cosylab.datatypes.*;");

		int property = mapProperty(propertyType, null, 0);
		// TODO not nice, pattern is a special case
		if (obj.name().startsWith("Ppattern"))
			property = PATTERN_PROPERTY;

		int primitiveType = mapPropertyToNonSeqType(property);

		// since there is no ObjectSeqProperty
		if (isSequenceProperty(property) && property != OBJECT_SEQ_PROPERTY)
			output.println("import com.cosylab.datatypes.ntuples.*;");
		
		if (property == PATTERN_PROPERTY)
			output.println("import com.cosylab.datatypes.Condition;");
					
		output.println();
		
		// definition
		// output.print("public abstract class " + obj.name() + " extends TypelessProperty implements ");
		// proxies does not work with abstract classes
		output.print("public class " + obj.name() + " extends TypelessProperty implements ");
		
		if (property == PATTERN_PROPERTY)
			output.println("abeans.datatypes.PatternProperty");
		else
			mapProperty(propertyType, output, 0);
		
		output.println("{");
		output.println();
		
		// number access for number properties
		if (isNumbericScalarProperty(property))
		{
			output.println(tab1 + "/**");
			output.println(tab1 + " * Number access implementation of this property (lazy initialization).");
			output.println(tab1 + " */");
			output.println(tab1 + "private NumberAccess na = null;");
			output.println();
		}
		
		// default value
		output.println(tab1 + "/**");
		output.println(tab1 + " * Property default value"); 
		output.println(tab1 + " */");
		output.print(tab1 + "private " + mapPropertyToType(property, false) + " defaultValue = ");
		if (isNumbericScalarProperty(property))
			output.println("0;");
		else
			output.println("null;");
		output.println();
		
		// constructors
		translate_bean_interface_property_constructors(obj, output);
		
		output.println(tab1 + "/**");
		output.println(tab1 + " * @see abeans.datatypes." + mapPropertyToType(property, true, true) + "Property#getAsynchronous()");
		output.println(tab1 + " */");
		output.println(tab1 + "public Request getAsynchronous() throws AbeansDataExchangeException {");
		output.println(tab2 + "return DynamicValueUtilities.getDynamicValueAsynchronous(getRemoteInfo(), remote, getDefaultTimeout(), interceptor, this, ResponseType." + mapPropertyToResponseType(property) + ");");
		output.println(tab1 + "}");
		output.println();

		output.println(tab1 + "/**");
		output.println(tab1 + " * @see abeans.datatypes." + mapPropertyToType(property, true, true) + "#setAsynchronous(" + mapPropertyToType(property, false) + ")");
		output.println(tab1 + " */");
		output.println(tab1 + "public Request setAsynchronous(" + mapPropertyToType(property, false) + " value) throws AbeansDataExchangeException {");
		output.println(tab2 + "throw new AbeansDataExchangeException(this, \"Setting a value is not supported by a readonly property.\");");
		output.println(tab1 + "}");
		output.println();

		output.println(tab1 + "/**");
		output.println(tab1 + " * @see com.cosylab.datatypes." + mapPropertyToType(property, true, true) + "Access#getLatestReceivedValue()");
		output.println(tab1 + " */");
		output.println(tab1 + "public " + mapPropertyToType(property, false) + " getLatestReceivedValue() {");
		output.println(tab2 + "Object drv = getLatestReceivedValueAsObject();");
		output.println(tab2 + "if (drv == null)");
		output.println(tab3 + "return defaultValue;");
		output.println(tab2 + "else");
		//if (isNumbericScalarProperty(property) || isNumbericScalarProperty(primitiveType))
			//output.println(tab3 + "return " + cast_to_abeans_type(propertyType, "drv") + ";");
		if (isNumbericScalarProperty(property))
			output.println(tab3 + "return ((" + mapPropertyToType(property, true, true) + ")drv)." + mapPropertyToType(property, false) + "Value();");
		else	// objects
			output.println(tab3 + "return (" + mapPropertyToType(property, true) + ")drv;");
		output.println(tab1 + "}");
		output.println();

		output.println(tab1 + "/**");
		output.println(tab1 + " * @see com.cosylab.datatypes." + mapPropertyToType(property, true, true) + "Access#getValue()");
		output.println(tab1 + " */");
		output.println(tab1 + "public " + mapPropertyToType(property, false) + " getValue() throws DataExchangeException {");
		//if (isNumbericScalarProperty(property) || isNumbericScalarProperty(primitiveType))
		//	output.println(tab2 + "return " + cast_to_abeans_type(propertyType, "DynamicValueUtilities.getDynamicValue(getRemoteInfo(), remote, getDefaultTimeout(), interceptor, this)") + ";");
		//else	// Object / ObjectSeq
		if (property == PATTERN_PROPERTY)
			// cast is missing
			output.println(tab2 + "return (java.util.BitSet)DynamicValueUtilities.get" + mapPropertyToType(property, true, true, true) + "DynamicValue(getRemoteInfo(), remote, getDefaultTimeout(), interceptor, this);");
		else
			output.println(tab2 + "return DynamicValueUtilities.get" + mapPropertyToType(property, true, true, true) + "DynamicValue(getRemoteInfo(), remote, getDefaultTimeout(), interceptor, this);");
		output.println(tab1 + "}");
		output.println();

		output.println(tab1 + "/**");
		output.println(tab1 + " * @see com.cosylab.datatypes." + mapPropertyToType(property, true, true) + "Access#setValue(" + mapPropertyToType(property, false) + ")");
		output.println(tab1 + " */");
		output.println(tab1 + "public void setValue(" + mapPropertyToType(property, false) + " value) throws DataExchangeException {");
		output.println(tab2 + "throw new DataExchangeException(this, \"Setting a value is not supported by a readonly property.\");");
		output.println(tab1 + "}");
		output.println();

		output.println(tab1 + "/**");
		output.println(tab1 + " * @see com.cosylab.datatypes.AbstractProperty#getAccessTypes()");
		output.println(tab1 + " */");
		output.println(tab1 + "public Class[] getAccessTypes() {");
		if (isNumbericScalarProperty(property))
			output.println(tab2 + "return new Class[] { " + mapPropertyToType(property, true, true) + "Access.class, NumberAccess.class };");
		else
			output.println(tab2 + "return new Class[] { " + mapPropertyToType(property, true, true) + "Access.class };");
		output.println(tab1 + "}");
		output.println();

		output.println(tab1 + "/**");
		output.println(tab1 + " * @see com.cosylab.datatypes.AbstractProperty#getDataAccess(java.lang.Class)");
		output.println(tab1 + " */");
		output.println(tab1 + "public DataAccess getDataAccess(Class type) throws IllegalViewException {");
		output.println(tab2 + "if (type == " + mapPropertyToType(property, true, true) + "Access.class)");
		output.println(tab2 + "{");
		output.println(tab3 + "return this;");
		output.println(tab2 + "}");
		if (isNumbericScalarProperty(property))
		{
			output.println(tab2 + "else if (type == NumberAccess.class)");
			output.println(tab2 + "{");
			output.println(tab3 + "if (na == null) na = abeans.datatypes.NumberAccessUtilities.createNumberDataAccess(getRemoteInfo(), remote, getDefaultTimeout(), interceptor, this);");
			output.println(tab3 + "return na;");
			output.println(tab2 + "}");
		}
		output.println(tab2 + "else");
		output.println(tab2 + "{");
		output.println(tab3 + "IllegalViewException ive = new IllegalViewException(this, \"Requested data access is not supported.\");");
		output.println(tab3 + "ive.putValue(\"type\", type);");
		output.println(tab3 + "throw ive;");
		output.println(tab2 + "}");
		output.println(tab1 + "}");
		output.println();

		output.println(tab1 + "/**");
		output.println(tab1 + "* @see com.cosylab.datatypes.DataAccess#getDataType()");
		output.println(tab1 + "*/");
		output.println(tab1 + "public Class getDataType() {");
		if (property == OBJECT_PROPERTY || property == OBJECT_SEQ_PROPERTY)
			output.println(tab2 + "return " + translate_type(propertyType) + ".class;");
		else if (isNumbericScalarProperty(property))
			output.println(tab2 + "return " + mapPropertyToType(property, true) + ".TYPE;");
		else
			output.println(tab2 + "return " + mapPropertyToType(property, true) + ".class;");
		output.println(tab1 + "}");
		output.println();

		output.println(tab1 + "/**");
		output.println(tab1 + " * @see com.cosylab.datatypes.AbstractProperty#getDefaultDataAccess()");
		output.println(tab1 + " */");
		output.println(tab1 + "public DataAccess getDefaultDataAccess() {");
		output.println(tab2 + "return this;");
		output.println(tab1 + "}");
		output.println();

		output.println(tab1 + "/**");
		output.println(tab1 + " * @see com.cosylab.datatypes.DataAccess#isSettable()");
		output.println(tab1 + " */");
		output.println(tab1 + "public boolean isSettable() {");
		output.println(tab2 + "return false;");
		output.println(tab1 + "}");
		output.println();

		output.println(tab1 + "/**");
		output.println(tab1 + " * @see abeans.models.acs.baci.TypelessProperty#newProxySet()");
		output.println(tab1 + " */");
		output.println(tab1 + "protected void newProxySet() {");
		output.println(tab2 + "super.newProxySet();");
		output.println(tab2 + "if (remote!=null && remoteInfo!=null)");
		output.println(tab2 + "{");
		output.println(tab3 + "try {");

		//if (isNumbericScalarProperty(property) || isNumbericScalarProperty(primitiveType))
		//	output.println(tab4 + "defaultValue = " + cast_to_abeans_type(propertyType, "CharacteristicContextUtilities.getCharacteristic(getRemoteInfo(), Characteristics.NAME_DEFAULT_VALUE, remote, getDefaultTimeout(), interceptor, this)") + ";");
		
		if (isNumbericScalarProperty(property))
			output.println(tab4 + "defaultValue = CharacteristicContextUtilities.get" + mapPropertyToType(property, true, true, true) + "Characteristic(getRemoteInfo(), Characteristics.NAME_DEFAULT_VALUE, remote, getDefaultTimeout(), interceptor, this);");
		else // Object / Object[]
			output.println(tab4 + "defaultValue = (" + mapPropertyToType(property, false) + ")CharacteristicContextUtilities.getCharacteristic(getRemoteInfo(), Characteristics.NAME_DEFAULT_VALUE, remote, getDefaultTimeout(), interceptor, this);");
			
		output.println(tab3 + "} catch (Exception e) {");
		if (isNumbericScalarProperty(property))
			output.println(tab4 + "defaultValue = 0;");
		else
			output.println(tab4 + "defaultValue = null;");
		output.println(tab3 + "}");
		output.println(tab2 + "}");
		output.println(tab1 + "}");
		output.println();

		if (isSequenceProperty(property))
		{
			output.println(tab1 + "/**");
			output.println(tab1 + " * @see com.cosylab.datatypes.ntuples.SequenceAccess#getSequenceLength()");
			output.println(tab1 + " */");
			output.println(tab1 + "public int getSequenceLength() throws DataExchangeException {");
			output.println(tab2 + mapPropertyToType(property, false) + " val = getLatestReceivedValue();");
			output.println(tab2 + "if (val!=null)");
			if (property == OBJECT_SEQ_PROPERTY)
				output.println(tab3 + "return java.lang.reflect.Array.getLength(getLatestReceivedValue());");
			else
				output.println(tab3 + "return getLatestReceivedValue().length;");
			output.println(tab2 + "else");
			output.println(tab3 + "return 0;");
			output.println(tab1 + "}");
		}

		output.println(tab1 + "/**");
		output.println(tab1 + "* Returns property default value.");
		output.println(tab1 + "* @return property default value");
		output.println(tab1 + "*/");
		output.println(tab1 + "public " + mapPropertyToType(property, false) + " getDefaultValue() {");
		output.println(tab2 + "return defaultValue;");
		output.println(tab1 + "}");

		//
		// methods to be overriden
		//
		if (isNumbericScalarProperty(primitiveType))
		{
			output.println(tab1 + "/**");
			output.println(tab1 + " * @see com.cosylab.datatypes." + mapPropertyToType(property, true, true) + "AbstractProperty#getMaximum()");
			output.println(tab1 + " */");
			output.println(tab1 + "public " + mapPropertyToType(primitiveType, false) + " getMaximum() throws DataExchangeException {");
			output.println(tab2 + "throw new AbeansDataExchangeException(this, \"Method cannot be implemented by P<type> interface.\");");
			output.println(tab1 + "}");
			output.println();
	
			output.println(tab1 + "/**");
			output.println(tab1 + " * @see com.cosylab.datatypes." + mapPropertyToType(property, true, true) + "AbstractProperty#getMinimum()");
			output.println(tab1 + " */");
			output.println(tab1 + "public " + mapPropertyToType(primitiveType, false) + " getMinimum() throws DataExchangeException {");
			output.println(tab2 + "throw new AbeansDataExchangeException(this, \"Method cannot be implemented by P<type> interface.\");");
			output.println(tab1 + "}");
			output.println();
		}
		
		//
		// metods of pattern property
		//
		if (property == PATTERN_PROPERTY)
		{
			output.println(tab1 + "/**");
			output.println(tab1 + "* @see com.cosylab.datatypes.PatternAbstractProperty#getBitDescriptions()");
			output.println(tab1 + "*/");
			output.println(tab1 + "public String[] getBitDescriptions() throws DataExchangeException {");
			output.println(tab2 + "return (String[])CharacteristicContextUtilities.getCharacteristic(getRemoteInfo(), PatternAbstractProperty.C_BIT_DESCRIPTIONS, remote, getDefaultTimeout(), interceptor, this);");	
			output.println(tab1 + "}");

			output.println(tab1 + "/**");
			output.println(tab1 + " * @see com.cosylab.datatypes.PatternAbstractProperty#getBitMask()");
			output.println(tab1 + " */");
			output.println(tab1 + "public java.util.BitSet getBitMask() throws DataExchangeException {");
			// TODO resolution mapping
			//output.println(tab2 + "return toAbeansType(CharacteristicContextUtilities.getCharacteristic(getRemoteInfo(), PatternAbstractProperty.NAME_BIT_MASK, remote, getDefaultTimeout(), interceptor, this));");
			output.println(tab2 + "java.util.BitSet bitSet = new java.util.BitSet(32);");
			output.println(tab2 + "bitSet.set(0, 32);");
			output.println(tab2 + "return bitSet;");	
			output.println(tab1 + "}");

			output.println(tab1 + "/**");
			output.println(tab1 + " * @see com.cosylab.datatypes.PatternAbstractProperty#getConditionWhenCleared()");
			output.println(tab1 + " */");
			output.println(tab1 + "public Condition[] getConditionWhenCleared() throws DataExchangeException {");
			output.println(tab2 + "return (Condition[])CharacteristicContextUtilities.getCharacteristic(getRemoteInfo(), PatternAbstractProperty.C_CONDITION_WHEN_SET, remote, getDefaultTimeout(), interceptor, this);");	
			output.println(tab1 + "}");

			output.println(tab1 + "/**");
			output.println(tab1 + " * @see com.cosylab.datatypes.PatternAbstractProperty#getConditionWhenSet()");
			output.println(tab1 + " */");
			output.println(tab1 + "public Condition[] getConditionWhenSet() throws DataExchangeException {");
			output.println(tab2 + "return (Condition[])CharacteristicContextUtilities.getCharacteristic(getRemoteInfo(), PatternAbstractProperty.C_CONDITION_WHEN_CLEARED, remote, getDefaultTimeout(), interceptor, this);");	
			output.println(tab1 + "}");
		}
		
		// callback singleton factory
		output.println(tab1 + "/**");
		output.println(tab1 + " * Returns callback implementation.");
		output.println(tab1 + " * @return callback implementation.");
		output.println(tab1 + " */");
		output.println(tab1 + "public abeans.models.acs.baci.util.async.CallbackImplementation getCallback() {");
		output.println(tab2 + "return " + fullname(cbType) + "Impl.getInstance();");
		output.println(tab1 + "}");
		output.println();
		
		output.println(tab1 + "/**");
		output.println(tab1 + " * Converts value to Abeans object.");
		output.println(tab1 + " * @param value	value to be converted to Abeans object.");
		output.println(tab1 + " * @return	Abeans object.");
		output.println(tab1 + " */");
		output.println(tab1 + "public Object toAbeansType(Object value) {");
		if (isNumbericScalarProperty(property) || isNumbericScalarProperty(primitiveType))
		{
			// min, max, etc. support (they are scalars)
			if (isSequenceProperty(property))
			{
				output.println(tab2 + "if (value.getClass().isArray())");
				output.println(tab3 + "return " + cast_to_abeans_type(propertyType, "value", true) + ";");
				output.println(tab2 + "else");
				output.println(tab3 + "return " + cast_to_abeans_type(propertyType, "value", true, true) + ";");
			}
			else
				output.println(tab2 + "return " + cast_to_abeans_type(propertyType, "value", true) + ";");
		}
		// TODO enum monitor fix (arrays are not supported)
		else if (propertyType.kind() == IdlType.e_enum)
		{
			output.println(tab2 + "if (value instanceof Integer)");
			output.println(tab3 + "return " + translate_type(propertyType) + ".from_int(((Integer)value).intValue());");
			output.println(tab2 + "else");
			output.println(tab3 + "return value;");		
		}
		else if (property == PATTERN_PROPERTY)
		{
			output.println(tab2 + "java.util.BitSet bitValue = new java.util.BitSet(32);");
			output.println(tab2 + "if (value instanceof Integer)");
			output.println(tab2 + "{");
			output.println(tab3 + "int intValue = ((Integer)value).intValue();");
			output.println(tab3 + "for (int i = 0; intValue != 0 && i <= 32; i++)");
			output.println(tab3 + "{");
			output.println(tab4 + "if ((intValue & 0x00000001) == 1)");
			output.println(tab5 + "bitValue.set(i);");
			output.println(tab4 + "intValue = intValue >>> 1;");
			output.println(tab3 + "}");
			output.println(tab2 + "}");
			output.println(tab2 + "return bitValue;");
		}
		else // Object / ObjectSeq
			output.println(tab2 + "return value;");		
		output.println(tab1 + "}");
		output.println();

		output.println(tab1 + "/**");
		output.println(tab1 + " * Converts value from Abeans object.");
		output.println(tab1 + " * @param value	value to be converted from Abeans object.");
		output.println(tab1 + " * @return	non-Abeans object.");
		output.println(tab1 + " */");
		output.println(tab1 + "public Object fromAbeansType(Object value) {");
		if (isNumbericScalarProperty(property) || isNumbericScalarProperty(primitiveType))
		{
			output.println(tab2 + "return " + cast_to_object_type(propertyType, "value", true) + ";");
		}
		else if (property == PATTERN_PROPERTY)
		{
			output.println(tab2 + "int intValue = 0;"); 
			output.println(tab2 + "java.util.BitSet bitValue = (java.util.BitSet)value;");
			output.println(tab2 + "if (bitValue != null)");
			output.println(tab2 + "{");
			output.println(tab3 + "for (int i = Math.min(bitValue.length()-1, 31); i >= 0; i--)");
			output.println(tab3 + "{");
			output.println(tab4 + "intValue <<= 1;");
			output.println(tab4 + "if (bitValue.get(i))");
			output.println(tab5 + "intValue++;");
			output.println(tab4 + "}");
			output.println(tab3 + "}");
			output.println(tab2 + "return new Integer(intValue);");
		}
		else // Object / ObjectSeq
			output.println(tab2 + "return value;");		
		output.println(tab1 + "}");
		output.println();
		
		output.println(tab1 + "/**");
		output.println(tab1 + " * Creates new instance of history value holder object.");
		output.println(tab1 + " * @return	new instance of history value holder object.");
		output.println(tab1 + " */");
		output.println(tab1 + "public Object getHistoryValueHolder() {");
		if (historyHolder != null)
			output.println(tab2 + "return new " + historyHolder + "();");
		else
			output.println(tab2 + "return null");
		output.println(tab1 + "}");
		output.println();

		output.println(tab1 + "/**");
		output.println(tab1 + " * Extracts value held by history value holder object.");
		output.println(tab1 + " * @param	history value holder object.");
		output.println(tab1 + " * @return	value held by history value holder object.");
		output.println(tab1 + " */");
		output.println(tab1 + "public Object extractHistoryValueHolder(Object holder) {");
		if (historyHolder != null)
			output.println(tab2 + "return ((" + historyHolder + ")holder).value;");
		else
			output.println(tab2 + "return null");
		output.println(tab1 + "}");
		output.println();

		output.println("}");
	}

	/**
	 * Translates ROtype property interface.
	 * @param obj
	 * @param itf
	 * @param propertyType
	 * @param output
	 */
	private void translate_bean_interface_property_ROtype(IdlObject obj, IdlInterface itf,
													 	  IdlObject propertyType, PrintWriter output)
	{
			
		addDescriptiveHeader(output, obj);

		int property = mapProperty(propertyType, null, 0);
		if (obj.name().startsWith("ROpattern"))
			property = PATTERN_PROPERTY;

		int primitiveType = mapPropertyToNonSeqType(property);

		if (isNumbericScalarProperty(primitiveType))
		{
			// imports
			output.println("import com.cosylab.datatypes.DataExchangeException;");
			//if (!isPattern)
			//{
				output.println("import abeans.datatypes.CharacteristicContextUtilities;");
				output.println("import com.cosylab.datatypes.NumericProperty;");
			//}
			output.println("import java.util.Map;");
		}

		output.println();
		
		// definition
		output.println("public class " + obj.name() + " extends " + fullname(itf));
		
		output.println("{");
		output.println();

		// constructors
		translate_bean_interface_property_constructors(obj, output);

		if (isNumbericScalarProperty(primitiveType))
		{
			output.println(tab1 + "/**");
			output.println(tab1 + " * @see com.cosylab.datatypes." + mapPropertyToType(property, true, true) + "AbstractProperty#getMaximum()");
			output.println(tab1 + " */");
			output.println(tab1 + "public " + mapPropertyToType(primitiveType, false) + " getMaximum() throws DataExchangeException {");
			//output.println(tab2 + "return " + cast_to_abeans_type(propertyType, "CharacteristicContextUtilities.getCharacteristic(getRemoteInfo(), NumericProperty.C_GRAPH_MAX, remote, getDefaultTimeout(), interceptor, this)", true) + ";");
			//if (isPattern)
			//	output.println(tab2 + "return Integer.MAX_VALUE;");	
			//else
				output.println(tab2 + "return CharacteristicContextUtilities.get" + mapPropertyToType(primitiveType, true, true, true) + "Characteristic(getRemoteInfo(), NumericProperty.C_GRAPH_MAX, remote, getDefaultTimeout(), interceptor, this);");	
			output.println(tab1 + "}");
			output.println();
	
			output.println(tab1 + "/**");
			output.println(tab1 + " * @see com.cosylab.datatypes." + mapPropertyToType(property, true, true) + "AbstractProperty#getMinimum()");
			output.println(tab1 + " */");
			output.println(tab1 + "public " + mapPropertyToType(primitiveType, false) + " getMinimum() throws DataExchangeException {");
			//output.println(tab2 + "return " + cast_to_abeans_type(propertyType, "CharacteristicContextUtilities.getCharacteristic(getRemoteInfo(), NumericProperty.C_GRAPH_MIN, remote, getDefaultTimeout(), interceptor, this)", true) + ";");
			//if (isPattern)
			//	output.println(tab2 + "return Integer.MIN_VALUE;");	
			//else
				output.println(tab2 + "return CharacteristicContextUtilities.get" + mapPropertyToType(primitiveType, true, true, true) + "Characteristic(getRemoteInfo(), NumericProperty.C_GRAPH_MIN, remote, getDefaultTimeout(), interceptor, this);");	
			output.println(tab1 + "}");
			output.println();
	
		}
		
		output.println("}");
	}

	/**
	 * Translates RWtype property interface.
	 * @param obj
	 * @param itf
	 * @param propertyType
	 * @param output
	 */
	private void translate_bean_interface_property_RWtype(IdlObject obj, IdlInterface itf, 
														  IdlObject propertyType, PrintWriter output)
	{
		addDescriptiveHeader(output, obj);

		int property = mapProperty(propertyType, null, 0);
		if (obj.name().startsWith("RWpattern"))
			property = PATTERN_PROPERTY;

		int primitiveType = mapPropertyToNonSeqType(property);

		// imports
		output.println("import abeans.datatypes.AbeansDataExchangeException;");
		output.println("import abeans.datatypes.DynamicValueUtilities;");
		output.println("import abeans.engine.Request;");

		output.println("import com.cosylab.datatypes.DataExchangeException;");
		

		if (isNumbericScalarProperty(primitiveType))
		{
			//if (!isPattern)
				output.println("import abeans.datatypes.CharacteristicContextUtilities;");
			output.println("import java.util.Map;");
		}
		
		output.println();
		
		// definition
		output.println("public class " + obj.name() + " extends " + fullname(itf));
		
		output.println("{");
		output.println();

		
		// constructors
		translate_bean_interface_property_constructors(obj, output);
		
		output.println(tab1 + "/**");
		output.println(tab1 + " * @see com.cosylab.datatypes.DataAccess#isSettable()");
		output.println(tab1 + " */");
		output.println(tab1 + "public boolean isSettable() {");
		output.println(tab2 + "return true;");
		output.println(tab1 + "}");
		output.println();

		output.println(tab1 + "/**");
		output.println(tab1 + " * @see abeans.datatypes." + mapPropertyToType(property, true, true) + "#setAsynchronous(" + mapPropertyToType(property, false) + ")");
		output.println(tab1 + " */");
		output.println(tab1 + "public Request setAsynchronous(" + mapPropertyToType(property, false) + " value) throws AbeansDataExchangeException {");
		//if (isNumbericScalarProperty(property) || isNumbericScalarProperty(primitiveType))
		//	output.println(tab2 + "return DynamicValueUtilities.setDynamicValueAsynchronous(getRemoteInfo(), remote, getDefaultTimeout(), interceptor, this, " + cast_to_object_type(propertyType, "value") + ");");
		//else // Object / ObjectSeq
		output.println(tab2 + "return DynamicValueUtilities.set" + mapPropertyToType(property, true, true, true) + "DynamicValueAsynchronous(getRemoteInfo(), remote, getDefaultTimeout(), interceptor, this, value);");		
		output.println(tab1 + "}");
		output.println();

		output.println(tab1 + "/**");
		output.println(tab1 + " * @see com.cosylab.datatypes." + mapPropertyToType(property, true, true) + "Access#setValue(" + mapPropertyToType(property, false) + ")");
		output.println(tab1 + " */");
		output.println(tab1 + "public void setValue(" + mapPropertyToType(property, false) + " value) throws DataExchangeException {");
		//if (isNumbericScalarProperty(property) || isNumbericScalarProperty(primitiveType))
		//	output.println(tab2 + "DynamicValueUtilities.setDynamicValueAsynchronous(getRemoteInfo(), remote, getDefaultTimeout(), interceptor, this, " + cast_to_object_type(propertyType, "value") + ");");
		//else // Object / ObjectSeq
		output.println(tab2 + "DynamicValueUtilities.set" + mapPropertyToType(property, true, true, true) + "DynamicValue(getRemoteInfo(), remote, getDefaultTimeout(), interceptor, this, value);");
			
		output.println(tab1 + "}");
		output.println();

		// TODO
		// increment, decrement, setNonBlocking

		if (isNumbericScalarProperty(primitiveType))
		{
			output.println(tab1 + "/**");
			output.println(tab1 + " * @see com.cosylab.datatypes." + mapPropertyToType(property, true, true) + "AbstractProperty#getMaximum()");
			output.println(tab1 + " */");
			output.println(tab1 + "public " + mapPropertyToType(primitiveType, false) + " getMaximum() throws DataExchangeException {");
			//output.println(tab2 + "return " + cast_to_abeans_type(propertyType, "CharacteristicContextUtilities.getCharacteristic(getRemoteInfo(), NAME_MAXIMUM, remote, getDefaultTimeout(), interceptor, this)", true) + ";");
			//if (isPattern)
			//	output.println(tab2 + "return Integer.MAX_VALUE;");	
			//else
				output.println(tab2 + "return CharacteristicContextUtilities.get" + mapPropertyToType(primitiveType, true, true, true) + "Characteristic(getRemoteInfo(), NAME_MAXIMUM, remote, getDefaultTimeout(), interceptor, this);");	
			output.println(tab1 + "}");
			output.println();
	
			output.println(tab1 + "/**");
			output.println(tab1 + " * @see com.cosylab.datatypes." + mapPropertyToType(property, true, true) + "AbstractProperty#getMinimum()");
			output.println(tab1 + " */");
			output.println(tab1 + "public " + mapPropertyToType(primitiveType, false) + " getMinimum() throws DataExchangeException {");
			//output.println(tab2 + "return " + cast_to_abeans_type(propertyType, "CharacteristicContextUtilities.getCharacteristic(getRemoteInfo(), NAME_MINIMUM, remote, getDefaultTimeout(), interceptor, this)", true) + ";");
			//if (isPattern)
			//	output.println(tab2 + "return Integer.MIN_VALUE;");	
			//else
				output.println(tab2 + "return CharacteristicContextUtilities.get" + mapPropertyToType(primitiveType, true, true, true) + "Characteristic(getRemoteInfo(), NAME_MINIMUM, remote, getDefaultTimeout(), interceptor, this);");	
			output.println(tab1 + "}");
			output.println();

		}
				
		output.println("}");
	}

	/**
	 * Generates property constructors.
	 * @param obj
	 * @param output
	 */
	private static void translate_bean_interface_property_constructors(IdlObject obj, PrintWriter output)
	{
		// constructors
		
		// visual composition & fromCORBA support
		output.println(tab1 + "/**");
		output.println(tab1 + " * Default constructor.");
		output.println(tab1 + " */");
		output.println(tab1 + "public " + obj.name() + "()");
		output.println(tab1 + "{");
		output.println(tab2 + "super();");
		output.println(tab1 + "}");
		output.println();
		
		output.println(tab1 + "/**");
		output.println(tab1 + " * Default constructor of " + obj.name() + "."); 
		output.println(tab1 + " * Constructor is given two arguments: the parent of the property and property name.");  
		output.println(tab1 + " * ");
		output.println(tab1 + " * @param parentComponent	parent component of this property.");
		output.println(tab1 + " * @param propertyName		name of the property.");
		output.println(tab1 + " */");
		output.println(tab1 + "public " + obj.name() + "(abeans.models.acs.baci.Component parentComponent, String propertyName)");
		output.println(tab1 + "{");
		output.println(tab2 + "super(parentComponent, propertyName);");
		output.println(tab1 + "}");
		output.println();
	}

	/**
	 * Translate an interface for the user
	 *
	 * @param obj interface to translate
	 * @param writeInto the directory where the interface must be defined
	 */
	private void translate_bean_interface(IdlObject obj, File writeInto)
	{
		// get inheritance list
		List inheritanceList = getInheritanceList(obj, new ArrayList(), false);
		// aleady issued
		//if (inheritanceList.size() > 1)
		//	System.out.println("WARNING: Multiple inheritance not supported, only first inteface will be inherited: "
		//						+ fullname(obj) + " inherits from " + inheritanceList);
	
	
		boolean isConnectable = isInheritedFrom(obj, BASE_CONNECTABLE_ID);
		boolean isLinkable = isConnectable || isInheritedFrom(obj, BASE_LINKABLE_ID);
	
		// check if it is a property interface,
		// property intefraces are handled differently
		if (isInheritedFrom(obj, BASE_PROPERTY_ID))
		{
			if (translate_bean_interface_property(obj, writeInto, inheritanceList))
				return;
		}
		else if (isInheritedFrom(obj, BASE_CALLBACK_ID))
		{
			// generate also implementation 
			translate_bean_interface_callback(obj, writeInto, inheritanceList);
		}
	
		File writeIntoAbeans = createDirectory(ABEANS_POSTFIX_DOTLESS, writeInto);
	    PrintWriter output = newFile(writeIntoAbeans, obj.name() /*+ "Bean"*/);
	
		addDescriptiveHeader(output, obj);
	
		// definition
		output.print("public class " + obj.name() /* + "Bean" */ + " extends ");
		if (inheritanceList.size() > 0)
		{
			IdlInterface itf = (IdlInterface)inheritanceList.get(0);
			if (itf.isForward())
				itf = itf.getInterface();
	
			String inheritFrom = fullname(itf);
			
			String id = itf.getId();
			if (id.equals(BASE_CONNECTABLE_ID))
				inheritFrom = "abeans.models.acs.baci.CharacteristicComponent";
			else if (id.equals(BASE_LINKABLE_ID))
				inheritFrom = "abeans.models.acs.baci.TypelessProperty";
				
			output.println(inheritFrom /*+ "Bean"*/);
		}
		else
			output.println("abeans.models.acs.baci.ProxyContainerSupport");
		output.println("{");
	
		output.println();
	
		// create list of linkable readonly attributes
		List linkableList = null;
		if (isConnectable)
		{
			linkableList = new ArrayList();
			
			obj.reset();
			while (obj.end() != true)
			{
				if (obj.current().kind() == IdlType.e_attribute)
				{
					IdlAttribute attribute = (IdlAttribute)obj.current();
					
					// if read-only
					if (attribute.isReadOnly())
					{
						IdlObject finalType = final_type(attribute.current());
						// ... and linkable
						if (isInheritedFrom(finalType, BASE_LINKABLE_ID))
						{
							// add to list
							linkableList.add(attribute);
							
							// and generate member
							output.println(tab1 + "/**");
							output.println(tab1 + " * " + attribute.name() + " accessor.");
							output.println(tab1 + " */");
							output.println(tab1 + "protected " + fullname(finalType) + " " + attribute.name() + ";");
							output.println();
						}
					}
				}
				obj.next();
			}
	
		}
	
	
		// constructors
	
		// visual composition & fromCORBA support
		output.println(tab1 + "/**");
		output.println(tab1 + " * Default constructor.");
		output.println(tab1 + " */");
		output.println(tab1 + "public " + obj.name() /* + "Bean" */ + "()");
		output.println(tab1 + "{");
		output.println(tab2 + "super();");
		if (isConnectable)
			output.println(tab2 + "initializeLinkables();");
		output.println(tab1 + "}");
		output.println();
	
		if (isConnectable)
		{
			output.println(tab1 + "/**");
			output.println(tab1 + " * Constructor.");
			output.println(tab1 + " * @param parent	parent family of this component.");
			output.println(tab1 + " * @param info   remote info od this component.");
			output.println(tab1 + " * @throws InitializationException");
			output.println(tab1 + " */");
			output.println(tab1 + "public " + obj.name() /* + "Bean" */ + "(abeans.models.Family parent, abeans.pluggable.RemoteInfo info)");
			output.println(tab2 + "throws abeans.core.InitializationException");
			output.println(tab1 + "{");
			output.println(tab2 + "super(parent, info);");
			output.println(tab2 + "initializeLinkables();");
			output.println(tab1 + "}");
			output.println();
		}
		else if (isLinkable)
		{
			output.println(tab1 + "/**");
			output.println(tab1 + " * Default constructor of " + obj.name() /* + "Bean" */ + "."); 
			output.println(tab1 + " * Constructor is given two arguments: the parent of the property and property name.");  
			output.println(tab1 + " * ");
			output.println(tab1 + " * @param parentComponent	parent component of this property.");
			output.println(tab1 + " * @param propertyName		name of the property.");
			output.println(tab1 + " */");
			output.println(tab1 + "public " + obj.name() /* + "Bean" */ + "(abeans.models.acs.baci.Component parentComponent, String propertyName)");
			output.println(tab1 + "{");
			output.println(tab2 + "super(parentComponent, propertyName);");
			output.println(tab1 + "}");
			output.println();
		}
		
		// initializeLinkables
		if (isConnectable)
		{
			output.println(tab1 + "/**");
			output.println(tab1 + " * Initialize <code>Linkable</code>s.");
			output.println(tab1 + " */");
			output.println(tab1 + "public void initializeLinkables()");
			output.println(tab1 + "{");
			
			if (linkableList != null)
			{
				Iterator it = linkableList.iterator();
				while (it.hasNext())
				{
					IdlAttribute attribute = (IdlAttribute)it.next();
					IdlObject finalType = final_type(attribute.current());
						
					output.println(tab2 + "if (" + attribute.name() + " == null)");
					output.println(tab3 + attribute.name() + " = new " + fullname(finalType) + "(this, \"" + attribute.name() + "\");");
				}
			}

			output.println(tab1 + "}");
		}		
		output.println();

		//
		// init proxy
		//
		if (isConnectable && linkableList != null && linkableList.size() > 0)
		{
			// initialize all readonly linkable attributes
			
			output.println(tab1 + "/**");
			output.println(tab1 + " * @see abeans.models.Linkable#initialize(abeans.pluggable.Proxy)");
			output.println(tab1 + " */");
			output.println(tab1 + "public synchronized void initialize(abeans.pluggable.Proxy proxy)");
			output.println(tab2 + "throws abeans.pluggable.RemoteException, abeans.core.AssertionFailed");
			output.println(tab1 + "{");
			output.println();
			output.println(tab2 + "if (proxy != null)");
			output.println(tab2 + "{");
			output.println();
			output.println(tab3 + "super.initialize(proxy);");
			output.println();
			output.println(tab3 + "// create properties");
			output.println();
	
			Iterator it = linkableList.iterator();
			while (it.hasNext())
			{
				IdlAttribute attribute = (IdlAttribute)it.next();
				IdlObject finalType = final_type(attribute.current());
				
				output.println(tab3 + "try");
				output.println(tab3 + "{");
				output.println(tab4 + "if (" + attribute.name() + " == null)");
				output.println(tab5 + attribute.name() + " = new " + fullname(finalType) + "(this, \"" + attribute.name() + "\");");
				output.println(tab4 + attribute.name() + ".initialize(" + attribute.name() + "().getProxy());");
				output.println(tab3 + "}");
				output.println(tab3 + "catch (Exception ex)");
				output.println(tab3 + "{");
				output.println(tab4 + "abeans.pluggable.RemoteException re = new abeans.pluggable.RemoteException(this, \"Failed to initialize property '" + attribute.name() + "'.\", ex);");
				output.println(tab4 + "re.putValue(\"proxy\", proxy);");
				output.println(tab4 + "re.putValue(\"" + attribute.name() + "\", " + attribute.name() + ");");
				output.println(tab4 + "re.caughtIn(this, \"initialize\");");
				output.println(tab4 + "throw re;");
				output.println(tab3 + "}");
				output.println();
	
			}
	
			output.println(tab2 + "}");
			output.println(tab2 + "else");
			output.println(tab2 + "{");
			
	/*
			// parent component will destroy all linkable (iterating throught the tree)
			it = linkableList.iterator();
			while (it.hasNext())
			{
				IdlAttribute attribute = (IdlAttribute)it.next();
				
				output.println(tab3 + "try");
				output.println(tab3 + "{");
				output.println(tab4 + "if (" + attribute.name()+ " != null)");
				output.println(tab5 + attribute.name() + ".initialize(null);");
				output.println(tab3 + "}");
				output.println(tab3 + "catch (Exception ex)");
				output.println(tab3 + "{");
				// TODO what to do here
				output.println(tab4 + "ex.printStackTrace();");
				output.println(tab3 + "}");
				output.println();
	
			}
	*/
			output.println(tab3 + "super.initialize(null);");
			output.println();
	
			output.println(tab2 + "}");
			output.println();
			output.println(tab1 + "}");
			output.println();
		}
		 
		//
		// translate the internal definitions
		// 
		String old_pkg = current_pkg;
	
		addToPkg(obj, obj.name() + "Package");
		File writeIntoPackage = null;
	
		obj.reset();
	
		while (obj.end() != true)
		{
			switch (obj.current().kind())
			{
	
			case IdlType.e_operation :
				translate_bean_operation(obj.current(), output, isLinkable, isConnectable);
				break;
	
			case IdlType.e_attribute :
				translate_bean_attribute(obj.current(), output, isLinkable, isConnectable,
										 isConnectable && linkableList!=null && linkableList.contains(obj.current()));
				break;
	
			case IdlType.e_const:
				translate_constant(obj.current(), null, output);
				break;

				case IdlType.e_enum :
					if (writeIntoPackage == null)
						writeIntoPackage = createDirectory(obj.name() + "Package", writeInto);
					translate_enum(obj.current(), writeIntoPackage);
					break;

				case IdlType.e_struct :
					if (writeIntoPackage == null)
						writeIntoPackage = createDirectory(obj.name() + "Package", writeInto);
					translate_struct(obj.current(), writeIntoPackage);
					break;

				case IdlType.e_union :
					if (writeIntoPackage == null)
						writeIntoPackage = createDirectory(obj.name() + "Package", writeInto);
					translate_union(obj.current(), writeIntoPackage);
					break;

				case IdlType.e_typedef :
					if (writeIntoPackage == null)
						writeIntoPackage = createDirectory(obj.name() + "Package", writeInto);
					translate_typedef(obj.current(), writeIntoPackage);
					break;

				case IdlType.e_exception :
					if (writeIntoPackage == null)
						writeIntoPackage = createDirectory(obj.name() + "Package", writeInto);
					translate_exception(obj.current(), writeIntoPackage);
					break;

				// TODO check this
				case IdlType.e_native :
					if (writeIntoPackage == null)
						writeIntoPackage = createDirectory(obj.name() + "Package", writeInto);
					translate_native(obj.current(), writeIntoPackage);
					break;

			}
	
			obj.next();
		}
	
		output.println("}");
		output.close();
	
		current_pkg = old_pkg;
	}

    /**
     * Check if the operation or the attribute is already in the list
     *
     * @param opList operations and attributes list
       * @param   op the operation or attribute
       * @return  true if the operation or attribute is included in the list
     */
    private static boolean isInto(List opList, IdlObject obj)
    {
        for (int i = 0; i < opList.size(); i++)
        {
            IdlObject elem = (IdlObject) opList.get(i);

            if (obj.name().equals(elem.name()) &&
                    obj.kind() == elem.kind())
            {
                // we would end up with more than one getXYZ()/setXYZ() method
                if (obj.kind() == IdlType.e_attribute)
                    return true;

                // check the signature
                if (obj.kind() == IdlType.e_operation)
                {
                    IdlOp op = (IdlOp) obj;
                    org.openorb.compiler.idl.reflect.idlParameter [] op_params = op.parameters();
                    IdlOp ops = (IdlOp) elem;
                    org.openorb.compiler.idl.reflect.idlParameter [] ops_params = ops.parameters();

                    // different number of parameters
                    if (op_params.length == ops_params.length)
                    {
                        boolean identical = true;

                        // compare the parameters, return true when equal, continue otherwise
                        for (int j = 0; j < op_params.length; j++)
                            if (((IdlObject) op_params[ j ]).kind() != ((IdlObject) op_params[ j ]).kind())
                                identical = false;

                        if (identical)
                            return true;
                    }
                }
            }
        }

        return false;
    }

    /**
     * Return the list of operations and attributes to implement
     * (incl. inheritance).
     *
     * @param obj interface object
     * @param opList already found operations and attributes list
     * @return operations and attributes list
     */
    private List getInheritanceOpList(IdlObject obj, List opList, boolean includeInherited)
    {
        obj.reset();

        while (obj.end() != true)
        {
            switch (obj.current().kind())
            {

            case IdlType.e_attribute :

            case IdlType.e_operation :

                if (!isInto(opList, obj.current()))
                    opList.add(obj.current());

                break;
            }

            obj.next();
        }

		if (includeInherited)
		{
			List list = ((IdlInterface) obj).getInheritance();
	        for ( int i = 0; i < list.size(); i++ )
	        {
	            if ( list.get(i) instanceof IdlInterface )
	            {
	                IdlInterface itf = ( IdlInterface ) list.get(i);
	
	                if ( itf.kind() == IdlType.e_forward_interface )
	                    itf = itf.getInterface();
	
	                opList = getInheritanceOpList( itf, opList, true );
	            }
	        }
		}
		
        return opList;
    }

    /**
     * Return the list of inherited objects
     *
     * @param obj the interface object
     * @param opList already found iherited objects
     * @return inherited objects list
     */
    private static List getInheritanceList(IdlObject obj, List inList, boolean recursive)
    {
        if ( ! ( obj instanceof IdlInterface ) )
            return inList;

        IdlInterface itf = (IdlInterface) obj;

        if (itf.isForward())
            itf = itf.getInterface();

        List list = itf.getInheritance();
/*
        boolean found = false;

        for (int i = 0; i < inList.size(); i++)
        {
            if (((String) inList.get(i)).equals(obj.getId()))
            {
                found = true;
                break;
            }
        }

        if (found == false)
        {
            inList.add(obj.getId());
        }

        for (int i = 0; i < list.size(); i++)
	            inList = getInheritanceList((IdlObject) list.get(i), inList, true);
*/
			
		if (recursive)
			inList.add(obj);
			
        for (int i = 0; i < list.size(); i++)
			if (recursive)
				inList = getInheritanceList((IdlObject) list.get(i), inList, true);
	        else
				inList.add(/*(IdlObject)*/ list.get(i));

        return inList;
    }

	/**
	 * Return true if object inherits from given IDL IR
	 *
	 * @param obj the interface object
	 * @param id  inheritance to be checked
	 * @return true if object inherits from given IDL IR, othervise false
	 */
	private static boolean isInheritedFrom(IdlObject obj, String id)
	{
		if ( ! ( obj instanceof IdlInterface ) )
			return false;

		IdlInterface itf = (IdlInterface) obj;
		
		if (itf.isForward())
			itf = itf.getInterface();

		List list = itf.getInheritance();
		for (int i = 0; i < list.size(); i++)
		{
			obj = (IdlObject) list.get(i);
			if (obj.getId().equals(id))
				return true;
			else if (isInheritedFrom(obj, id))
				return true;

		}

		return false;
	}

    /**
     * Return the initial name.
     */
    private static String initialName(String name)
    {
        if (name.startsWith("_"))
            return name.substring(1, name.length());

        return name;
    }

    /**
     * Translate a proxy for an interface
     *
     * @param obj the interface to translate
     * @param writeInto the directory where the interface must be defined
     */
    private void translate_interface(IdlObject obj, File writeInto)
    {
		final String baseName = obj.name() + "Proxy";

		// get inheritance list
		List inheritanceList = getInheritanceList(obj, new ArrayList(), false);
		if (inheritanceList.size() > 1)
			System.out.println("WARNING: Multiple inheritance not supported, only first inteface will be inherited: "
								+ fullname(obj) + " inherits from " + inheritanceList);

		// write into abeans/proxy directory
		// PACKAGE HANDLING
		File writeIntoAbeansProxy = createDirectory(ABEANS_POSTFIX_DOTLESS + File.separator + PROXY_POSTFIX_DOTLESS, writeInto);
		PrintWriter output = newFile(writeIntoAbeansProxy, baseName);

		// PACKAGE HANDLING
		String old_pkg = current_pkg;
		current_pkg += PROXY_POSTFIX;
		
        addDescriptiveHeader(output, obj);

		// PACKAGE HANDLING
        current_pkg = old_pkg;

		// definition
		output.print("public class " + baseName + " extends ");
		if (inheritanceList.size() > 0)
			// TODO PACKAGE HANDLING
			output.println(fullname((IdlObject)inheritanceList.get(0), ABEANS_POSTFIX_DOTLESS+PROXY_POSTFIX) + "Proxy");
		else
			output.println("abeans.pluggable.acs.maci.NarrowCORBAProxySupport");
		output.println("{");

		// get operation list w/ inherited operations
        List intoList = getInheritanceOpList(obj, new ArrayList(), false);

        boolean bUseHash = IdlCompiler.minTableSize <= intoList.size();

        final String[] operationNames = sortIntoArray(intoList);

        // generate ops hashtable
        if (bUseHash)
        {
            if (IdlCompiler.useClasses)
            {
                output.println(tab + "private static final java.util.Map operationMap = new java.util.HashMap();");
                output.println();
                output.println(tab + "static {");
                for (int i = 0; i < operationNames.length; i++)
                {
                    final String name = operationNames[i];
                    output.println(tab3 + "operationMap.put(\"" + name + "\",");
                    output.println(tab5 + "new Operation_" + name + "());");
                }
            }
            else //if (IdlCompiler.useSwitch)
            {
                output.println(tab + "private static final String[] operationNames = new String[" +
                        operationNames.length + "];");
                output.println();

                output.println(tab + "static {");
                for (int i = 0; i < operationNames.length; i++)
                {
                    output.println(tab2 + "operationNames[" + i + "] = \"" +
                            operationNames[i] + "\";");
                }
            }

            output.println(tab + "}");
            output.println();
        }

		// delegate obj
		output.println(tab1 + "/**");
		output.println(tab1 + " * Delegate object.");
		output.println(tab1 + "*/");
		output.println(tab1 + "private " + fullname_CORBA(obj) + " delegate;");
		output.println();
	
		// constructor
		output.println(tab1 + "/**");
		output.println(tab1 + "* Default constructor.");
		output.println(tab1 + "* @param delegate delegate CORBA object object.");
		output.println(tab1 + "*/");
		output.println(tab1 + "public " + baseName + "(org.omg.CORBA.Object delegate)");
		output.println(tab1 + "{");
		output.println(tab2 + "super(delegate);");
		output.println(tab2 + "this.delegate = " + fullname_CORBA(obj) + "Helper.narrow(delegate);");
		output.println(tab1 + "}");
		output.println();

		// delegate accessor
		output.println(tab1 + "/**");
		output.println(tab1 + " * Returns the delegate CORBA object on which all methods are invoked.");
		output.println(tab1 + " * @return				delegate CORBA object object.");
		output.println(tab1 + " * @see abeans.pluggable.acs.maci.NarrowCORBAProxy#getDelegate()");
		output.println(tab1 + " */");
		output.println(tab1 + "public org.omg.CORBA.Object getDelegate()");
		output.println(tab1 + "{");
		output.println(tab2 + "return delegate;");
		output.println(tab1 + "}");
		output.println();

        // translate the invoke operation
		output.println(tab1 + "/**");
		output.println(tab1 + " * @see abeans.pluggable.acs.maci.NarrowCORBAProxy#invoke(java.lang.String, java.lang.Object[])");
		output.println(tab1 + " */");
		output.println(tab1 + "public java.lang.Object invoke(String opName, java.lang.Object[] parameters) throws abeans.pluggable.RemoteException");
        output.println(tab1 + "{");

        output.println();

        if (bUseHash)
        {
            if (IdlCompiler.useClasses)
            {
                output.println(tab2 + "final AbstractOperation operation = (AbstractOperation)operationMap.get(opName);");
                output.println();

                output.println(tab2 + "if (null == operation)");
				if (inheritanceList.size() > 0)
					output.println(tab3 + "return super.invoke(opName, parameters);");
				else
					output.println(tab3 + "throw new org.omg.CORBA.BAD_OPERATION(opName);");
                output.println();

                output.println(tab2 + "return operation.invoke(this, parameters);");
            }
            else //if (IdlCompiler.useSwitch)
            {
                output.println(tab2 + "final int index = java.util.Arrays.binarySearch(operationNames, opName);");

                output.println(tab2 + "if (index < 0)");
				if (inheritanceList.size() > 0)
					output.println(tab3 + "return super.invoke(opName, parameters);");
				else
					output.println(tab3 + "throw new org.omg.CORBA.BAD_OPERATION(opName);");
				output.println();

                output.println(tab2 + "switch (index) {");

                for (int i = 0; i < operationNames.length; i++)
                {
                    output.println(tab3 + "case " + i + " :");
                    output.println(tab4 + "return _invoke_" + operationNames[i] + "(parameters);");
                }

                output.println(tab2 + "}");
                output.println();

                output.println(tab2 + "return null;");
            }
        }
        else
        {
            output.print(tab2);
            for (int i = 0; i < operationNames.length; i++)
            {
                final String name = operationNames[i];

                output.println("if (opName.equals(\"" + name + "\")) ");
                output.println(tab3 + "return _invoke_" + name + "(parameters);");
                output.print(tab2 + "else ");
            }

			output.println();
			if (inheritanceList.size() > 0)
				output.println(tab3 + "return super.invoke(opName, parameters);");
			else
	            output.println(tab3 + "throw new org.omg.CORBA.BAD_OPERATION(opName);");
        }

        output.println(tab + "}");
        output.println();

        // make helper methods
        if (intoList.size() > 0)
        {
			output.println(tab1 + "//");
			output.println(tab1 + "// helper methods");
			output.println(tab1 + "//");
			output.println();
			
	        for (int i = 0; i < intoList.size(); i++)
	            writeOperationHelperMethod((IdlObject)intoList.get(i), output);
	        
	        // write operation classes
	        if (bUseHash && IdlCompiler.useClasses)
	        {
	            //
	            output.println(tab1 + "// operation classes");
	            output.println(tab1 + "private abstract static class AbstractOperation");
				output.println(tab1 + "{");
	
	            output.println(tab2 + "protected abstract java.lang.Object invoke(final " + baseName + " target, java.lang.Object[] parameters);");
	
	            output.println(tab1 + "}");
	            output.println();
	            for (int i = 0; i < intoList.size(); i++)
	                writeOperationHelperClass((IdlObject)intoList.get(i), output, baseName);
	        }
		}

        output.println("}");

        output.close();

		write_helper(obj, writeInto);
		write_holder(obj, writeInto);
    }

    /**
	 * @param list
	 * @return
	 */
	private String[] sortIntoArray(final List list) {
        final String[] output;

        {
            int count = 0;

            for (int i = 0; i < list.size(); i++) {
                final IdlObject obj = ((IdlObject)list.get(i));
                switch (obj.kind())
                {
                    case IdlType.e_operation :
                        count++;
                        break;

                    case IdlType.e_attribute :
                        count += ((!((IdlAttribute)obj).readOnly()) ? 2 : 1);
                        break;
                }
            }

            output = new String[count];
        }

        for (int i = 0, count = 0; i < list.size(); i++) {
            final IdlObject obj = ((IdlObject)list.get(i));
            final String name = initialName(obj.name());
            switch (obj.kind())
            {
                case IdlType.e_operation :
                    output[count++] = name;
                    break;

                case IdlType.e_attribute :
                    output[count++] = "_get_" + name;

                    if (!((IdlAttribute)obj).readOnly())
                    {
                        output[count++] = "_set_" + name;
                    }

                    break;
            }
        }

        Arrays.sort(output);

        return output;
    }

    /**
     * Writes operation helper method for the proxy.
	 * @param obj
	 * @param output
	 */
	private void writeOperationHelperMethod(final IdlObject obj, final PrintWriter output)
    {
        switch (obj.kind())
        {
            case IdlType.e_operation :
                writeOperationHelperMethodHeader(obj, output, EMPTY_STRING);
                translate_operation_proxy("_invoke_" + initialName(obj.name()), obj, output);
                break;

            case IdlType.e_attribute :
                writeOperationHelperMethodHeader(obj, output, "_get_");
                translate_read_attribute_proxy(obj, output);

                if (!((IdlAttribute)obj).readOnly())
                {
                    output.println(tab1 + "}");
                    output.println();
                    writeOperationHelperMethodHeader(obj, output, "_set_");
                    translate_write_attribute_proxy(obj, output);
                }
                break;
            default :
                throw new Error("Illegal condition");
        }
        output.println(tab1 + "}");
        output.println();
    }

    /**
     * Writes helper method proxy header.
	 * @param obj
	 * @param output
	 * @param operationPrefix
	 */
	private void writeOperationHelperMethodHeader(final IdlObject obj,
	             final PrintWriter output, final String operationPrefix)
    {
        final String name = initialName(obj.name());

        output.print(tab1 + "private java.lang.Object _invoke_" + operationPrefix + name + "(java.lang.Object[] parameters)");
        
        // method
        if (operationPrefix.length()==0)
        	output.println(" throws abeans.pluggable.RemoteException");
        else
        	output.println();
        	
        output.println(tab1 + "{");
    }

    /**
     * Writes helper method proxy class.
	 * @param obj
	 * @param output
	 * @param baseName
	 */
	private void writeOperationHelperClass(final IdlObject obj,
            final PrintWriter output, final String baseName)
    {
        final String name = initialName(obj.name());

        switch (obj.kind())
        {
            case IdlType.e_operation :
                writeOperationHelperClass(output, baseName, name, EMPTY_STRING);
                break;

            case IdlType.e_attribute :
                writeOperationHelperClass(output, baseName, name, "_get_");

                if (!((IdlAttribute)obj).readOnly())
                    writeOperationHelperClass(output, baseName, name, "_set_");

                break;
        }

    }

    /**
     * Writes helper method proxy class.
	 * @param output
	 * @param baseName
	 * @param name
	 * @param operationPrefix
	 */
	private void writeOperationHelperClass(final PrintWriter output,
            final String baseName, final String name,
            final String operationPrefix)
    {
        output.println(tab1 + "private static final class " + "Operation_" + operationPrefix + name + " extends AbstractOperation");
        output.println(tab1 + "{");

        output.println(tab2 + "protected java.lang.Object invoke(final " + baseName + " target, java.lang.Object[] parameters)");
		output.println(tab2 + "{");
        output.println(tab3 + "return _invoke_" + operationPrefix + name + "(parameters);");
        output.println(tab2 + "}");

        output.println(tab1 + "}");
        output.println();
    }

    /**
     * Translate a module
     *
     * @param obj the module to translate
     * @param writeInto the directory where the module must be defined
     */
    private void translate_module(IdlObject obj, File writeInto, int translateType)
    {
        String old_pkg;

        File intoModule;

        if (translateType == 0)
            intoModule = createDirectory(obj.name(), writeInto);
        else
            intoModule = getDirectory(obj.name(), writeInto);

        old_pkg = current_pkg;

        addToPkg(obj, obj.name());

        translate_object(obj, intoModule, translateType);

        current_pkg = old_pkg;
    }

    /**
     * Translate a user module
     *
     * @param obj the module to translate
     * @param writeInto the directory where the module must be defined
     */
    private void translate_bean_module(IdlObject obj, File writeInto)
    {
        String old_pkg;

        File intoModule;

        if (obj.getPrefix() != null)
        {
            writeInto = getPrefixDirectories(obj.getPrefix(), writeInto);
        }

        intoModule = getDirectory(obj.name(), writeInto);

        old_pkg = current_pkg;

        if (obj.getPrefix() != null)
        {
            if (IdlCompiler.reversePrefix)
                addToPkg(obj, inversedPrefix(obj.getPrefix()) + "." + obj.name());
            else
                addToPkg(obj, obj.getPrefix() + "." + obj.name());
        }
        else
            addToPkg(obj, obj.name());

        translate_bean_object(obj, intoModule);

        current_pkg = old_pkg;
    }


    /**
     * Translate a value type state
     */
    private static void translate_state_member(IdlObject obj, PrintWriter output)
    {
        IdlStateMember member = (IdlStateMember) obj;

        if (obj.hasComment())
            javadoc(output, obj);
        else
        {
            output.println(tab + "/**");

            if (member.public_member())
                output.println(tab + " *  Public member : " + obj.name());
            else
                output.println(tab + " * Private member : " + obj.name());

            output.println(tab + " */");
        }

        if (member.public_member())
            output.print(tab + " public ");
        else
            output.print(tab + " protected ");

        obj.reset();

        translate_type(obj.current(), output);

        output.println(" " + obj.name() + ";");

        output.println();
    }

    /**
     * Return the list of the inherited members
     *
     */
    private static List getInheritedStateMember(IdlObject obj)
    {
        List list = new ArrayList();
        List sub = null;

        IdlValue [] inherited = ((IdlValue) obj).getInheritance();

        if (inherited.length != 0)
        {
            sub = getInheritedStateMember(inherited[ 0 ]);
        }

        if (sub != null)
        {
            for (int i = 0; i < sub.size(); i++)
                list.add(sub.get(i));
        }

        obj.reset();

        while (obj.end() != true)
        {
            if (obj.current().kind() == IdlType.e_state_member)
                list.add(obj.current());

            obj.next();
        }

        return list;
    }

    /**
     * Translate a Value Type
     *
     * @param obj la value type to translate
     * @param writeInto the directory where the module must be defined
     */
	// TODO org.omg -> translate_value_type
    private void translate_value_type(IdlObject obj, File writeInto)
    {
        IdlValue value = (IdlValue) obj;
        boolean base_custom = false;
        String old_pkg;

		File writeIntoAbeans = createDirectory(ABEANS_POSTFIX_DOTLESS, writeInto);
        PrintWriter output = newFile(writeIntoAbeans, obj.name());

        addDescriptiveHeader(output, obj);

        if (value.abstract_value())
        {
            output.print("public interface " + obj.name() + " extends org.omg.CORBA.portable.ValueBase");

            IdlValue [] abs_inheritance = value.getInheritance();

            if (abs_inheritance.length != 0)
                output.print(", ");

            for (int i = 0; i < abs_inheritance.length; i++)
            {
                output.print(fullname(abs_inheritance[ i ]));

                if (i + 1 < abs_inheritance.length)
                    output.print(", ");
            }

            output.println();

            if (value.supports().size() != 0)
            {
                output.print(tab2 + ", ");

                List list = value.supports();

                for (int i = 0; i < list.size(); i++)
                {
                    IdlInterface itf = (IdlInterface) list.get(i);

                    output.print(fullname(itf));

                    if (!itf.abstract_interface())
                        output.print("Operations");

                    if (i + 1 < list.size())
                        output.print(", ");
                }
            }

            output.println("{");

            // Functions from IDL description

            File intoMe = null;

            if (isEmptyValue(obj) == false)
                intoMe = createDirectory(obj.name() + "Package", writeInto);
            else
                intoMe = writeInto;

            // Traduit les definitions internes
            old_pkg = current_pkg;

            addToPkg(obj, obj.name() + "Package");

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
                    translate_constant(obj.current(), null, output);
                    break;

                case IdlType.e_operation :
                    translate_operation(obj.current(), output);
                    break;

                case IdlType.e_attribute :
                    translate_attribute(obj.current(), output);
                    break;
                }

                obj.next();
            }

            current_pkg = old_pkg;

            output.println("}");
            output.println();
            output.close();
        }
        else
        {
            // -----------------------------------------
            // The class implementing the concrete value
            // -----------------------------------------

            output.print("public abstract class " + obj.name());

            // Case : Values types that do not inherit from other values or interfaces
            if ((value.supports().size() == 0) && (value.getInheritance().length == 0))
            {
                if (value.custom_value())
                {
                    output.println(" implements org.omg.CORBA.portable.CustomValue");
                }
                else
                {
                    output.println(" implements org.omg.CORBA.portable.StreamableValue");
                }
            }
            else
            {

                IdlValue [] inheritance = value.getInheritance();

                boolean extends_value = false;
                boolean streamNeed = false;

                if (inheritance.length == 0)
                    streamNeed = true;

                if (inheritance.length != 0)
                    extends_value = true;

                for (int i = 0; i < inheritance.length; i++)
                {
                    if (inheritance[ i ].custom_value())
                        base_custom = true;
                }

                if ((extends_value == false) && (value.supports().size() == 0))
                {
                    output.print(" implements org.omg.CORBA.portable.StreamableValue");
                }

                boolean extend_concrete = false;
                boolean extend_abstract = false;

                for (int i = 0; i < inheritance.length; i++)
                {
                    if (! inheritance[ i ].abstract_value())
                        extend_concrete = true;
                    else
                        extend_abstract = true;
                }

                // case : Inheritance from other stateful values
                if (extends_value)
                {
                    if (extend_concrete)
                    {
                        output.print(" extends ");
                        int j = 0;

                        for (int i = 0; i < inheritance.length; i++)
                        {
                            if (inheritance[ i ].abstract_value() == false)
                            {
                                j++;

                                if (j > 1)
                                    output.print(", ");

                                output.print(fullname(inheritance[ i ]));
                            }
                        }
                    }

                    if (extend_abstract)
                    {
                        output.print(" implements ");
                        int j = 0;

                        for (int i = 0; i < inheritance.length; i++)
                        {
                            if (inheritance[ i ].abstract_value())
                            {
                                j++;

                                if (j > 1)
                                    output.print(", ");

                                output.print(fullname(inheritance[ i ]));
                            }
                        }

                        if (!extend_concrete)
                            output.print(", org.omg.CORBA.portable.StreamableValue");
                    }

                }

                if (!(extend_abstract && (value.supports().size() != 0)))
                    output.println();

                // case : Inheritance from abstract values
                boolean implement_map = false;

                if (value.custom_value() && !base_custom)
                {
                    output.print(tab2 + "implements org.omg.CORBA.portable.CustomValue");
                    implement_map = true;
                }

                // case : Supported interfaces
                if (value.supports().size() != 0)
                {
                    if (implement_map)
                        output.print(", ");
                    else
                    {
                        if (!extend_abstract)
                            output.print(tab2 + "implements ");
                        else
                            output.print(", ");

                        if (streamNeed)
                            output.print("org.omg.CORBA.portable.StreamableValue, ");
                    }

                    List list = value.supports();

                    for (int i = 0; i < list.size(); i++)
                    {
                        IdlInterface itf = (IdlInterface) list.get(i);

                        output.print(itf.name());

                        if (!itf.abstract_interface())
                            output.print("Operations");

                        if (i + 1 < list.size())
                            output.print(", ");
                    }
                }

                output.println();
            }

            output.println("{");

            // Functions from IDL description

            File intoMe = null;

            if (isEmptyValue(obj) == false)
                intoMe = createDirectory(obj.name() + "Package", writeInto);
            else
                intoMe = writeInto;

            // Translate the internal definitions
            old_pkg = current_pkg;

            addToPkg(obj, obj.name() + "Package");

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
                    translate_constant(obj.current(), null, output);
                    break;

                case IdlType.e_operation :
                    translate_operation(obj.current(), output);
                    break;

                case IdlType.e_attribute :
                    translate_attribute(obj.current(), output);
                    break;

                case IdlType.e_state_member :
                    translate_state_member(obj.current(), output);
                    break;
                }

                obj.next();
            }

            current_pkg = old_pkg;

            // Functions from ValueBase

            output.println(tab + "/**");
            output.println(tab + " * Return the truncatable ids");
            output.println(tab + " */");

            output.println(tab + "static final String[] _ids_list =");
            output.println(tab + "{");

            String [] list = value.truncatableList();
            int max = list.length;

            if (!value.isTruncatable())
                max = 1;

            for (int i = 0; i < max; i++)
            {
                output.print(tab2 + "\"" + list[ i ] + "\"");

                if (i + 1 < list.length)
                    output.print(",");

                output.println();
            }

            output.println(tab + "};");
            output.println();

            output.println(tab + "public String [] _truncatable_ids()");
            output.println(tab + "{");
            output.println(tab2 + "return _ids_list;");
            output.println(tab + "}");
            output.println();

            // Functions from Streamable
            if (!value.custom_value() && !base_custom)
            {
                output.println(tab + "/**");
                output.println(tab + " * Unmarshal the value into an InputStream");
                output.println(tab + " */");
                output.println(tab + "public void _read(org.omg.CORBA.portable.InputStream is)");
                output.println(tab + "{");

                List stateList = getInheritedStateMember(obj);

                for (int i = 0; i < stateList.size(); i++)
                {
                    IdlStateMember state = (IdlStateMember) stateList.get(i);
                    state.reset();
                    translate_unmarshalling_member(state.current(), output, "is", state.name(), tab2 + EMPTY_STRING);
                }

                output.println(tab + "}");
                output.println();

                output.println(tab + "/**");
                output.println(tab + " * Marshal the value into an OutputStream");
                output.println(tab + " */");
                output.println(tab + " public void _write(org.omg.CORBA.portable.OutputStream os)");
                output.println(tab + " {");

                for (int i = 0; i < stateList.size(); i++)
                {
                    IdlStateMember state = (IdlStateMember) stateList.get(i);
                    state.reset();
                    translate_marshalling_member(state.current(), output, "os", state.name(), tab2 + EMPTY_STRING);
                }

                output.println(tab + "}");
                output.println();

                output.println(tab + "/**");
                output.println(tab + " * Return the value TypeCode");
                output.println(tab + " */");
                output.println(tab + " public org.omg.CORBA.TypeCode _type()");
                output.println(tab + " {");
                output.println(tab2 + "return " + fullname(obj) + "Helper.type();");
                output.println(tab + " }");
                output.println();
            }

            output.println("}");
            output.println();
            output.close();

            // ----------------------------------
            // The class implementing the factory
            // ----------------------------------
            boolean factory = false;
            obj.reset();

            while (obj.end() != true)
            {
                if (obj.current().kind() == IdlType.e_factory)
                {
                    factory = true;
                    break;
                }

                obj.next();
            }

            if (factory)
            {
				writeInto = createDirectory(ABEANS_POSTFIX_DOTLESS, writeInto);
                output = newFile(writeInto, obj.name() + "ValueFactory");

                addDescriptiveHeader(output, obj.current());

                output.println("public interface " + obj.name() + "ValueFactory extends org.omg.CORBA.portable.ValueFactory");
                output.println("{");

                obj.reset();

                while (obj.end() != true)
                {
                    if (obj.current().kind() == IdlType.e_factory)
                    {
                        output.println(tab + "/**");
                        output.println(tab + " * Return the value type");
                        output.println(tab + " */");
                        output.print(tab + " public abstract " + fullname(obj) + " " + obj.current().name() + "(");

                        obj.current().reset();

                        while (obj.current().end() != true)
                        {
                            IdlFactoryMember member = (IdlFactoryMember) obj.current().current();

                            member.reset();
                            translate_type(member.current(), output);
                            output.print(" " + member.name());

                            obj.current().next();

                            if (obj.current().end() != true)
                                output.print(", ");
                        }

                        output.println(");");
                        output.println();
                    }

                    obj.next();
                }

                output.println("}");
                output.println();
                output.close();
            }
        }

        write_holder(obj, writeInto);
        //write_helper(obj, writeInto);

        writeDefaultValueFactory((IdlValue)obj, writeInto);
        writeDefaultValueImpl((IdlValue)obj, writeInto);
    }

	// TODO org.omg -> writeDefaultValueFactory
    private void writeDefaultValueFactory(final IdlValue obj, final File writeInto)
    {
        if( obj.isAbstract() || obj.isCustom() ||
                ( null == IdlCompiler.generateValueFactory) )
        {
            return;
        }

        final String className = obj.name() + "DefaultFactory";
        final String implName = obj.name() + IdlCompiler.generateValueFactory;
        final PrintWriter output = newFile(writeInto, className);

        addPackageName(output);
        output.println("// " + fullname(obj));

        output.println("public class " + className + " implements org.omg.CORBA.portable.ValueFactory {");

        output.println(tab1 + "public " + className + "() {}");
        output.println();

        output.println(tab1 + "public Serializable read_value(");
        output.println(tab3 + "final org.omg.CORBA_2_3.portable.InputStream is) {");
        output.println(tab2 + "return is.read_value(new " + implName + "());");
        output.println(tab1 + "}");
        output.println();

        output.println(tab1 + "}");
        output.println();
        output.flush();
        output.close();
    }

    private void writeDefaultValueImpl(final IdlValue obj, final File writeInto)
    {
        if( obj.isAbstract() || ( null == IdlCompiler.generateValueImpl ) )
        {
            return;
        }
        final String className = obj.name() + IdlCompiler.generateValueImpl;
        final PrintWriter output = newFile(writeInto, className);

        addPackageName(output);

        output.println("public class " + className + " extends " + obj.name() + " {");

        output.println(tab1 + "public " + className + "() {}");
        output.println();

        writeDefaultMethods(output, obj, new HashSet());

        output.println("}");
        output.println();
        output.flush();
        output.close();
    }

    private static void writeDefaultMethods(final PrintWriter output, final IdlObject obj, final Set ids)
    {
        if (!ids.add(obj.getId())) {
            return;
        }

        for (obj.reset(); !obj.end(); obj.next())
        {
            switch (obj.current().kind())
            {
                case IdlType.e_operation :
                    writeDefaultOperation(output, (IdlOp)obj.current());
                    break;

                case IdlType.e_attribute :
                    writeDefaultAttribute(output, (IdlAttribute)obj.current());
                    break;
            }
        }

        switch (obj.kind())
        {
            case IdlType.e_interface:
                writeDefaultMethodsForInterfaces(output, ((IdlInterface)obj).getInheritance() ,ids);
                break;

            case IdlType.e_value:
                writeDefaultMethodsForValueTypes(output, ((IdlValue)obj).getInheritanceList(),ids);
                writeDefaultMethodsForInterfaces(output, ((IdlValue)obj).supports(),ids);
                break;
        }
    }

    private static void writeDefaultMethodsForValueTypes(final PrintWriter output, final List list, final Set ids)
    {
        for (int i = 0; i < list.size(); i++)
        {
            writeDefaultMethods(output, ((IdlValueInheritance)list.get(i)).getValue(), ids);
        }
    }

    private static void writeDefaultMethodsForInterfaces(final PrintWriter output, final List list, final Set ids)
    {
        for (int i = 0; i < list.size(); i++)
        {
            writeDefaultMethods(output, (IdlInterface)list.get(i), ids);
        }
    }

    private static void writeDefaultAttribute(final PrintWriter output, final IdlAttribute obj)
    {
        obj.reset();
        final IdlObject attributeType = obj.current();

        output.print(tab + "public ");
        translate_type(attributeType, output);
        output.println(" " + obj.name() + "() {");

        output.println(tab2 + "throw new Error(\"Unimplemented operation\");");

        output.println(tab1 + "}");
        output.println();

        if (!obj.readOnly())
        {
            output.print(tab + "public ");
            output.print("void " + obj.name() + "(");
            translate_type(attributeType, output);
            output.println(") {");
            output.println(tab2 + "throw new Error(\"Unimplemented operation\");");
            output.println(tab1 + "}");
            output.println();
        }
    }

	// TODO orm.omg -> writeDefaultOperation
    private static void writeDefaultOperation(final PrintWriter output, final IdlOp obj)
    {
        IdlRaises r;
        IdlContext c;
        boolean someParams = false;

        output.print(tab + "public ");

        obj.reset();

        final IdlObject returnType = obj.current();

        translate_type(returnType, output);

        output.print(" " + obj.name() + "(");

        obj.next();

        if (!obj.end())
        {
            if (obj.current().kind() == IdlType.e_param)
            {
                someParams = true;

                while (!obj.end())
                {

                    obj.current().reset();
                    translate_parameter(obj.current().current(), output, ((IdlParam) obj.current()).param_attr());

                    output.print(" " + obj.current().name());

                    obj.next();

                    if (obj.end() != true)
                    {
                        if (obj.current().kind() == IdlType.e_param)
                            output.print(", ");
                        else
                            break;
                    }
                }
            }
        }

        c = getContext(obj);

        if (c != null)
        {
            if (someParams == true)
                output.print(", ");

            output.print("org.omg.CORBA.Context ctx");
        }

        output.print(")");

        if (obj.end() != true)
        {
            if (obj.current().kind() == IdlType.e_raises)
            {
                output.println();
                output.print(tab2 + "throws ");
                r = (IdlRaises) obj.current();

                r.reset();

                while (!r.end())
                {

                    output.print(fullname(r.current()));

                    r.next();

                    if (!r.end())

                        output.print(", ");

                }
            }
        }

        output.println("{");
        output.println(tab2 + "throw new Error(\"Unimplemented operation\");");
        output.println(tab1 + "}");
        output.println();
    }


    /**
     * Translate a Value Box
     *
     * @param obj the value box to translate
     * @param writeInto the directory where the module must be defined
     */
    // TODO org.omg -> translate_value_box
    private void translate_value_box(IdlObject obj, File writeInto)
    {
        IdlValueBox value = (IdlValueBox) obj;

        if (value.simple())
        {
			File writeIntoAbeans = createDirectory(ABEANS_POSTFIX_DOTLESS, writeInto);
            PrintWriter output = newFile(writeIntoAbeans, obj.name());

            addDescriptiveHeader(output, obj);

            output.println("public class " + obj.name() + " implements org.omg.CORBA.portable.ValueBase");
            output.println("{");

            output.println(tab + "/**");
            output.println(tab + " * Reference to the boxed value");
            output.println(tab + " */");
            output.print(tab + "public ");

            obj.reset();
            translate_type(obj.current(), output);
            output.println(" value;");
            output.println();

            output.println(tab + "/**");
            output.println(tab + " * Constructor");
            output.println(tab + " * ");
            output.println(tab + " * @param initial the initial boxed value");
            output.println(tab + " */");
            output.print(tab + "public " + obj.name() + "(");
            translate_type(obj.current(), output);
            output.println(" initial)");
            output.println(tab + "{");
            output.println(tab2 + "value = initial;");
            output.println(tab + "}");
            output.println();

            output.println(tab + "//");
            output.println(tab + "// Return value box id");
            output.println(tab + "//");
            output.println(tab + "private static String[] _ids = { " + obj.name() + "Helper.id() };");
            output.println();

            output.println(tab + "/**");
            output.println(tab + " * Return truncatable ids");
            output.println(tab + " */");
            output.println(tab + " public String[] _truncatable_ids()");
            output.println(tab + " {");
            output.println(tab2 + "return _ids;");
            output.println(tab + " }");
            output.println();

            output.println("}");

            output.close();

            write_holder(obj, writeInto);
            //write_helper(obj, writeInto);
        }
        else
        {
            // Definition of the sub-types
            obj.reset();

            while (obj.end() != true)
            {
                switch (obj.current().kind())
                {

                case IdlType.e_union :
                    translate_union(obj.current(), writeInto);
                    break;

                case IdlType.e_struct :
                    translate_struct(obj.current(), writeInto);
                    break;

                case IdlType.e_enum :
                    translate_enum(obj.current(), writeInto);
                    break;
                }

                obj.next();
            }

            obj.reset();

            switch (obj.current().kind())
            {

            case IdlType.e_string :

            case IdlType.e_wstring :

            case IdlType.e_simple :

            case IdlType.e_fixed :

            case IdlType.e_union :

            case IdlType.e_struct :

            case IdlType.e_enum :

            case IdlType.e_sequence :

            case IdlType.e_array :

            case IdlType.e_typedef :

            case IdlType.e_ident :
                //write_helper(obj, writeInto);
                write_holder(obj, writeInto);
                break;
            }
        }

    }

    /**
     * Translate a native type
     *
     * @param obj the module to translate
     * @param writeInto the directory where the module must be defined
     */
    private void translate_native(IdlObject obj, File writeInto)
    {
        write_holder(obj, writeInto);
        //write_helper(obj, writeInto);
    }

    /**
     * Translate an object content
     */
    private void translate_object_content(IdlObject obj, File writeInto, int translateType)
    {
        switch (obj.current().kind())
        {

        case IdlType.e_module :
            translate_module(obj.current(), writeInto, translateType);
            break;

        case IdlType.e_const :

            if (translateType == 0)
                translate_constant(obj.current(), writeInto, null);

            break;

        case IdlType.e_enum :
            if (translateType == 0)
                translate_enum(obj.current(), writeInto);

            break;

        case IdlType.e_struct :
            if (translateType == 0)
                translate_struct(obj.current(), writeInto);

            break;

        case IdlType.e_union :
            if (translateType == 0)
                translate_union(obj.current(), writeInto);

            break;

        case IdlType.e_typedef :
            if (translateType == 0)
                translate_typedef(obj.current(), writeInto);

            break;

        case IdlType.e_exception :
            if (translateType == 0)
                translate_exception(obj.current(), writeInto);

            break;

        case IdlType.e_native :
            if (translateType == 0)
                translate_native(obj.current(), writeInto);

            break;

        case IdlType.e_value_box :
            if (translateType == 0)
                translate_value_box(obj.current(), writeInto);

            break;

        case IdlType.e_value :
            if (translateType == 0)
                translate_value_type(obj.current(), writeInto);

            break;

        case IdlType.e_interface :
        
        	if (translateType == 0)
				if (((IdlInterface) (obj.current())).abstract_interface() == false)
				{
					if (((IdlInterface) (obj.current())).local_interface() == false)
						translate_interface(obj.current(), writeInto);
				}
				
            break;
        }

    }

    /**
     * Translate the data from a container object (Module, Interface, Root)
     *
     * @param obj the object to translate
     * @param writeInto the write access
     * @param translateType the translation type (0=data, 1=stub, 2=skeleton)
     */
    private void translate_object(IdlObject obj, File writeInto, int translateType)
    {
        obj.reset();

        while (obj.end() != true)
        {
            File tmpInto = writeInto;

            if (obj.current().included() == false)
            {
                String old_pkg = current_pkg;

                if (IdlCompiler.usePrefix)
                    if ((obj.current().getPrefix() != null) && (obj.kind() == IdlType.e_root))
                    {
                        if (translateType == 0)
                        {
                            tmpInto = createPrefixDirectories(obj.current().getPrefix(), writeInto);
                        }
                        else
                        {
                            tmpInto = getPrefixDirectories(obj.current().getPrefix(), writeInto);
                        }
						
                        if (IdlCompiler.reversePrefix)
                            addToPkg(obj, inversedPrefix(obj.current().getPrefix()));
                        else
                            addToPkg(obj, obj.current().getPrefix());
                    }

                translate_object_content(obj, tmpInto, translateType);

                current_pkg = old_pkg;
            }

            obj.next();
        }
    }

    /**
     * Translate the data from a container object (Module, Interface, Root)
     * for the user code
     *
     * @param obj the object to translate
     * @param writeInto the write access
     */
    private void translate_bean_object(IdlObject obj, File writeInto)
    {
        obj.reset();

        while (obj.end() != true)
        {
            if (obj.included() == false)
                switch (obj.current().kind())
                {

                case IdlType.e_module :
                    translate_bean_module(obj.current(), writeInto);
                    break;

                case IdlType.e_interface :

					if (((IdlInterface) (obj.current())).abstract_interface() == false)
					{
						if (((IdlInterface) (obj.current())).local_interface() == false)
							translate_bean_interface(obj.current(), writeInto);
					}

                    break;
                }

            obj.next();
        }
    }

    /**
     * Translate the data from IDL to Abeans model java classes, helpers, holders 
     *
     * @param obj The compilation graph root
     * @param packageName The directory where the definitions are added
     */
	public void translateData(IdlObject obj, String packageName)
    {
        _root = obj;

        File first = null;

        if (IdlCompiler.outdir != null)
            first = new File(IdlCompiler.outdir);

        File writeInto = createDirectory(packageName, first);

        translate_object(obj, writeInto, 0);
    }

    /**
     * Generate the Abeans R3 BACI model implemenration (bean)
     *
     * @param obj The compilation graph root
     * @param packageName The directory where the definitions are added
     */
	public void translateBean(IdlObject obj, String packageName)
    {
        _root = obj;

        File first = null;

        if (IdlCompiler.outdir != null)
            first = new File(IdlCompiler.outdir);

        File writeInto = getDirectory(packageName, first);

        initial = writeInto;

        translate_bean_object(obj, writeInto);
    }

}
