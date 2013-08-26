/*
 * Copyright (c) Eric D. Friedman 1998. All Rights Reserved.
 * Copyright (c) Paul Kinnucan 1998. All Rights Reserved.
 *
 * $Revision: 1.26 $ 
 * $Date: 2002/02/27 10:32:22 $ 
 *
 * InterfaceFactory is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 *
 * InterfaceFactory is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * To obtain a copy of the GNU General Public License write to the
 * Free Software Foundation, Inc.,  59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.  
 */

package jde.wizards;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.io.PrintWriter;
import java.util.Hashtable;
import java.util.Vector;
import java.util.Enumeration;

/**
 * Defines a factory for creating  skeleton implementations of 
 * Java interfaces. The factory can be invoked from the command line
 * or from another program. The factory can generate implementations for
 * multiple interfaces when invoked from the command line.
 *
 * @author Eric D. Friedman and Paul Kinnucan
 * @version $Revision: 1.26 $
 */

public class InterfaceFactory extends MethodFactory
{

  /** A table w/ declaring classes as keys and vectors of method
   * signatures as values */
  private Hashtable interfaces = new Hashtable();

  /** The interface factory. */
  static InterfaceFactory interfaceFactory;
  
  public InterfaceFactory() {}

  /** 
   * Creates an InterfaceFactory that uses the specified NameFactory
   * for generating parameter names 
   *
   * @param factory Factory for generating parameter names
   */
  public InterfaceFactory(NameFactory factory)
  {
    super(factory);
  }

  /** 
   * Adds a signature to the signature table.
   * The signatures are stored in Vectors with their declaring
   * Classes as keys in a hashtable.  This allows them to be pulled
   * out in groups when we print, and keeping the declaring Class info
   * lets us put in comments about where the method (or method group)
   * comes from.  Signatures are not added if they're already registered
   * because we do not want duplicate method implementations even though
   * a class might implement interfaces that inherit from some common
   * super-interface. 
   *
   * @param sig Signature to be stored in the signature table.
   */
  private final void sortByDeclaringClass(Signature sig)
  {
    String declaring = sig.getDeclaringClass().getName();
    if (interfaces.containsKey( declaring ))
    {
      Vector v = (Vector)interfaces.get( declaring );
      if (! v.contains( sig ) ) // "There can be only one" - the Highlander
        v.addElement(sig);
    } // end of if (interfaces.containsKey( dec ))
    else
    {
      Vector v = new Vector();
      v.addElement(sig);
      interfaces.put( declaring, v );
    } // end of else
  }


  /** 
   * Clears the import and interface hashtables for this factory so they
   * can be re-used to process a new set of interfaces.
   */
  public void flush()
  {
    super.flush();
    interfaces.clear();
  }

  
  /**
   * Generates signatures based on introspection of the specified interface. 
   * Strips package specifiers from generated signatures.
   *
   * @param name the interface to process for signatures.
   * @exception NotAnInterfaceException the requested class isn't an interface
   * @exception java.lang.ClassNotFoundException the requested class cannot
   be loaded 
  */
  public void process(String interfaceName)
    throws ClassNotFoundException, NotAnInterfaceException
  {
    process(interfaceName, true);
  }  
  
  /**
   * Generates signatures based on introspection of the specified class. 
   *
   * @param name the interface to process for signatures.
   * @param truncate toggles truncation of package specifiers in signatures..
   *
   * @exception NotAnInterfaceException the requested class isn't an interface
   * @exception java.lang.ClassNotFoundException the requested class cannot
   * be loaded 
  */
  public void process(String name, boolean truncate)
    throws ClassNotFoundException, NotAnInterfaceException
  {
    if (null == namefactory)
      namefactory = new DefaultNameFactory();
    
    Class aclass = Class.forName( name );
    if (false == aclass.isInterface())
      throw new NotAnInterfaceException(name);
    
    Method[] methods = aclass.getMethods();
    for (int i = 0; i < methods.length; i++)
      sortByDeclaringClass( new Signature( methods[i], this, truncate ) );
  }


  /**
   * Makes an implementation of an interface. This method delegates the creation
   * of the implementation to makeInterfaceInternal.
   *
   * @param name Name of interface to be implemented.
   * @param javadoc If <code>true</code> generate skeletal Javadoc for the implementation.
   * @param truncate If <code>true</code>, truncate package specifier when generating code.
   * @param newline If <code>true</code>, insert a newline after opening brace.
   * 
   */
  public static void makeInterface(String name, 
			           boolean javadoc,
				   boolean truncate,
				   boolean newline) {

    if (interfaceFactory == null)
      interfaceFactory = new InterfaceFactory();

    interfaceFactory.flush();
    interfaceFactory.makeInterfaceInternal(name, javadoc, truncate, newline);

  }

 /**
   * Makes an implementation of an interface.
   *
   * @param name Name of interface to be implemented.
   * @param javadoc If <code>true</code> generate skeletal Javadoc for the implementation.
   * @param truncate If <code>true</code>, truncate package specifier when generating code.
   * @param newline If <code>true</code>, insert a newline after opening brace.
   * 
   */
  private void makeInterfaceInternal(String name,
				     boolean javadoc,
				     boolean truncate,
				     boolean newline) {
    try {
      process(name, truncate);
    }
    catch (ClassNotFoundException e) {
      println("(error \"Error: could not find interface named: " + name + ". "
	      + "Note: name must be qualified.\")");
      return;
    }
    catch (NotAnInterfaceException e) {
      println("(error \"Error: " + name + " is not an interface.\")");
      return;
    }
    catch (Exception e) {
      println("(error \"Error: unknown type.\")");
      return;
    }

    dump(new PrintWriter( System.out, true),
	 javadoc,
	 truncate,
	 newline );
  }


  public static void getImportedClasses() {
    String res = "(list ";
    Enumeration i = interfaceFactory.imports.keys();
    while (i.hasMoreElements()) {
      Class c = (Class) i.nextElement();
      res += "\"" + c.getName() + "\" ";
    }
    res += ")";
    println(res);
  }


  public void dump(PrintWriter out,
                          boolean javadoc,
                          boolean truncate,
                          boolean newline)
  {
    String res = "";

    Enumeration declaring_classes = interfaces.keys();
    while (declaring_classes.hasMoreElements())
    {
      String interf = (String)declaring_classes.nextElement();
      Vector v = (Vector)interfaces.get(interf);
      res = "\"// implementation of " + interf + " interface\n";
      Enumeration e = v.elements();
      while (e.hasMoreElements())
      {
        Signature sig = (Signature)e.nextElement();
	String todo = "  // TODO: implement this " + interf + " method\n";
	res += getMethodSkeleton(sig, javadoc, newline, todo);
      } // end of while (e.hasMoreElements())
    }
    res += "\"";
    println(res);
  }
} // SignatureFactory

class NotAnInterfaceException extends Exception
{
  NotAnInterfaceException (String name)
  {
    super(name);
  }
}

// End of InterfaceFactory.java
