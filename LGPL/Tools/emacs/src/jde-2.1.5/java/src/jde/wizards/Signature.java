/*
 * Copyright (c) Eric D. Friedman 1998. All Rights Reserved.
 * Copyright (c) Paul Kinnucan 1998. All Rights Reserved.
 *
 * $Revision: 1.26 $ 
 * $Date: 2002/02/27 10:32:23 $ 
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

/**
 * Defines a method signature.
 *
 * @author Eric D. Friedman
 * @version $Revision: 1.26 $
 */

public class Signature
{
  /** 
   * The method represented by this signature 
  */
  private Method method;

  /** 
   * the parameters in this signature 
   */
  private Class[] parameters;

  /** 
   * the exceptions thrown by this signature 
   */
  private Class[] exceptions;

  /** 
   * the return value 
   */
  private Class return_value;

  /** 
   * The Class that declared this signature
   */
  private Class declaring;

  /**
   * the InterfaceFactory processing this signature 
   */
  private ClassRegistry registry;

  /**
   * Toggles truncation of package information in signatures 
   */
  private boolean truncate;

  /** 
   * Creates a signature for the specified method and register
   * its types with the specified registry (which may be null).
   * Removes package info from generated signatures.
   *
   * @param meth Method represented by this signature
   * @param aregistry Registry containing this signature
   */
  public Signature (Method meth, ClassRegistry aregistry)
  {
    this(meth,aregistry,true);
  }

  /** 
   * Creates a signature for the specified method and register its
   * types with the specified registry (which may be null).
   *
   * @param meth Method represented by thi signature
   * @param aregistry Registry containing this signature
   * @param truncate toggles removal of package info from generated signatures 
   */
  public Signature (Method meth, ClassRegistry aregistry, boolean truncate)
  {
    this.method   = meth;
    this.registry = aregistry;
    this.truncate = truncate;
    
    parameters   = method.getParameterTypes();
    exceptions   = method.getExceptionTypes();
    return_value = method.getReturnType();
    declaring    = method.getDeclaringClass();

    register();
  }

  /** 
   * Toggles truncation of package info .
   * 
   * @param b Truncation toggle
   */
  public void setTruncating( boolean b )
  {
    truncate = b;
  }

  /** 
   * Sets the registry used to register this signature's types.
   * 
   * @param registry Registry used to register this signature's type.
   */
  public void setRegistry(ClassRegistry registry )
  {
    this.registry = registry;
  }

  /** 
   * Gets the class that declared the method represented by this signature.
   *
   * @return The class this method was declared in 
   */
  public Class getDeclaringClass ()
  {
    return declaring;
  }

  /** 
   * Generates a javadoc string for this signature.
   * 
   * @return A javadoc for this method
   */
  public String toJavaDoc()
  {
    NameFactory factory = registry.getNameFactory();
    StringBuffer buf = new StringBuffer("/**\n *\n *");
    for (int i = 0; i < parameters.length; i++)
      buf.append(" @param " +
                 factory.getParameterName(parameters[i],
                                 i + 1,
                                 parameters.length) +
                 " <description>\n *");

    if (! "void".equals(return_value.getName()))
      buf.append(" @return <description>" + "\n *");
    for (int i = 0; i < exceptions.length; i++)
      buf.append(" @exception " + exceptions[i].getName() + " <description>\n *");
    buf.append("/");
    return buf.toString();
  }

  /** 
   * Gets the signature as a string.
   *
   * @return Signature of this method.
   */
  public String toString()
  {
    String m    = getModifiers();
    String r    = baseName( return_value );
    String meth = method.getName();

    String p    = getParameters( parameters );
    String e    = getExceptions( exceptions );

    return m + " " + r + " " + meth + "(" + p + ")" + e;
  }

  public boolean paramsEqual(Class[] p) {
    int n = parameters.length;
    boolean res = (p.length == n);
    if (res) 
      for (int i = 0; i < n; ++i) 
	if (! p[i].equals(parameters[i])) {
	  res = false;
	  break;
	}
    return res;
  }

  /**
   * Tests whether a given object equals this signature.
   * The object is considered equal if it is a signature
   * and it has the same method name and parameter list.
   *
   * @param compare Test object
   * @return <code>true</code> if the test object equals this signature.
   */
  public boolean equals(Object compare)
  {
    if (compare instanceof Signature)
    {
      Signature sig = (Signature)compare;
      return method.getName().equals( sig.getMethod().getName() ) &&
	     paramsEqual(sig.getMethod().getParameterTypes());
    }
    return false;
  }

  /**
   * Gets the method of which this is a signature.
   *
   * @return Method of which this is a signature.
   */
  public Method getMethod()
  {
    return method;
  }
    
  /** 
   *  Computes the basename of the specified class.  This returns
   *  "Object" from "java.lang.Object."  It returns the "single"
   *  form of an Array object. 
   *
   * @param type Class whose basename is required
   * 
   * @return basename
   */
  public final String baseName(Class type)
  {
    String name = null;
    if (type.isArray())
    {
      try
      {
        Class cl       = type;
        int dimensions = 0;
        while (cl.isArray())
        {
          cl = cl.getComponentType();
          dimensions++;
        }
        StringBuffer sb = new StringBuffer();
        sb.append(cl.getName());
        for (int i = 0; i < dimensions; i++)
          sb.append("[]");
        name = sb.toString();
      }
      catch (Throwable e)
      {
        name = type.getName();
      }
    }
    else
      name = type.getName();
      
    if (truncate)
    {
      int idx = name.lastIndexOf( '.' );
      if (idx > -1)
        return name.substring( idx + 1 );
    } // end of if (truncate)

    return name;
  }

  /** 
   * Gets a throw clause listing the exceptions thrown by this method.
   * 
   * @param except Vector of exceptions
   *
   * @return Exceptions thrown by this method.
   */
  private final String getExceptions(Class[] except)
  {
    if ((null == except) || (except.length == 0))
      return "";
    StringBuffer buf = new StringBuffer(" throws ");

    for (int i = 0; i < except.length; i++)
    {
      String type = baseName( except[i] );

      buf.append( type );
      if (i < except.length - 1)
        buf.append(", ");
    } // end of for (int i = 0; i < except.length; i++)
    return buf.toString();
  }

  /** 
   * Gets a parameter list for this method; parameters are named
   * by the NameFactory whose default implementation uses param1
   * .. paramn 
   *
   * @param params Parameters of this method
   *
   * @return Parameter list in string form
   */
  public final String getParameters (Class[] params)
  {
    if ((null == params) || (params.length == 0))
      return "";
    StringBuffer buf = new StringBuffer();
    NameFactory factory = registry.getNameFactory();

    for (int i = 0; i < params.length; i++)
    {
      String type = baseName( params[i] );
      String name = factory.getParameterName(params[i], i + 1, params.length );

      buf.append( type + " " + name );

      if (i < params.length - 1)
        buf.append(", ");
    } // end of for (int i = 0; i < params.length; i++)

    return buf.toString();
  }

  /** 
   * Gets list of modifiers for this method.
   * Interface methods are always public and may be synchronized 
   */ 
  private final String getModifiers ()
  {
    StringBuffer buf = new StringBuffer("public");
    int mod = method.getModifiers();

    if ( Modifier.isSynchronized( mod ) )
      buf.append(" synchronized");
    return buf.toString();
  }

  /** 
   * Register this Signature's types with the SignatureRegistry
   */
  private final void register()
  {
    if (null != registry)
    {
      registry.registerImport( declaring );
      registry.registerImport( return_value );
    
      for (int i = 0; i < parameters.length; i++)
        registry.registerImport( parameters[i] );

      for (int i = 0; i < exceptions.length; i++)
        registry.registerImport( exceptions[i] );
    }
  }

} // end of Signature class
