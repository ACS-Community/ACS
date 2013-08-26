net.pacbell.edf2.tools.SignatureFactory

USAGE:
  jre -cp SignatureFactory.jar net.pacbell.edf2.tools.SignatureFactory [-ndt] <some.java.interface> [<additional.java.interface> ...]

SYNOPSIS:
  -n suppress newline before opening brace in method stub.
  -d suppress javadoc output.
  -t suppress truncation of package information in types.

OVERVIEW:
SignatureFactory uses Java's Reflection API to generate method
implementation stubs for Java interfaces.  Its output includes:

* method stubs with modifiers, arguments, and exception declarations.
  Identical method stubs are rendered only once in cases where a
  complex interface inheritance graph causes the same method signature
  to be reflected multiple times.

* (optional) javadoc stubs for each method with named @param and
  @exception tags where appropriate;

* an import list for the dependencies of the methods defined in each
  of the interfaces processed by the Factory.  Dependencies are rendered
  uniquely no matter how many times a particular type occurs in 
  different methods.

* optional expansion/truncation of types to include/exclude fully
  qualified package names.  java.lang.Object <=> Object

* user configurable placement of the opening brace in method stubs.

* // style comments indicating what interface a set of methods is
  derived from.

* // style comments inside stub braces reminding the developer to
  supply an implementation.

* includes <description> placeholders for comments in javadoc stubs.
  These will be ignored by web browsers until you replace them with
  your own text (which, of course, you should).

APIs:

The main() method in the SignatureFactory class provides all of the
features listed above from a command line interface.  The Factory
itself, however, can be used (and reused) in applications that would
like to generate Signatures for discrete sets of intefaces.

Additionally, the SignatureFactory accepts user-defined
implementations of the net.pacbell.edf2.tools.NameFactory interface
for the purposes of generating method parameter names.  This interface
defines the following method:

/** Returns a unique (descriptive?) parameter name for the specified
 * type.
 *
 * @param type - the parameter type
 * @param param_num - the parameter's number in its method Signature
 * @param total_params - the total number of parameters in the Signature.
 */
public String getParameterName( Class type, int param_num, int total_params );

Implementors may use this interface to create new schemes for naming 
generated method parameters.  For example: 

// Name unique AWTEvent parameters "evt"
if ( (AWTEvent.class.isAssignableFrom( type )) && ( 0 == total_params ) )
  return "evt";

You can also subclass the DefaultNameFactory implementation of NameFactory
if you want to use its behavior (returning "param" + param_num) as
a fallback.

EXAMPLES:

% jre -cp sigfac/SignatureFactory.jar net.pacbell.edf2.tools.SignatureFactory java.util.Enumeration
import java.util.Enumeration;
// implementation of java.util.Enumeration interface

/**
 *
 * @return <description>
 */
public boolean hasMoreElements()
{
  // TODO: implement this java.util.Enumeration method
}

/**
 *
 * @return <description>
 */
public Object nextElement()
{
  // TODO: implement this java.util.Enumeration method
}

% jre -cp sigfac/SignatureFactory.jar net.pacbell.edf2.tools.SignatureFactory -ntd java.util.Enumeration
import java.util.Enumeration;
// implementation of java.util.Enumeration interface

public boolean hasMoreElements() {
  // TODO: implement this java.util.Enumeration method
}

public java.lang.Object nextElement() {
  // TODO: implement this java.util.Enumeration method
}

% jre -cp sigfac/SignatureFactory.jar net.pacbell.edf2.tools.SignatureFactory java.awt.event.MouseMotionListener java.awt.event.MouseListener

import java.awt.event.MouseMotionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
// implementation of java.awt.event.MouseListener interface

/**
 *
 * @param param1 <description>
 */
public void mouseClicked(MouseEvent param1)
{
  // TODO: implement this java.awt.event.MouseListener method
}

/**
 *
 * @param param1 <description>
 */
public void mousePressed(MouseEvent param1)
{
  // TODO: implement this java.awt.event.MouseListener method
}

/**
 *
 * @param param1 <description>
 */
public void mouseReleased(MouseEvent param1)
{
  // TODO: implement this java.awt.event.MouseListener method
}

/**
 *
 * @param param1 <description>
 */
public void mouseEntered(MouseEvent param1)
{
  // TODO: implement this java.awt.event.MouseListener method
}

/**
 *
 * @param param1 <description>
 */
public void mouseExited(MouseEvent param1)
{
  // TODO: implement this java.awt.event.MouseListener method
}
// implementation of java.awt.event.MouseMotionListener interface

/**
 *
 * @param param1 <description>
 */
public void mouseDragged(MouseEvent param1)
{
  // TODO: implement this java.awt.event.MouseMotionListener method
}

/**
 *
 * @param param1 <description>
 */
public void mouseMoved(MouseEvent param1)
{
  // TODO: implement this java.awt.event.MouseMotionListener method
}
