<!-- edited with XMLSPY v5 rel. 2 U (http://www.xmlspy.com) by Bogdan Jeram (E.S.O.) -->
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" xmlns:acserr="Alma/ACSError" xmlns:redirect="http://xml.apache.org/xalan/redirect" extension-element-prefixes="redirect">
	<xsl:output method="text" version="1.0" encoding="ASCII"/>
	
<!--                                                                BEGIN: template for members          -->
<xsl:template match="acserr:Member">
		 <xsl:variable name="MemberType">
		 <xsl:choose>
					<xsl:when test='@type="string"'>
					<xsl:text>String</xsl:text>
					</xsl:when>
					<xsl:when test='@type="long"'>
					<xsl:text>int</xsl:text>
					</xsl:when>
					<xsl:otherwise>
					<xsl:value-of select="@type"/>
					</xsl:otherwise>
		  </xsl:choose>
		</xsl:variable>
<!--                                                                           set member -->
<xsl:text>
        public void set</xsl:text>
	           <xsl:value-of select="@name"/>
				<xsl:text>(</xsl:text>
				<xsl:value-of select="$MemberType"/>
				<xsl:text> value)
	{ 
		setProperty("</xsl:text>
				 <xsl:value-of select="@name"/>
				<xsl:text>", ""+value);
	}            				
	</xsl:text>
	<!--                                                            get member -->
	<xsl:text>
	public </xsl:text>
	<xsl:value-of select="$MemberType"/>
					 <xsl:text> get</xsl:text>
	  	           <xsl:value-of select="@name"/>
	  	           <xsl:text>()</xsl:text>
        			<xsl:choose>
					<xsl:when test='@type="long"'>
					<xsl:text>
	{ 
		return Integer.parseInt(</xsl:text>
					</xsl:when>
					<xsl:when test='@type="double"'>
					<xsl:text> throws NumberFormatException 
	{ 
		return Double.parseDouble(</xsl:text>
					</xsl:when>
					<xsl:when test='@type="boolean"'>
					<xsl:text>
	{ 
		return Boolean.parseBoolean(</xsl:text>
					</xsl:when>
					<xsl:otherwise>
					<xsl:text>
	{ 
		return (</xsl:text>
					</xsl:otherwise>
		            </xsl:choose> 
		            <xsl:text>getProperty("</xsl:text>
        			<xsl:value-of select="@name"/>
    			    <xsl:text>")); 
	}
  </xsl:text> 
</xsl:template>
<!--                                                                END: template for members          -->	
	
<xsl:template match="/acserr:Type">
<xsl:variable name="NumCodes">
			<xsl:number value="count(//acserr:Code)"/>
</xsl:variable>
<!-- we generate type exception just in case if there is some error code -->
<xsl:if test="count(//acserr:ErrorCode[not(@_suppressExceptionGeneration)]) > 0">
<xsl:variable name="ClassName">
		<xsl:text>AcsJ</xsl:text><xsl:value-of select="@name"/><xsl:text>Ex</xsl:text>
	</xsl:variable>
<xsl:variable name="FileName">
	<xsl:value-of select="@_prefix"/><xsl:text>/</xsl:text><xsl:value-of select="@name"/><xsl:text>/wrappers/</xsl:text><xsl:value-of select="$ClassName"/><xsl:text>.java</xsl:text>
</xsl:variable>
<redirect:write select="$FileName">

<xsl:text>/*
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
 * 
 * ========================================================================
 * == THIS IS GENERATED CODE!!! DO NOT MODIFY! ALL CHANGES WILL BE LOST! ==
 * ======================================================================== 
 */
package </xsl:text>
 <xsl:value-of select="/acserr:Type/@_prefix"/>
<xsl:text>.</xsl:text>
<xsl:value-of select="/acserr:Type/@name"/>
<xsl:text>.wrappers;

import alma.acs.exceptions.*;
//import alma.ACSErr.ACSErrType;
import alma.ACSErr.ErrorTrace;
import </xsl:text>
<xsl:value-of select="/acserr:Type/@_prefix"/>
<xsl:text>.</xsl:text>
<xsl:value-of select="/acserr:Type/@name"/>
<xsl:text>.</xsl:text>
<xsl:value-of select="/acserr:Type/@name"/>
<xsl:text>Ex;


/**
 * Java native style exception class representing the error type 
 * &lt;code&gt;alma.ACSErr.ACSErrType.ACSErrTypeTest&lt;/code&gt;.    
 * &lt;p&gt; 
 * This class is abstract, so that individual classes for each error code
 * within the given error type can be generated as subclasses.
 * They are interoperable with {@link alma.ACSErr.ErrorTrace}.
 * The purpose is to bring back to Java a limited hierarchy of exceptions, which can
 * only be mimicked in CORBA by using a twofold number scheme with 
 * "type" and "code".
 * &lt;p&gt; 
 * @author ACS Error System Code Generator
 * created Sep 25, 2003 4:18:09 PM
 */
public abstract class </xsl:text>
<xsl:value-of select="$ClassName"/>
<xsl:text> extends AcsJException
{

	/////////////////////////////////////////////////////////////
	// Implementation of Constructors from AcsJException
	// most of which are derived from java.lang.Throwable
	/////////////////////////////////////////////////////////////

	public </xsl:text> 
<xsl:value-of select="$ClassName"/>
<xsl:text>()
	{
		super();
	}

        /**
          * @deprecated The data in &lt;code&gt;message&lt;/code&gt; should be given as parameters instead!
          */
	public </xsl:text>
<xsl:value-of select="$ClassName"/>
<xsl:text>(String message)
	{
		super(message);
	}

        /**
         * @deprecated The data in &lt;code&gt;message&lt;/code&gt; should be given as parameters instead!
         */
	public </xsl:text>
<xsl:value-of select="$ClassName"/>
<xsl:text>(String message, Throwable cause)
	{
		super(message, cause);
	}

	public </xsl:text>
<xsl:value-of select="$ClassName"/>
<xsl:text>(Throwable cause)
	{
		super(null, cause);
	}

	public </xsl:text>
<xsl:value-of select="$ClassName"/>
<xsl:text>(ErrorTrace etCause)
	{
		super(etCause);
	}

        /**
         * @deprecated The data in &lt;code&gt;message&lt;/code&gt; should be given as parameters instead!
         */
	public </xsl:text>
<xsl:value-of select="$ClassName"/>
<xsl:text>(String message, ErrorTrace etCause)
	{
		super(message, etCause);
	}

	/////////////////////////////////////////////////////////////
	// Mapping of Error Type 
	/////////////////////////////////////////////////////////////

	/**
	 * Returns the error type, which is fixed to 
	 * &lt;code&gt;ACSErrType.ACSErrTypeTest&lt;/code&gt;. 
	 * 
	 * @see alma.acs.exceptions.AcsJException#getErrorType()
	 */
	protected final int getErrorType()
	{
		return </xsl:text>
		<xsl:value-of select="/acserr:Type/@_prefix"/>
		<xsl:text>.ACSErr.</xsl:text>
		<xsl:value-of select="/acserr:Type/@name"/>
		<xsl:text>.value;
	}

	/**
	 * Returns the CORBA exception that corresponds to the error type
	 * (and not to any particular error code).
	 * Note that if CORBA exceptions would support inheritance,
	 * the error code-specific CORBA exceptions should inherit from this one,	 
	 * which unfortunately they can't.
	 */
	public </xsl:text>
<xsl:value-of select="@name"/>
<xsl:text>Ex to</xsl:text>
<xsl:value-of select="@name"/>	
	<xsl:text>Ex()
	{
		ErrorTrace et = getErrorTrace(); 
		</xsl:text>
		<xsl:value-of select="@name"/>	
		<xsl:text>Ex acsEx = new </xsl:text>
		<xsl:value-of select="@name"/>	
		<xsl:text>Ex(et);
		return acsEx;
	}

}  </xsl:text>


</redirect:write>
</xsl:if>


<!-- **************************************************************************************************************-->
<!-- ***************************************** Code generation for each code in a separate file   ******************-->
<!-- **************************************************************************************************************-->

<!-- *********************************************************Generated code for Codes has to be addded  -->

<xsl:for-each select="acserr:ErrorCode[not(@_suppressExceptionGeneration)]">

<xsl:variable name="ClassNameEx">
	<xsl:text>AcsJ</xsl:text><xsl:value-of select="@name"/><xsl:text>Ex</xsl:text>
</xsl:variable>

<xsl:variable name="FileName">
	<xsl:value-of select="/acserr:Type/@_prefix"/><xsl:text>/</xsl:text><xsl:value-of select="/acserr:Type/@name"/><xsl:text>/wrappers/</xsl:text><xsl:value-of select="$ClassNameEx"/><xsl:text>.java</xsl:text>
</xsl:variable>
<redirect:write select="$FileName">

<xsl:text>/*
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
 * 
 * ========================================================================
 * == THIS IS GENERATED CODE!!! DO NOT MODIFY! ALL CHANGES WILL BE LOST! ==
 * ======================================================================== 
 */
 
package </xsl:text>
 <xsl:value-of select="/acserr:Type/@_prefix"/>
<xsl:text>.</xsl:text>
<xsl:value-of select="/acserr:Type/@name"/>
<xsl:text>.wrappers;

import org.omg.CORBA.UserException; 
import alma.acs.exceptions.*;

import alma.ACSErr.ErrorTrace;
import </xsl:text>
<xsl:value-of select="/acserr:Type/@_prefix"/>
<xsl:text>.</xsl:text>
<xsl:value-of select="/acserr:Type/@name"/>
<xsl:text>.</xsl:text><xsl:value-of select="@name"/>

<xsl:text>;
import </xsl:text>
<xsl:value-of select="/acserr:Type/@_prefix"/>
<xsl:text>.</xsl:text>
<xsl:value-of select="/acserr:Type/@name"/>
<xsl:text>.</xsl:text>
<xsl:value-of select="@name"/>
<xsl:text>Ex;

/**
 * Java native style exception class representing the error type 
 * &lt;code&gt;alma.ACSErr.ACSErrType.</xsl:text>
 <xsl:value-of select="/acserr:Type/@name"/>
 <xsl:text>&lt;/code&gt;,
 * error code &lt;code&gt;</xsl:text>
 <xsl:value-of select="@name"/>
  <xsl:text>.value&lt;/code&gt;.     
 * 
 * @see AcsJ</xsl:text>
 <xsl:value-of select="/acserr:Type/@name"/>
 <xsl:text>Ex
 * 
 * @author ACS Error System Code Generator
 * created Sep 25, 2003 4:18:09 PM
 */
public class </xsl:text>
<xsl:value-of select="$ClassNameEx"/>
<xsl:text> extends AcsJ</xsl:text>
<xsl:value-of select="/acserr:Type/@name"/>
<xsl:text>Ex
{

	public </xsl:text> 
<xsl:value-of select="$ClassNameEx"/>
<xsl:text>()
	{
		super();
	}
	
        /**
         * @deprecated The data in &lt;code&gt;message&lt;/code&gt; should be given as parameters instead!
         */
	public </xsl:text>
<xsl:value-of select="$ClassNameEx"/>
<xsl:text>(String message)
	{
		super(message);
	}

        /**
         * @deprecated The data in &lt;code&gt;message&lt;/code&gt; should be given as parameters instead!
         */
	public </xsl:text>
<xsl:value-of select="$ClassNameEx"/>
<xsl:text>(String message, Throwable cause)
	{
		super(message, cause);
	}

	public </xsl:text>
<xsl:value-of select="$ClassNameEx"/>
<xsl:text>(Throwable cause)
	{
		super(null, cause);
	}

	public </xsl:text>
<xsl:value-of select="$ClassNameEx"/>
<xsl:text>(ErrorTrace etCause)
	{
		super(etCause);
	}

        /**
         * @deprecated The data in &lt;code&gt;message&lt;/code&gt; should be given as parameters instead!
         */
	public </xsl:text>
<xsl:value-of select="$ClassNameEx"/>
<xsl:text>(String message, ErrorTrace etCause)
	{
		super(message, etCause);
	}


	/////////////////////////////////////////////////////////////
	// Code specific methods
	/////////////////////////////////////////////////////////////
  
       /**
        * Returns the short description of the error
        */
	public String getShortDescription() 
	{
	   return "</xsl:text>
	   <xsl:value-of select="@shortDescription"/>
	   <xsl:text>";
	}
    
	/**
	 * Returns the error code, which is fixed to &lt;code&gt;</xsl:text>
	 <xsl:number value="$NumCodes+position()-1"/>
	 <xsl:text>&lt;/code&gt;, given by
	 * &lt;code&gt;</xsl:text>
 	<xsl:value-of select="@name"/>
	 <xsl:text>.value&lt;/code&gt;. 
	 * 
	 * @see alma.acs.exceptions.AcsJException#getErrorCode()
	 */ 
	protected final int getErrorCode()
	{
		return </xsl:text>
	<xsl:value-of select="@name"/> 
	 <xsl:text>.value;
	 }

	/**
	 * @see alma.acs.exceptions.AcsJException#toCorbaException()
	 */
	public UserException toCorbaException()
	{
		return to</xsl:text>
<xsl:value-of select="@name"/> 		
<xsl:text>Ex();
	}
	
	/**
	 * Creates an &lt;code&gt;</xsl:text>
	 <xsl:value-of select="@name"/> 	
	 <xsl:text>&lt;/code&gt; that represents this exception
	 * with all its caused-by child exceptions.
	 * &lt;p&gt;
	 * Typically to be called from a top-level catch block that must
	 * convert any of the Java exceptions used internally by the Java program
	 * to an IDL type exception that can be thrown on over CORBA.
	 *   
	 * @return the type-safe subclass of &lt;code&gt;org.omg.CORBA.UserException&lt;/code&gt; 
	 * 			with an embedded &lt;code&gt;ErrorTrace&lt;/code&gt;.
	 * @see #toCorbaException
	 */
	public </xsl:text>
<xsl:value-of select="@name"/>
<xsl:text>Ex to</xsl:text>
<xsl:value-of select="@name"/>	
	<xsl:text>Ex()
	{
		ErrorTrace et = getErrorTrace(); 
		</xsl:text>
		<xsl:value-of select="@name"/>	
		<xsl:text>Ex acsEx = new </xsl:text>
		<xsl:value-of select="@name"/>	
		<xsl:text>Ex(et);
		return acsEx;
	}
	
	
	/**
	 * Converts a CORBA &lt;code&gt;</xsl:text>
		<xsl:value-of select="@name"/>	
	<xsl:text>&lt;/code&gt; to an instance of this class.
	 * &lt;p&gt;
	 * Note that unlike the constructor {@link #AcsJErrTest0Ex(ErrorTrace)}, 
	 * this static conversion method
	 * does not wrap the existing chain of exceptions with a new exception. 
	 * It simply converts all exceptions found in the &lt;code&gt;ErrorTrace&lt;code&gt;
	 * of &lt;code&gt;corbaEx&lt;/code&gt; to the corresponding Java exceptions, knowing that 
	 * the top level exception is of type &lt;code&gt;</xsl:text>
		<xsl:value-of select="@name"/>	
	<xsl:text>&lt;/code&gt; and can 
	 * always be converted to &lt;code&gt;</xsl:text>
		<xsl:value-of select="$ClassNameEx"/>	
	<xsl:text>&lt;/code&gt;.
	 * &lt;p&gt;
	 * Here's an example of how to use this method in a Java program that 
	 * makes a call so some other component etc.:
	 * &lt;pre&gt;
	 * private void methodThatMakesARemoteCall() throws </xsl:text>
		<xsl:value-of select="$ClassNameEx"/>	
	<xsl:text>
	* {
	 *   try
	 *   {
	 *     // this fakes the remote call to a method
	 *     // which can throw an </xsl:text>
		<xsl:value-of select="@name"/>	
	<xsl:text>Ex...
	 *     throw new </xsl:text>
		<xsl:value-of select="@name"/>	
	<xsl:text>Ex();
	 *   }
	 *   catch (</xsl:text>
		<xsl:value-of select="@name"/>	
	<xsl:text>Ex corbaEx)
	 *   {
	 *     throw </xsl:text>
		<xsl:value-of select="$ClassNameEx"/>	
	<xsl:text>.from</xsl:text>
		<xsl:value-of select="@name"/>	
	<xsl:text>Ex(corbaEx);
	 *   }
	 * } 
	 * &lt;/pre&gt; 
	 * @param corbaEx the CORBA equivalent of this class; will be converted
	 * @return  the newly created instance, with data fields and caused-by exceptions 
	 * 			converted from &lt;code&gt;corbaEx&lt;/code&gt;. 
	 */
	public static </xsl:text>
<xsl:value-of select="$ClassNameEx"/>
	<xsl:text> from</xsl:text>
	<xsl:value-of select="@name"/>	
	<xsl:text>Ex(</xsl:text>
	<xsl:value-of select="@name"/>	
	<xsl:text>Ex corbaEx)
	{
		ErrorTrace et = corbaEx.errorTrace;
		
		String message = ErrorTraceManipulator.getProperty(
			et, CorbaExceptionConverter.PROPERTY_JAVAEXCEPTION_MESSAGE);
		</xsl:text>
<xsl:value-of select="$ClassNameEx"/>
<xsl:text> jEx = new </xsl:text>
<xsl:value-of select="$ClassNameEx"/>
<xsl:text>(message);
		
		CorbaExceptionConverter.convertErrorTraceToJavaException(et, jEx);
		
		return jEx;
	}
	
	/////////////////////////////////////////////////////////////
	// Getter/Setter for members
	/////////////////////////////////////////////////////////////	
</xsl:text>
<!--             *******************************************************                                    members -->
<xsl:apply-templates/>		
<xsl:text>
} 
</xsl:text>

</redirect:write>
<!-- *********************************************************Generated Completion code for  Codes  -->

<xsl:variable name="ClassNameCompl">
	<xsl:value-of select="@name"/><xsl:text>AcsJCompletion</xsl:text>
</xsl:variable>

<xsl:variable name="FileName">
	<xsl:value-of select="/acserr:Type/@_prefix"/><xsl:text>/</xsl:text><xsl:value-of select="/acserr:Type/@name"/><xsl:text>/wrappers/</xsl:text><xsl:value-of select="$ClassNameCompl"/><xsl:text>.java</xsl:text>
</xsl:variable>
<redirect:write select="$FileName">

<xsl:text>/*
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
 *
 * ========================================================================
 * == THIS IS GENERATED CODE!!! DO NOT MODIFY! ALL CHANGES WILL BE LOST! ==
 * ======================================================================== 
 *
 */
 
package </xsl:text>
 <xsl:value-of select="/acserr:Type/@_prefix"/>
<xsl:text>.</xsl:text>
<xsl:value-of select="/acserr:Type/@name"/>
<xsl:text>.wrappers;

import alma.ACSErr.Completion;
import alma.acs.exceptions.*;
import </xsl:text>
<xsl:value-of select="/acserr:Type/@_prefix"/>
<xsl:text>.</xsl:text>
<xsl:value-of select="/acserr:Type/@name"/>
<xsl:text>.wrappers.AcsJ</xsl:text>
<xsl:value-of select="@name"/>
<xsl:text>Ex;

public class </xsl:text>
<xsl:value-of select="$ClassNameCompl"/>
<xsl:text> extends AcsJCompletion
{
	/**
	 * Creates a new &lt;code&gt;</xsl:text>
<xsl:value-of select="$ClassNameCompl"/>
<xsl:text>&lt;/code&gt;
	 * with a corresponding exception (</xsl:text>
<xsl:value-of select="$ClassNameEx"/>
<xsl:text>) attached.
	 */
	public </xsl:text>
<xsl:value-of select="$ClassNameCompl"/>	
<xsl:text> () {
		super(new </xsl:text>
<xsl:value-of select="$ClassNameEx"/>	
<xsl:text>()); 
	}

	/**
	 * Creates a new &lt;code&gt;</xsl:text>
<xsl:value-of select="$ClassNameCompl"/>
<xsl:text>&lt;/code&gt;
	 * with a corresponding exception (</xsl:text>
<xsl:value-of select="$ClassNameEx"/>
 <xsl:text>) attached 
	 * that wraps an existing exception (&lt;code&gt;acsJEx&lt;/code&gt;.)
	 */
	public </xsl:text>
<xsl:value-of select="$ClassNameCompl"/>
<xsl:text> (AcsJException acsJEx) {
		super(new </xsl:text>
<xsl:value-of select="$ClassNameEx"/>		
<xsl:text>(acsJEx));  
	}
	
	/**
	 * Creates a new &lt;code&gt;</xsl:text>
<xsl:value-of select="$ClassNameCompl"/>
 <xsl:text>&lt;/code&gt;
	 * from another &lt;code&gt;AcsJCompletion&lt;/code&gt; (&lt;code&gt;acsJComp&lt;/code&gt;).
	 * &lt;p&gt;
	 * If present, the existing error trace is converted to Java exceptions
	 * and wrapped with an &lt;code&gt;</xsl:text>
<xsl:value-of select="$ClassNameEx"/>
<xsl:text>&lt;/code&gt;.
	 */
	public </xsl:text>
<xsl:value-of select="$ClassNameCompl"/>
<xsl:text>(AcsJCompletion acsJComp) {		
			init(new </xsl:text>
<xsl:value-of select="$ClassNameEx"/>
<xsl:text>(acsJComp.getAcsJException()));
	}
	
	/**
	 * Converts a Corba completion to an &lt;code&gt;</xsl:text>
<xsl:value-of select="$ClassNameCompl"/>
<xsl:text>&lt;/code&gt;
	 * such that a new &lt;code&gt;</xsl:text>
<xsl:value-of select="$ClassNameEx"/>
<xsl:text>&lt;/code&gt; is created as the attached error.
	 * If &lt;code&gt;corbaComp&lt;/code&gt; carries error information, these &lt;code&gt;ErrorTrace&lt;/code&gt;
	 * objects are converted to Java exceptions, which are attached as the course of
	 * the new &lt;code&gt;</xsl:text>
<xsl:value-of select="$ClassNameEx"/>
<xsl:text>&lt;/code&gt;.
	 * @param corbaComp
	 */
	public </xsl:text>
<xsl:value-of select="$ClassNameCompl"/>
<xsl:text>(Completion corbaComp) {
		
		 this(AcsJCompletion.fromCorbaCompletion(corbaComp));
	}
	
       /**
        * Returns the short description of the error
        */
	public String getShortDescription() 
	{
	   return "</xsl:text>
	   <xsl:value-of select="@shortDescription"/>
	   <xsl:text>";
	}	

	/////////////////////////////////////////////////////////////
	// Getter/Setter for members
	/////////////////////////////////////////////////////////////	
</xsl:text>
<!--             *******************************************************                                    members -->
<xsl:apply-templates/>		
<xsl:text>
}
</xsl:text>
</redirect:write>
<!-- *********************************************************Generated Completion code for Codes  END-->
</xsl:for-each>


<!-- *********************************************************Generated code for Error-Free Codes  -->

<xsl:for-each select="acserr:Code">

<xsl:variable name="ClassName">
	<xsl:value-of select="@name"/><xsl:text>AcsJCompletion</xsl:text>
</xsl:variable>

<xsl:variable name="FileName">
	<xsl:value-of select="/acserr:Type/@_prefix"/><xsl:text>/</xsl:text><xsl:value-of select="/acserr:Type/@name"/><xsl:text>/wrappers/</xsl:text><xsl:value-of select="$ClassName"/><xsl:text>.java</xsl:text>
</xsl:variable>
<redirect:write select="$FileName">

<xsl:text>/*
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
 *
 * ========================================================================
 * == THIS IS GENERATED CODE!!! DO NOT MODIFY! ALL CHANGES WILL BE LOST! ==
 * ======================================================================== 
 *
 */
 
package </xsl:text>
 <xsl:value-of select="/acserr:Type/@_prefix"/>
<xsl:text>.</xsl:text>
<xsl:value-of select="/acserr:Type/@name"/>
<xsl:text>.wrappers;

import alma.acs.exceptions.*;
import </xsl:text>
<xsl:value-of select="/acserr:Type/@_prefix"/>
<xsl:text>.</xsl:text>
<xsl:value-of select="/acserr:Type/@name"/>
<xsl:text>.</xsl:text><xsl:value-of select="@name"/>

<xsl:text>;

/**
 * @author ACS Error System Code Generator
 * created Jan 21, 2004 1:46:41 PM
 */
public class </xsl:text>
<xsl:value-of select="$ClassName"/>
<xsl:text> extends AcsJCompletion
{
	/**
	 * @param type
	 * @param code
	 */ 
	public </xsl:text> 
<xsl:value-of select="$ClassName"/>
<xsl:text>()
	{
		super(</xsl:text>
		<xsl:value-of select="/acserr:Type/@_prefix"/>
		<xsl:text>.ACSErr.</xsl:text>
		<xsl:value-of select="/acserr:Type/@name"/>
		<xsl:text>.value, </xsl:text>
 		 <xsl:value-of select="@name"/> 
		<xsl:text>.value);
	}
	
	/**
        * Returns the short description of the error
        */
	public String getShortDescription() 
	{
	   return "</xsl:text>
	   <xsl:value-of select="@shortDescription"/>
	   <xsl:text>";
	}
<!--
to be uncommented latter if we found that name value pairs should be used in non error completion as well.	
	/////////////////////////////////////////////////////////////
	// Getter/Setter for members
	/////////////////////////////////////////////////////////////	
</xsl:text>
             *******************************************************                                    members 
<xsl:apply-templates/>		
<xsl:text>
-->
}
</xsl:text>


</redirect:write>
</xsl:for-each>


<!-- *********************************************************Generated code for Error-Free Codes  END-->

</xsl:template>
</xsl:stylesheet>
