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
package alma.acs.component.dynwrapper;

import java.io.StringReader;
import java.util.logging.Logger;

import org.exolab.castor.xml.Unmarshaller;
import org.exolab.castor.xml.util.ClassDescriptorResolverImpl;

import alma.xmlentity.XmlEntityStruct;

/**
 * A type mapper that takes the IDL struct used for CORBA transport of xml entity objects (<code>XmlEntityStruct</code>)
 * and instantiates the correct Castor binding class from the stringified xml.
 * 
 * @author hsommer Dec 4, 2002 6:38:13 PM
 */
public class CastorUnmarshalMapper extends TypeMapper
{

	/**
	 * Constructor for CastorUnmarshalMapper.
	 * @param delegate
	 */
	public CastorUnmarshalMapper(Object delegate, Logger logger)
	{
		super(delegate, logger);
	}

	/**
	 * @see alma.acs.component.dynwrapper.TypeMapper#canTranslate(java.lang.Class, java.lang.Class, alma.acs.component.dynwrapper.ComponentInvocationHandler)
	 */
	public boolean canTranslate(
		Class<?> oldObjClass,
		Class<?> newObjClass,
		ComponentInvocationHandler invHandler)
	{
		boolean canTranslate = false;
		
		if (XmlEntityStruct.class.isAssignableFrom(oldObjClass))
		{
			// check if the Castor framework has the classes available to instantiate the xml as binding objects 
			ClassDescriptorResolverImpl clDescRes = new ClassDescriptorResolverImpl();
			clDescRes.setIntrospection(false);
			if (clDescRes.resolve(newObjClass) != null)
			{
				canTranslate = true;
			}
		}
		
		if (m_verbose)
		{
			String msg = "can " + (canTranslate ? "" : "not ") + "translate from class '" + oldObjClass.getName()  + 
						"' to class '" + newObjClass.getName() + "'.";
			m_logger.finer(msg);
		}	
		return canTranslate;
	}


	/**
	 * Translates an <code>XmlEntityStruct</code> to a Castor binding object graph.
	 * <p>
	 * This will only happen for <code>in</code>-parameters (server-side) or return values (client-side), 
	 * since xml entities as <code>out</code> or <code>inout</code>-parameters will be mapped to 
	 * <code>XmlEntityStructHolder</code>s instead of <code>XmlEntityStruct</code>s.
	 * Therefore preserving object identity (parameter <code>newObjectTemplate</code>) is not an issue.
	 *<p>
	 * Returns null if the <code>XmlEntityStruct</code> or the contained XML is null, or if the XML string
	 * is empty or whitespace only. Throws an exception if the XML is syntactically invalid. 
	 *
	 * @see alma.acs.component.dynwrapper.TypeMapper#translate(java.lang.Object, java.lang.Object, java.lang.Class, 
	 * 		alma.acs.component.dynwrapper.ComponentInvocationHandler)
	 */
	public <T> Object translate(
		Object oldObject,
		T newObjectTemplate,
		Class<T> newObjectClass,
		ComponentInvocationHandler invHandler)
		throws DynWrapperException
	{
		XmlEntityStruct entStruct = (XmlEntityStruct) oldObject;
		
		if (entStruct == null || entStruct.xmlString == null || entStruct.xmlString.trim().length() == 0)
		{
			return null;
		}
		
		// todo (for support of entity version conversions) 
		// check entStruct.schemaVersion and use a previous version of newObjectClass if required,
		// e.g. the respective class from a different Java package that denotes the vintage version     
		Unmarshaller unmarsh = new Unmarshaller(newObjectClass);
		unmarsh.setValidation(false);
		unmarsh.setWhitespacePreserve(true);
		Object entity;
		try {
			entity = unmarsh.unmarshal(new StringReader(entStruct.xmlString));
		}
		catch (Exception ex) {
			
			String errorMsg = "failed to unmarshal entity object of type '" + 
								entStruct.entityTypeName + "' using the Castor framework. ";
			if (ex.getMessage().trim().startsWith("unable to find FieldDescriptor for") ) {
				errorMsg += "This is likely a versioning problem, where the XML document contains data types " +
				"which are no longer allowed by the current XML schema, " + 
				"and are therefore unknown to the generated Castor binding classes. ";
			}
			String xmlMsg = null;
			if (entStruct.xmlString.length() <= 300)
			{
				xmlMsg = "XML string=\n" + entStruct.xmlString;
			}
			else
			{
				xmlMsg = "XML string (first 300 chars) =\n" + 
							entStruct.xmlString.substring(0, 300) + "***TRUNCATED|";
			}
			throw new DynWrapperException(errorMsg + xmlMsg, ex);
		}
		return entity;
	}

}
