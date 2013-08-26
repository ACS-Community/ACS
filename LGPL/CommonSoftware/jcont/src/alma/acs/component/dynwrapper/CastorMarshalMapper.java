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

import java.io.StringWriter;
import java.util.logging.Logger;

import org.exolab.castor.xml.Marshaller;

import alma.acs.entityutil.EntityException;
import alma.acs.entityutil.EntityTFinder;
import alma.entities.commonentity.EntityT;
import alma.xmlentity.XmlEntityStruct;


/**
 * A type mapper that can marshal a Castor generated entity object
 * to the IDL struct used for CORBA transport (<code>XmlEntityStruct</code>).
 * 
 * @author hsommer
 */
public class CastorMarshalMapper extends TypeMapper
{

	private EntityTFinder m_entityTFinder;


	/**
	 * @see alma.acs.component.dynwrapper.TypeMapper#TypeMapper(java.lang.Object, Logger)
	 */
	public CastorMarshalMapper(Object delegate, Logger logger)
	{
		super(delegate, logger);
		m_entityTFinder = new EntityTFinder(logger);
		m_entityTFinder.setVerbose(m_verbose);
	}
	
	
	/**
	 * True if <code>oldObjClass</code> is a Castor generated entity class
	 * that has a child of type <code>EntityT</code>, 
	 * and if <code>newObjClass</code> is the <code>XmlEntityStruct</code> class.
	 * 
	 * @see alma.acs.component.dynwrapper.TypeMapper#canTranslate(java.lang.Class, java.lang.Class, 
	 * alma.acs.component.dynwrapper.ComponentInvocationHandler)
	 */
	public boolean canTranslate(Class<?> oldObjClass, Class<?> newObjClass, ComponentInvocationHandler invHandler)
	{
		boolean canTranslate = true;
		
		try
		{
			// check if oldObjClass has a child of type EntityT (or subtype)
			m_entityTFinder.getEntityTMethod(oldObjClass);
			
			// check if newObjClass is XmlEntityStruct
			// we don't trust Class#equals() here; we really want it to be XmlEntityStruct and not a subclass,
			// but that can't happen anyway since IDL structs can't be subclassed. 
			if (!XmlEntityStruct.class.isAssignableFrom(newObjClass))
			{
				throw new DynWrapperException();
			}
		}
		catch (Exception ex)
		{
			canTranslate = false;
		}
		
		if (m_verbose)
		{
			m_logger.finest("can " + (canTranslate ? "" : "not ") + "translate from class '" + oldObjClass.getName()  + 
						"' to class '" + newObjClass.getName());
		}
		
		return canTranslate;
	}


	/**
	 * Converts a Castor generated entity object to the marshalled representation as
	 * an <code>XmlEntityStruct</code>.
	 *  
	 * @see alma.acs.component.dynwrapper.TypeMapper#translate(java.lang.Object, java.lang.Object, java.lang.Class,
	 * alma.acs.component.dynwrapper.ComponentInvocationHandler)
	 */
	@SuppressWarnings("unchecked")
	public Object translate(Object oldObject, Object newObjectTemplate, 
							Class newObjectClass, ComponentInvocationHandler invHandler)
		throws DynWrapperException
	{
		if (oldObject == null) {
			return null;
		}
		
		EntityT entityMeta;

		try
		{
			entityMeta = m_entityTFinder.extractEntityT(oldObject);
		}
		catch (EntityException e)
		{
			String extractionErrMsg = "failed to extract EntityT object from " + 
										oldObject.getClass().getName();
			throw new DynWrapperException(extractionErrMsg, e);
		}
		
		if (entityMeta == null)
		{
			String extractionNullErrMsg = "entity object of type " + oldObject.getClass().getName() +
					" must have a non-null child assignable to " + EntityT.class.getName() + " to be serialized.";
			throw new DynWrapperException(extractionNullErrMsg);
		}
		
		XmlEntityStruct entStruct = null;
		
		// TODO: check if newObjectTemplate is needed here at all, or if those cases would always be
		// dealt with using a Holder class. Just confused right now...
		if (newObjectTemplate != null)
		{
			if (!XmlEntityStruct.class.isInstance(newObjectTemplate))
			{
				throw new DynWrapperException("invalid template for XmlEntityStruct object");
			}
			entStruct = (XmlEntityStruct) newObjectTemplate;
		}
		else
		{
			entStruct = new XmlEntityStruct();
		} 
		
		entStruct.entityId = entityMeta.getEntityId();
		entStruct.entityTypeName = entityMeta.getEntityTypeName(); 
		entStruct.schemaVersion = ( entityMeta.getSchemaVersion() != null ? entityMeta.getSchemaVersion() : "");  
		try
		{
			StringWriter wr = new StringWriter();
			Marshaller marsh = new Marshaller(wr);
			marsh.setValidation(false);
			marsh.marshal(oldObject);
			entStruct.xmlString = wr.toString();
		}
		catch (Exception e)
		{
			String msg = "failed to marshal entity object with id='" + entityMeta.getEntityId() +
							"' using the Castor Marshaller without validation.";
			throw new DynWrapperException(msg);
		}

		if (m_verbose)
		{
			m_logger.finer("successfully translated from '" + oldObject.getClass().getName()  + "' to '" + 
							entStruct.getClass().getName() + "'.");
		}
		return entStruct;
	}

}
