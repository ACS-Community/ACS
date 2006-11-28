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
package alma.acs.entityutil;

import java.io.StringReader;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.exolab.castor.xml.Unmarshaller;

import alma.entities.commonentity.EntityT;
import alma.xmlentity.XmlEntityStruct;

/**
 * Helper class to convert serialized XML entities from the ALMA project data model (APDM) 
 * to their Java binding class representations.
 * <p>
 * Note that normally a Java component does not need to explicitly perform this conversion, 
 * since the Java container translates between the XML-string representation that goes over CORBA,
 * and the binding class (tree) that the custom-generated component's Java interface uses 
 * as parameters or return value. 
 * <p>
 * However, there is a case where a Java component should use the <code>EntityDeserializer</code>:
 * Applications receiving XML data from the ALMA archive: the archive does currently not offer a type-safe
 * interface for APDM entities. Use {@link #deserializeEntity(XmlEntityStruct, Class)} to convert
 * the <code>XmlEntityStruct</code> you get from the archive into a Java binding object tree
 * whose root node is an instance of the specified <code>Class</code>. 
 *        
 * @author hsommer May 6, 2003 5:31:57 PM
 */
public class EntityDeserializer
{
	private Logger m_logger;

	private static EntityDeserializer s_entityDeserializer;
	private EntityTFinder m_finder;
	

	/**
	 * Singleton accessor.
	 * 
	 * @param logger Logger to be used 
	 * 			(may be null in subsequent invocations since only one instance of  
	 * 			 EntityDeserializer is constructed and then reused)
	 *  
	 * @return single instance
	 */
	public static EntityDeserializer getEntityDeserializer(Logger logger)
	{
		if (s_entityDeserializer == null)
		{
			s_entityDeserializer = new EntityDeserializer(logger);
		}
		return s_entityDeserializer;
	}


	/**
	 * @param logger
	 */
	private EntityDeserializer(Logger logger)
	{
		m_logger = logger;
		m_finder = new EntityTFinder(m_logger);
	}


	/**
	 * Deserializes (parses) an entity xml and instantiates the binding objects.
	 * 
	 * @param xmlString  the stringified xml. 
	 * @param entityClass  the binding class of which we want an instance
	 *                      filled with the xml data.
	 * @return  the binding class instance (should be casted by the caller).
	 * @deprecated  use {@link #deserializeEntity(XmlEntityStruct, Class)} instead.
	 *              With the addition of timestamp to <code>EntityT</code>,
	 *              this method will produce binding classes whose XML data can't be stored
	 *              in the archive. 
	 */
	public Object deserializeEntity(String xmlString, Class entityClass)
		throws EntityException
	{
		return deserializeEntity(xmlString, entityClass, null);
	}


	/**
	 * Deserializes (parses) an entity xml and instantiates the binding objects.
	 * 
	 * @param xmlString  the stringified xml. 
	 * @param entityClass  the binding class of which we want an instance
	 *                      filled with the xml data.
	 * @param timestamp  the timestamp to be stored in the entity object, for later serialization.
	 * @return  the binding class instance (should be casted by the caller).
	 * @throws EntityException if the binding class instance can't be constructed from the xml. 
	 *         Note that this exception is only logged at FINER level by this method, so make sure to log it appropriately.
	 */
	private Object deserializeEntity(String xmlString, Class entityClass, String timestamp)
	throws EntityException
	{
		if (xmlString == null) 
			throw new EntityException("xmlString was null.");
			
		if (entityClass == null) 
			throw new EntityException("entityClass was null.");
			
		if (timestamp == null)
			timestamp = "";
		
		Object entity;
		try
		{
			Unmarshaller unmarsh = new Unmarshaller(entityClass);
			unmarsh.setValidation(false);
			unmarsh.setWhitespacePreserve(true);
			entity = unmarsh.unmarshal(new StringReader(xmlString));
		}
		catch (Exception e)
		{
			String xmlStringStart = xmlString.substring(0, Math.min(512, xmlString.length())); // trim huge xml to avoid overly long log message
			String msg = "Failed to parse xml into entity binding class '" + entityClass.getName() + "'. The xml data was " + xmlStringStart;
			m_logger.log(Level.FINER, msg, e);
			throw new EntityException(msg, e);
		}
		
		// store the timestamp in the entity; this is a temporary storage, until 
		// the entity is converted back into an XmlEntityStruct; then the timestamp
		// becomes irrelevant.
		if (entity != null)
		{
			EntityT entityMeta = m_finder.extractEntityT(entity);
			if (entityMeta != null)
			{
				entityMeta.setTimestamp(timestamp);
			}
		}
		
		return entity;
	}


	/**
	 * Deserializes (parses) the entity xml contained in <code>xes</code>
	 * and instantiates the binding objects.
	 * 
	 * @param xes the struct used for CORBA transport of serialized entity objects.
	 * @param entityClass  the binding class of which we want an instance
	 *                      filled with the xml data, e.g. <code>SchedBlock.class</code>.
	 * @return  the binding class instance (should be cast to <code>entityClass</code> by the caller).
	 * @throws EntityException
	 */
	public Object deserializeEntity(XmlEntityStruct xes, Class entityClass)
		throws EntityException
	{
		String xml = xes.xmlString;
		String timestamp = xes.timeStamp;
		return deserializeEntity(xml, entityClass, timestamp);
	}
}
