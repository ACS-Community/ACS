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

import java.util.logging.Logger;

import junit.framework.TestCase;

import alma.entities.commonentity.EntityT;
import alma.xmlentity.XmlEntityStruct;
import alma.xmljbind.test.obsproposal.ObsProposal;

/**
 * @author hsommer
 * created Aug 19, 2003 11:48:58 AM
 */
public class EntitySerializerTest extends TestCase
{
	private EntitySerializer m_serializer;
	private TestEntityFactory m_entityFactory;
	
	/**
	 * {@inheritDoc}
	 * 
	 * @see junit.framework.TestCase#setUp()
	 */
	protected void setUp() throws Exception
	{
		super.setUp();
		m_serializer = EntitySerializer.getEntitySerializer(Logger.getLogger("EntitySerializerTest"));
		m_entityFactory = new TestEntityFactory();
	}

	
	public void testSerializeNull() throws EntityException
	{
		XmlEntityStruct entStruct = m_serializer.serializeEntity(null);
		assertNull(entStruct);

		entStruct = m_serializer.serializeEntity(null, new EntityT());
		assertNull(entStruct);

		entStruct = m_serializer.serializeEntity(null, null);
		assertNull(entStruct);
	}
	
	
	public void testSerializeNonCastor()
	{
		Object nonCastorObj = new String();

		try
		{
			m_serializer.serializeEntity(nonCastorObj);
			fail("serializing a non-castor object should have yielded an exception!");
		}
		catch (EntityException e)
		{
			// that's good
		}
	}
	
	
	public void testSerializeObsProposal() throws EntityException
	{
		ObsProposal obsProp = m_entityFactory.getObsProposal();
		String obsPropXml = m_serializer.serializeEntity(obsProp).xmlString;
		assertNotNull(obsPropXml);
		
		System.out.println(obsPropXml);
	}
}
