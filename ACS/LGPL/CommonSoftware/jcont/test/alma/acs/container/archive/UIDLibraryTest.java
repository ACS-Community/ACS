/*
 * 	  Created on 26-Sep-2005
 * 
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
package alma.acs.container.archive;

import java.util.logging.Logger;

import junit.framework.TestCase;

import alma.ArchiveIdentifierError.wrappers.AcsJIdentifierUnavailableEx;
import alma.ArchiveIdentifierError.wrappers.AcsJUidAlreadyExistsEx;
import alma.acs.logging.ClientLogManager;
import alma.entities.commonentity.EntityT;
import alma.xmlstore.IdentifierJ;

/**
 * @author simon, hsommer
 */
//alma.archive.identifier.UIDLibraryTest
public class UIDLibraryTest extends TestCase
{
	private Logger logger;
	private IdentifierJ ident;
	
	/**
	 * @param name
	 * @throws java.lang.Exception
	 */
	public UIDLibraryTest() throws Exception {
		super("UIDLibraryTest");
		logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(getName(), false);
	}

	protected void setUp() throws Exception {
		super.setUp();
		
		// build order issue: at jcont unit test time, we do not have the real identifier archive available yet, thus we fake it 
		ident = new IdentifierJMock(logger, 0x1L, 0xAABBL);
	}
	
	protected void tearDown() throws Exception {
		super.tearDown();
	}

	
	public void testAllocateDefaultID() throws Exception 
	{
		UIDLibrary lib = new UIDLibrary(logger);
		
		EntityT e = new EntityT();
		
		// assign and check a UID
		lib.assignUniqueEntityId(e, ident);
		String uid = e.getEntityId();
		logger.info("Got a new UID for an entity object: " + uid);
		assertEquals("uid://X01/Xaabb/X1", uid);
		
		// try to reassign a UID
		try {
			lib.assignUniqueEntityId(e, ident);
			fail("expected AcsJUidAlreadyExistsEx");
		} catch (AcsJUidAlreadyExistsEx ex) {
			// fine
		}		
		// verify that the exception did not change the old ID
		assertEquals(uid, e.getEntityId());		
		
		// try a null identifier archive
		try {
			lib.assignUniqueEntityId(e, (IdentifierJ)null);
			fail("Expected AcsJIdentifierUnavailableEx when using a null Identifier archive.");
		} catch (AcsJIdentifierUnavailableEx ex) {
			// fine
		}		
	}

	
//	public void testRestrictedRanges()  throws Exception
//	{
//		ContainerServices containerServices = getContainerServices();
//		UIDLibrary lib =  new UIDLibrary(m_logger);
//		
//		int max = 10;
//		String last = "";
//		ArrayList idlist = new ArrayList();
//		
//		EntityT entity = new EntityT();
//		EntityRefT ref = new EntityRefT();
//		
//		URI uri = lib.getNewRestrictedRange(max,"test");
//		for (int x = 0; x<max; x++)
//		{
//			lib.assignUniqueEntityId(entity,uri);
//			String id = entity.getEntityId();
//			
//			assertTrue(!last.equalsIgnoreCase(id));
//			last = id;
//			
//			idlist.add(id);
//		}
//		
//		try
//		{
//			lib.assignUniqueEntityRef(ref,uri);
//			fail();
//		}
//		catch (UIDLibraryException e){};
//		
//		try
//		{
//			lib.assignUniqueEntityId(entity,uri);
//			fail();
//		}
//		catch (UIDLibraryException e){};
//		
//		last = "";
//		lib.fetchRange(uri,"test");
//		for (int x = 0; x<max; x++)
//		{
//			
//			lib.assignUniqueEntityRef(ref,uri);
//			String id = ref.getEntityId();
//			assertTrue(!last.equalsIgnoreCase(id));
//			
//			last = id;
//			assertTrue(idlist.contains(id));
//		}
//		
//		try
//		{
//			lib.assignUniqueEntityId(entity,uri);
//			fail();
//		}
//		catch (UIDLibraryException e){};
//		try
//		{
//			lib.assignUniqueEntityRef(ref,uri);
//		}
//		catch (UIDLibraryException e){};		
//	}
//	
//	public void testMultipleRanges() throws Exception
//	{
//		ContainerServices containerServices = getContainerServices();
//		UIDLibrary lib = new UIDLibrary(m_logger);
//		int max = 10;
//		int numRanges = 100;
//		for (int x = 0; x < numRanges; x++)
//		{
//			URI uri = lib.getNewRestrictedRange(max,"test");
//		}
//	}
}
