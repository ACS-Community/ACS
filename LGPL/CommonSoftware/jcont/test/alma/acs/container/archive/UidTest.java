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
package alma.acs.container.archive;


import junit.framework.TestCase;

import alma.acs.testsupport.tat.TATJUnitRunner;

/**
 * @author hsommer Jun 6, 2003 1:58:43 PM
 */
public class UidTest extends TestCase
{
	private static final String UID_1 = "uid://X0000000000000018/X00000000"; 
	private static final String UID_2 = "uid://X0000000000000005/X0000000a";
	
	public void testParse()
	{
		Uid uid = new Uid();
		boolean parseResult = uid.parse(UID_1);
		assertTrue("must parse uid " + UID_1, parseResult);
	}
	
	public void testStatelessToString()
	{
		Uid uid = new Uid();
		String uidString = uid.toString(5, 10);
		assertEquals(UID_2, uidString);
	}

	public void testIncrementing() throws UniqueIdException
	{
		Uid uid = new Uid();
		uid.parse(UID_1);
		
		// limit the ID range 
		uid.setLocalIdentifierMaxValue(1);
		
		// increment the local part
		assertTrue(uid.canIncrementLocalIdentifier());
		uid.incrementLocalIdentifier();
		assertEquals(1, uid.getLocalIdentifier());
		assertEquals(0x18, uid.getGlobalIdentifier());// no affect on global part

		// now the range limit should apply
		assertFalse(uid.canIncrementLocalIdentifier());
		
		// check parameter validation
		try
		{
			uid.setLocalIdentifierMaxValue(0);
			fail("UniqueIdException expected for setLocalIdentifierMaxValue.");
		}
		catch (UniqueIdException e)
		{
		}
	}
	
	public static void main(String[] args)
	{
		TATJUnitRunner.run(UidTest.class);
	}

}
