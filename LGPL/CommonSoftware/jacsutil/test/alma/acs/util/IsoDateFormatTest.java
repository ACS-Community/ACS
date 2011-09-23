/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2005
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

package alma.acs.util;


import java.util.Date;
import junit.framework.TestCase;

//import static org.junit.Assert.assertEquals;
//import org.junit.Test;


/**
 * @author hsommer
 */
public class IsoDateFormatTest extends TestCase // TODO remove this extends once TATJUnitRunner has been upgraded to JUnit4
{
	//@Test
	public void testFormatDate() throws Exception {
		Date date = new Date(1345678901234L);
		String isoTimestamp = IsoDateFormat.formatDate(date);
		assertEquals("2012-08-22T23:41:41.234", isoTimestamp);
	}

	//@Test
	public void testParseTimestamp() throws Exception {
		Date date = IsoDateFormat.parseIsoTimestamp("2011-09-12T15:40:47.568");
		assertEquals(1315842047568L, date.getTime());
	}

}