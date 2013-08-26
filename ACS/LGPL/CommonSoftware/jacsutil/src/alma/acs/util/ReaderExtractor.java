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
package alma.acs.util;

import java.io.IOException;
import java.io.Reader;


/**
 * Helper class that reads all <code>char</code>s from a <code>Reader</code> object 
 * and returns them as a <code>String</code>.
 */
public class ReaderExtractor
{
	private Reader m_reader;
	
	public ReaderExtractor(Reader reader)
	{
		m_reader = reader;
	}		
	
	public String extract() throws IOException
	{
		StringBuffer buffer = new StringBuffer();
		char[] cBuff = new char[256];
		int readResult = m_reader.read(cBuff, 0, cBuff.length);
		while (readResult > -1)
		{
			buffer.append(cBuff, 0, readResult);
			readResult = m_reader.read(cBuff, 0, cBuff.length);
		}		
		return buffer.toString();
	}		
}
