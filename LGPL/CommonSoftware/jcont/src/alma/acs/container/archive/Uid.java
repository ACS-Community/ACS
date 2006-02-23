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



/**
 * TODO: Remove this class and move the origional from archive to 
 * define. Should remove any possibilty of cyclic dependancies
 * 
 * @author simon, hsommer
 * 
 * Uid.java Created by simon on 07-Nov-2002
 * 
 */
public class Uid
{
	 
	private long m_globalIdentifier = 0;
	private int m_localIdentifier = 0;
	// todo: check treatment of negative values: 
	// shouldn't this rather be 
//	private long m_globalIdentifier = Long.MIN_VALUE;
//	private int m_localIdentifier = Integer.MIN_VALUE;
	
	private final String protocol = "uid";
	private final String seperator = "://" ;

	private final int globalIdentifierLength = 16;
	private final int localIdentifierLength = 8;
	
	/**
	 * To fetch small ID ranges, only used with local persistence of one ID range 
	 * (current workaround for laptop scenario)  
	 */
	private long m_localIdentifierMaxValue = Integer.MAX_VALUE;

		
	public Uid()
	{
	}

	/**
	 * Constructor for Uid from string representation.
	 */
	public Uid(String uid) throws UniqueIdException
	{
		boolean ok = parse(uid);
		if (!ok)
		{
			throw new UniqueIdException("invalid id string " + uid);
		}
	}
	
	public synchronized boolean parse(String id)
	{
		
		String idstring = id;
		
		int divider = id.indexOf(seperator);
		if (divider == -1) return false;
		if (id.indexOf(protocol) == -1) return false;
		idstring = id.substring(divider+seperator.length(),id.length());

		if (idstring.indexOf("/") == -1) return false;
		

		// find the two X's
		int firstX = id.indexOf("X");
		if (firstX == -1) return false;
		int secondX = id.indexOf("X",firstX+1);
		if (secondX == -1) return false;

		String global = id.substring(firstX+1,firstX+1+globalIdentifierLength);
		String local = id.substring(secondX+1,id.length());

		m_globalIdentifier = Long.parseLong(global, 16);
		m_localIdentifier = Integer.parseInt(local,16);

		return true;
	}
	

	public String toString()
	{
		return toString(getGlobalIdentifier(), getLocalIdentifier());
	}

	
	public String toString(long globalIdentifier, int localIdentifier)
	{
		// TODO: check if implicit conversion to _unsigned_ long/int is desired here.
		String globalString = padString(Long.toHexString(globalIdentifier), globalIdentifierLength);
		String localString = padString(Long.toHexString(localIdentifier), localIdentifierLength); // Integer.toHexString ?
		
		String uid = protocol + seperator + "X" + globalString + "/X" + localString;
		return uid;
	}
	

	private String padString(String input, int length)
	{
		String local = input;
		for (int x = input.length();x<length;x++)
		{
			local = "0" + local;
		}
		return local;
	}
	

	/**
	 * Returns the globalIdentifier.
	 * @return long
	 */
	public synchronized long getGlobalIdentifier()
	{
		return m_globalIdentifier;
	}

	/**
	 * Returns the localIdentifier.
	 * @return long
	 */
	public synchronized int getLocalIdentifier()
	{
		return m_localIdentifier;
	}

	/**
	 * Sets the globalIdentifier.
	 * @param globalIdentifier The globalIdentifier to set
	 */
	public synchronized void setGlobalIdentifier(long globalIdentifier)
	{
		this.m_globalIdentifier= globalIdentifier;
	}

	/**
	 * Sets the localIdentifier.
	 * @param localIdentifier The localIdentifier to set
	 */
	public synchronized void setLocalIdentifier(int localIdentifier)
	{
		this.m_localIdentifier = localIdentifier;
	}

	/**
	 * 
	 * @param maxValue
	 * @throws UniqueIdException  if maxValue < current localIdentifier
	 */
	public synchronized void setLocalIdentifierMaxValue(int maxValue)
		throws UniqueIdException
	{
		if (maxValue >= m_localIdentifier)
		{
			m_localIdentifierMaxValue = maxValue;
		}
		else
		{
			throw new UniqueIdException("illegal attempt to set max value=" + maxValue + 
									" to local identifier=" + m_localIdentifierMaxValue);
		}
	}

	public synchronized boolean canIncrementLocalIdentifier()
	{
		return (m_localIdentifier < m_localIdentifierMaxValue);
	}
	
	public synchronized void incrementLocalIdentifier() throws UniqueIdException
	{
		if (!canIncrementLocalIdentifier())
		{
			throw new UniqueIdException("local cache used up, can't increment local identifier.");
		}
		
		m_localIdentifier++;
	}
}
