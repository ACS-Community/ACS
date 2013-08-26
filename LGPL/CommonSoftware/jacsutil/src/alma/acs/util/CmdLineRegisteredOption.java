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

/**
 * Used both for declaration of possible cmd line options, 
 * as well as to hold actual values
 * 
 * @author hsommer
 */
public class CmdLineRegisteredOption extends CmdLineOption
{
	protected int m_minValuesCount;
	
	// could be used to generate 'usage' instructions, ignored for the time being
	protected  String description;
	
	
	public CmdLineRegisteredOption(String name, int minValuesCount)
	{
		this(name, null, minValuesCount);
	}
	
	public CmdLineRegisteredOption(String name, String altName, int minValuesCount)
	{
		super(name, altName);
		m_minValuesCount = minValuesCount;
	}
	
	/**
	 * @return The minimal number of required arguments, as passed in the constructor.
	 */
	public int getMinValueCount()
	{
		return m_minValuesCount;
	}

	/**
	 * Returns <code>super.equals(obj)</code> because the minimum number required arguments
	 * should not be part of the CmdLineRegisteredOption identity (only the name gets used).
	 * We override this method anyway, to document this fact.
	 */
	@Override
	public boolean equals(Object obj) {
		return super.equals(obj);
	}
	
	/** 
	 * hashCode should only depend on option name, thus calling <code>super.hashCode()</code>.
	 * We override this method to document this fact.
	 */
	@Override
	public int hashCode() {
		return super.hashCode();
	}

}
