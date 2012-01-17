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
 * Class that represents a command line option for {@link alma.acs.util.CmdLineArgs}.
 * @author hsommer
 */
public class CmdLineOption
{
	protected String name;
	protected String altName;
		
	
	CmdLineOption(String name) {
		this(name, null);
	}
	
	/**
	 * @param name the name of the option, for example "-debug".
	 * @param altName an alternative name of the option, that is understood as a synonym of <code>name</code>.
	 */
	CmdLineOption(String name, String altName) {
		this.name = name.trim();
		this.altName = altName;
	}
	
	String getName() {
		return name;
	}
	
	String getAlternativeName() {
		return altName;
	}
	
	/**
	 * @see java.lang.Object#equals(Object)
	 */
	@Override
	public boolean equals(Object obj)
	{
		if (this == obj)
			return true;
		if (obj == null || !(obj instanceof CmdLineOption))
			return false;

		CmdLineOption other = (CmdLineOption) obj;
		return ( this.getName().equals(other.getName()) );
	}


	/**
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode()
	{
		return name.hashCode();
	}

}
