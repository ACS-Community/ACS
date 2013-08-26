/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
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
/*
 * Created on Jun 21, 2003
 *
 * To change the template for this generated file go to
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
package alma.acs.exceptions;

import java.util.Properties;

import alma.ACSErr.ErrorTrace;
import alma.ACSErr.NameValue;

/**
 * @author hsommer
 *
 * To change the template for this generated type comment go to
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
public class ErrorTraceManipulator
{

	/**
	 * Returns all properties that are stored in <code>et.data</code>.
	 * @param et
	 * @return the <code>NameValue</code> pairs in a new <code>Properties</code> map. 
	 */
	public static Properties getProperties(ErrorTrace et)
	{
		Properties props = new Properties();
		if (et.data != null)
		{
			for (int i = 0; i < et.data.length; i++)
			{
				NameValue nv = et.data[i];
				if (nv.name != null)
				{
					props.setProperty(nv.name, nv.value);
				}
			}
		}
		return props;
	}
	
	public static String getProperty(ErrorTrace et, String key)
	{
		String value = null;
		if (et.data != null)
		{
			for (int i = 0; i < et.data.length; i++)
			{
				NameValue nv = et.data[i];
				if (nv.name.equals(key))
				{
					value = nv.value;
					break;
				}
			}
		}
		return value;
	}


	public static String setProperty(ErrorTrace et, String key, String value)
	{
		String oldValue = null;
		boolean alreadyThere = false;
		if (et.data != null)
		{
			for (int i = 0; i < et.data.length; i++)
			{
				NameValue nv = et.data[i];
				if (nv.name.equals(key))
				{
					alreadyThere = true;
					oldValue = nv.value;
					nv.value = value;
					break;
				}
			}
		}
		
		if (!alreadyThere)
		{
			NameValue[] oldNVs = et.data;
			int oldLength = 0;
			if (oldNVs == null)
			{
				et.data = new NameValue[1];
			}
			else
			{
				oldLength = oldNVs.length;
				et.data = new NameValue[oldLength + 1];
				System.arraycopy(oldNVs, 0, et.data, 0, oldLength);
			}
			et.data[oldLength] = new NameValue(key, value);
		}
	
		return oldValue;
	}
}
