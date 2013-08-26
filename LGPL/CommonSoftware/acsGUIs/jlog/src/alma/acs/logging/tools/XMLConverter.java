/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2009
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 200, All rights reserved
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
package alma.acs.logging.tools;

import com.cosylab.logging.engine.log.ILogEntry;

/**
 * Convert a log into XML.
 * <P>
 * This class is here for simmetry with other conversions
 * but the real work is delegated to the {@link ILogEntry}.
 * 
 * @author acaproni
 * @since ACS 8.1.0
 */
public class XMLConverter extends LogConverter {

	/**
	 * @see LogConverter
	 */
	@Override
	public String convert(ILogEntry log) {
		if (log==null) {
			throw new IllegalArgumentException("Impossible to convert a null log");
		}
		return log.toXMLString()+"\n";
	}
	
	/**
	 * @see LogConverter
	 */
	@Override
	public String getHeader() {
		return "";
	}
}
