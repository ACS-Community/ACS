/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2010
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acs.alarmsanalyzer.document;

import java.util.Collection;

public class SuppressedContainer extends SupAnnCommonContainer {
	
	/**
	 * The singleton
	 */
	private static SuppressedContainer singleton=null;
	
	public static SupAnnCommonContainer getInstance() {
		if (singleton==null) {
			singleton = new SuppressedContainer();
		}
		return singleton;
	}
	
	/**
	 * Constructor
	 */
	private SuppressedContainer() {}
	
	@Override
	public Collection<?> getNumbers() {
		return suppressed.values();
	}

}
