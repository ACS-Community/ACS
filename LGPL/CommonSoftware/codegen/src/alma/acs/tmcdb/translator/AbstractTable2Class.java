/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.tmcdb.translator;

import java.util.List;
import java.util.Map;

public abstract class AbstractTable2Class {

	protected static final String PACKAGE = "alma.acs.tmcdb.";

	protected Map<String, String> map;
	protected List<String> tablesWithGeneratedKeys;

	public Map<String,String> getMap() { return map; };

	public boolean hasGeneratedKey(String table) {
		return tablesWithGeneratedKeys.contains(table.toLowerCase());
	}

}
