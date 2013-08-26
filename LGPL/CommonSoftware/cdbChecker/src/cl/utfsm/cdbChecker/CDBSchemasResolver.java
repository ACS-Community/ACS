/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) UTFSM - Universidad Tecnica Federico Santa Maria, 2011
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
/**
 * @author Rodrigo Araya (raraya[at]inf.utfsm.cl) & Nicolas Barriga (nbarriga[at]inf.utfsm.cl) & Marco Salgado (msalgado[at]inf.utfsm.cl)
 * */

package cl.utfsm.cdbChecker;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;

public class CDBSchemasResolver implements EntityResolver
{
	private String schemaPaths[];
	private CDBChecker _checker;

	/**
	 * Legacy use of this class resolves only "http://" xsd IDs.
	 * This boolean allows to keep this strange old behavior, but also
	 * to reuse this class as a general xsd resolver.
	 */
	private boolean resolveOnlyHttp = true;

	private final Map<String, String> xsdCache = new HashMap<String, String>();
	
	/**
	 * Instantiates a CDBSchemasResolver and initialises the array of directories where to search for schema files.
	 */
	public CDBSchemasResolver(CDBChecker checker, String XSDPath) {
		_checker = checker;
		
//		System.out.println("XSDPath = " + XSDPath);
		
		schemaPaths = XSDPath.split(File.pathSeparator);
	}

	public void setResolveOnlyHttp(boolean resolveOnlyHttp) {
		this.resolveOnlyHttp = resolveOnlyHttp;
	}

	/**
	 * Resolves an entity. Called whenever a schema file needs to be mapped into a real file
	 */
	@Override
	public InputSource resolveEntity(String publicID, String systemID) {
//		System.out.println("resolveEntity: " + systemID);
		
		if (systemID != null) {
			if (!resolveOnlyHttp || systemID.startsWith("http://")) {
				String[] arr = systemID.split("/");
				return new InputSource(findSchemaFile(arr[arr.length - 1]));
			}
		}
		return null;
	}

	/**
	 * Finds a schema file, if it exists, in the directories configured at construction time. 
	 * Returns null if the schema file cannot be found.
	 * <p>
	 * TODO: Check if it is OK for this method to set the global error flag of CDBChecker, 
	 *       which is the only reason why this class keeps the checker reference...
	 *  
	 * @param schemaName Simple name of schema file. e.g. "Fridge.xsd"
	 * @return The absolute path of the schema file, or null if no such file was found.
	 */
	public String findSchemaFile(String schemaName) {
		
		// try the cache first
		if (xsdCache.containsKey(schemaName)) {
			return xsdCache.get(schemaName); // File or null
		}
		
//		System.out.println("Trying to find " + schemaName);
		
		for (int i = 0; i < schemaPaths.length; i++) {
			String filePath = schemaPaths[i] + File.separator + schemaName;
			File xsdFile = new File(filePath);
			if (xsdFile.exists()) {
				xsdCache.put(schemaName, filePath);
				return filePath;
			}
		}
		if (_checker != null) {
			_checker.setGlobalErrorFlag(true);
		}
		xsdCache.put(schemaName, null);
		return null;
	}
}

