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
package alma.tools.entitybuilder;

import java.io.File;
import java.util.Map;

import org.exolab.castor.net.URIException;
import org.exolab.castor.net.URILocation;
import org.exolab.castor.net.URIResolver;
import org.exolab.castor.net.util.URILocationImpl;
import org.exolab.castor.net.util.URIResolverImpl;

/**
 * The ALMA replacement for <code>org.exolab.castor.net.util.URIResolverImpl</code> 
 * (or previously <code>org.exolab.castor.xml.schema.reader.PathServices</code>).
 * Is so smart as to support the multi staged build process with INTROOT, ACSROOT etc.
 * 
 * @author hsommer Jan 23, 2003 10:19:46 AM
 */
public class AlmaURIResolver implements URIResolver
{
	private static boolean DEBUG = false;
	
	private Map<String, File> schemaName2File;

    public AlmaURIResolver(Map<String, File> schemaName2File) {
        this.schemaName2File = schemaName2File;
    }
    
    /**
     * @param href the (schema) file name etc.
     * @param documentBase is ignored
     */
    public URILocation resolve(String href, String documentBase) throws URIException {
        if (DEBUG) {
            System.out.println("resolve called with href=" + href + ", documentBase=" + documentBase);
        }
        
        File schemaFile = schemaName2File.get(href);
        if (schemaFile == null) {
            throw new URIException("schema file '" + href + "' not found!");
        }
        String absolutePath = schemaFile.getAbsolutePath();        
        URILocation loc = new URILocationImpl(absolutePath);
        
        if (DEBUG) {
            System.out.println("    -> " + loc.getAbsoluteURI());
        }
        
        return loc;
    }
    
    /**
     * Returns null.
     * @see URIResolverImpl#resolveURN(String)
     */
    public URILocation resolveURN(String urn) throws URIException {
        System.out.println("resolveURN called with urn=" + urn);
        return null;
    }
}
