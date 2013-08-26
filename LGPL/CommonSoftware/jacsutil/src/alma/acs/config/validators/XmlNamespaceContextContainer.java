/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2005
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

package alma.acs.config.validators;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import javax.xml.namespace.NamespaceContext;

/**
 * Support class for XPath queries which allows to bind XML namespaces to prefixes,
 * using {@link javax.xml.xpath.XPath#setNamespaceContext(javax.xml.namespace.NamespaceContext)}.
 * This is especially needed for XPath queries on XML documents that use an (unnamed) default namespace,
 * for which an "artificial" prefix must be defined and communicated to the XPath engine,
 * so that the XPath expression can use that prefix in order to match the default namespace.
 * <p> 
 * This class with its method {@link #addNamespace(String, String)} This method serves a similar purpose 
 * as jdom's {@link org.jdom.xpath.XPath#addNamespace(String, String)} method.
 * <p>
 * Implementation note: we only keep a map to quickly go from prefix to namespace, but not the other way around.
 * According to http://xml.apache.org/xalan-j/xpath_apis.html#namespacecontext the XPath processor in Xalan
 * never needs the methods that return prefixes, thus their performance does not matter at the moment.
 * 
 * @author hsommer
 */
public class XmlNamespaceContextContainer implements NamespaceContext {

	private Map<String, String> prefixUriMap = new HashMap<String, String>();
	
	public String getNamespaceURI(String prefix) {
		return prefixUriMap.get(prefix);
	}

	/**
	 * @see javax.xml.namespace.NamespaceContext#getPrefix(java.lang.String)
	 */
	public String getPrefix(String namespaceURI) {
		for (Iterator<String> iter = getPrefixes(namespaceURI); iter.hasNext();) {
			String prefix = iter.next();
			String uri = prefixUriMap.get(prefix);
			if (uri.equals(namespaceURI)) {
				return prefix;
			}
		}
		return null;
	}

	public Iterator<String> getPrefixes(String namespaceURI) {
		Set<String> prefixes = prefixUriMap.keySet();
		return prefixes.iterator();
	}
	
	
	/**
	 * Adds a prefix/namespace pair.
	 * @param prefix
	 * @param uri
	 */
	public void addNamespace(String prefix, String uri) {
		// todo: more checks for valid xml names 
		if (prefix != null && uri != null) {
			prefixUriMap.put(prefix, uri);
		}
	}

}
