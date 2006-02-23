package com.cosylab.acs.maci.manager;

import java.net.URI;
import java.net.URISyntaxException;

/**
 * CURL helper class providing various utilities.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class CURLHelper
{
	
	/**
	 * String denoting CURL sheme.
	 */
	public static final String CURL_SCHEME = "curl";

	/**
	 * Raw String denoting CURL sheme.
	 */
	public static final String CURL_SCHEME_STRING = CURL_SCHEME+"://";
	
	/**
	 * Empty string.
	 */
	private static final String emptyString = "";

	/**
	 * String with slash.
	 */
	private static final String slashString = "/";

	/**
	 * URI factory method creating URIs from CURLs represended by <code>java.lang.String</code> class.
	 * Given string are also preprocessed in order to satisfy URI standards.
	 * @param curl	curl to be transformed to URI.
	 * @return URI	URI representation of an CURL.
	 */
	public static URI createURI(String curl) throws URISyntaxException
	{
		if (curl == null)
			throw new URISyntaxException("<null>", "Null CURL.");
			
		if (curl.length() == 0)
			throw new URISyntaxException(curl, "Zero length CURL.");

		// we have got URI formed CURL
		if (curl.toLowerCase().startsWith(CURL_SCHEME_STRING))
		{
			URI uri = new URI(curl);
			
			/*
			// examine path for ':'
			String path = uri.getPath();
			if (path != null)
			{
				// replace
				String cleanPath = path.replace(':', '/');
				
				// if replacement has to be made, recreate URI
				if (cleanPath != path)
					uri = new URI(uri.getScheme(), (uri.getAuthority()!=null) ? uri.getAuthority() : emptyString,
								   cleanPath, uri.getQuery(), uri.getFragment());
			}
			*/

			return uri;
		}
		// if not curl is not URI formed, then only path part is expected
		else
		{
			/*
			// replace all ':' to '/'
			curl = curl.replace(':', '/');
			*/			

			// public URI(String scheme, String authority, String path, String query, String fragment)
			return new URI(CURL_SCHEME, emptyString, slashString+curl, null, null);
		}
	}
}
