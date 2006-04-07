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
 * Normalizes XML data by escaping special characters such as '&' or ' <'.
 * 
 * @author hsommer created Jul 25, 2003 10:14:34 AM
 */
public class XmlNormalizer {

    /**
     * Normalizes a string to not conflict with XML markup characters. 
     * Only allocates memory if the string <code>s</code> does contain such markup.
     * Otherwise <code>s</code> is returned.
     */
    private static String normalize(String s, boolean normalizeOnlyInsideQuotes) {
        if (s == null)
            return s;

        StringBuffer str = null;
        int begin = 0;
        boolean currentlyInsideQuotes = false;
        
        for (int i = 0; i < s.length(); i++) {
            char ch = s.charAt(i);
        	String trans = null;
        	
            if (ch == '\"') {
            	currentlyInsideQuotes = !currentlyInsideQuotes;
            	// don't translate the quote char if we expect entire XML including valid markup
            	if (!normalizeOnlyInsideQuotes) {
    	            trans = translate(ch);
            	}
            }
            else if (!normalizeOnlyInsideQuotes || currentlyInsideQuotes){
                trans = translate(ch);
            }            

            if (trans != null) {
                // we found a special char
                if (str == null) {
                    // memory allocation only if character escapes are necessary
                    str = new StringBuffer(s.length() + 16);
                }
                // copy all chars from the last special char to the current special char at once -- 
                // hopefully with slightly better performance than 1 char at a time
                str.append(s.substring(begin, i)).append(trans);
                begin = i + 1;
            }
        }
        if (str != null) {
            str.append(s.substring(begin));
            return str.toString();
        }
        // nothing had to be translated
        return s;
    }

    /**
     * Normalizes a string to not conflict with XML markup characters. 
     * Only allocates memory if the string <code>s</code> does contain such markup.
     * Otherwise <code>s</code> is returned.
     */
    public static String normalize(String s) {
    	return normalize(s, false);
    }
    
    
    /**
     * Normalizes any text in between quotes ("text") inside <code>xmlString</code>
     * but leaves other text untouched. 
     * This method can be used to fix illegal text in an unparsable XML document, 
     * without destroying the XML markup itself.
     * 
     * @param xmlString 
     * @return
     */
    public static String normalizeInsideQuotes(String xmlString) {
    	return normalize(xmlString, true);
    }
    
    
    /**
     * Translates the given character <code>ch</code> to its masked XML representation
     * if this character is one of { &lt;, &gt;, &amp;, ", ' }.
     * Otherwise returns <code>null</code>.
     * @param ch
     * @return The corresponding XML representation such as "&amp;lt;" for a '&lt;'.
     */
    public static String translate(char ch) {
        switch (ch) {
        case '<':
            return "&lt;";
        case '>':
            return "&gt;";
        case '&':
            return "&amp;";
        case '"':
            return "&quot;";
        case '\'':
            return "&apos;";
        default:
            return null;
        }
    }

}
