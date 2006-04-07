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
    private static String normalize(String s, boolean normalizeXMLEmbeddedTextOnly) {
        if (s == null)
            return s;

        StringBuffer str = null;
        int begin = 0;
        boolean currentlyInsideText = false;
        boolean currentlyInsideCharacterContent = false;
        
        for (int i = 0; i < s.length(); i++) {
            char ch = s.charAt(i);
        	String trans = null;

        	if (normalizeXMLEmbeddedTextOnly && !currentlyInsideText) {
	            if (ch == '<') {
	            	currentlyInsideCharacterContent = false;
	            	continue;
	            }
	            else if (ch == '>') {
	            	currentlyInsideCharacterContent = true;
	            	continue;
	            }
        	}
        	
            if (ch == '\"') {
            	currentlyInsideText = !currentlyInsideText;
            	// don't translate the quote char if we expect entire XML including valid markup
            	if (!normalizeXMLEmbeddedTextOnly) {
    	            trans = translate(ch);
            	}
            }
            else if (!normalizeXMLEmbeddedTextOnly || currentlyInsideText || currentlyInsideCharacterContent) {
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
     * Normalizes any text in between quotes ("text") or text given as character data of mixed-content elements, 
     * all inside <code>xmlString</code>. Other text is left untouched. 
     * This method can be used to fix illegal text in an unparsable XML document, 
     * without destroying the XML markup itself.
     * <p>
     * Note that some rather simple parsing is used, and that there may be cases in which the result 
     * is undesirable. Thus this method should only be used as a fallback solution when the XML string can not be parsed with a 
     * regular XML parser. 
     * 
     * @param xmlString 
     * @return
     */
    public static String normalizeXMLEmbeddedTextOnly(String xmlString) {
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
