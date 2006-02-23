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
    public static String normalize(String s) {
        if (s == null)
            return s;

        StringBuffer str = null;
        int begin = 0;
        for (int i = 0; i < s.length(); i++) {
            char ch = s.charAt(i);

            String trans = translate(ch);
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

    private static String translate(char ch) {
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
