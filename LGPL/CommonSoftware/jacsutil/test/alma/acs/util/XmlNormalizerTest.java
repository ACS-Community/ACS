/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
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

import alma.acs.util.XmlNormalizer;
import junit.framework.TestCase;

/**
 * @author hsommer
 * created Nov 30, 2004 1:52:28 PM
 */
public class XmlNormalizerTest extends TestCase {
    
    public void testNormalizeHarmlessString() {
        String harmless = "ay caramba... what a harmless string!";
        String normalized = XmlNormalizer.normalize(harmless);
        assertTrue("no memory allocation for harmless strings", normalized == harmless);
    }

    public void testNormalizeXMLString() {
        String xml = "<elem foo=\"bar&\">abc&\"def\"ghijk</elem>";
        
        String normalized1 = XmlNormalizer.normalize(xml);
        assertEquals("&lt;elem foo=&quot;bar&amp;&quot;&gt;abc&amp;&quot;def&quot;ghijk&lt;/elem&gt;", normalized1);
        
        String normalized2 = XmlNormalizer.normalizeXMLEmbeddedTextOnly(xml);
        assertEquals("<elem foo=\"bar&amp;\">abc&amp;\"def\"ghijk</elem>", normalized2);
    }

}
