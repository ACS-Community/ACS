/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
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
package alma.acs.logging.table;

import javax.swing.JComponent;

/**
 * This class helps formatting the tooltip as desired.
 * <P>
 * This class has been written because the same method is called in different places.
 * 
 * @author acaproni
 *
 */
public class LogTooltipHelper {
	/**
	 * Format the string before setting the tooltip for the given component
	 * The tooltip is shown only if the text is not visible (i.e. the num.
	 * of displayed chars for the column containing the text is less then
	 * the given text).
	 * <P>
	 * To show the string as multi-line it is transformed in HTML and
	 * <code>\n</code> are replaced by <code>&lt;BR&gt;</code>. 
	 * To show strings containing HTML and/or XML the <code>&lt;PRE&gt;</code> tag is used 
	 * (for this reason existing &lt; and &gt; in the original string are replaced by &lt; and &gt;)
	 * 
	 * @param c The component to set the tooltip 
	 * @param text The string to display in the tooltip
	 * @param colWidth The max number of chars for each line of the tooltip
	 */
	public static void setToolTip(JComponent  c, String text, int colWidth) {
		if (text==null || text.length()==0)	{
			c.setToolTipText(null);
			return;
		}
		// Insert the 'new line' if text is longer then colWidth
		StringBuilder str = new StringBuilder();
		String toolTip;
		if (text.length()>colWidth) {
			int count=0;
			
			for (int t=0; t<text.length(); t++) {
				if (++count>=colWidth) {
					count=0;
					str.append('\n');
				}
				char ch = text.charAt(t);
				str.append(ch);
				if (ch=='\n') {
					count=0;
				} else {
					count++;
				}
			}
			toolTip=str.toString();
		} else {
			toolTip=text;
		}
		// Format the string as HTML
		toolTip=toolTip.replaceAll("<","&lt;");
		toolTip=toolTip.replaceAll(">","&gt;");
		toolTip=toolTip.replaceAll("\n", "<BR>");
		// Eventually, set the tooltip
		c.setToolTipText("<HTML><PRE>"+toolTip+"</PRE></HTML>");
	}
}
