/*
 * Created on Nov 18, 2003
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package cern.gp.util;

import java.awt.Color;

/**
 * A class that was intended for setting colors and fonts using Html Rendering in JLabel
 * Finally not used.
 * @author Vito Baggiolini
 * @version $Revision: 1.2 $, $Date: 2006/09/25 08:52:36 $, $Author: acaproni $
 */
public class HtmlTagger {
	//size=\""-1\" color=\"#00FF00\" bgcolor=\"#FF0000\">" 
	private static final String BGCOLOR_TAG = " bgcolor=\"#";
	private static final String COLOR_TAG = " color=\"#";
	public static String setTags(String text, boolean italic, boolean bold) {
		return setTags(text, italic, bold, null, null);
	}
	public static String setTags(String text, Color foreGround, Color backGround) {
		return setTags(text, false, false, foreGround, backGround);
	}
	public static String setTags(String text, boolean italic, boolean bold, Color foreGround, Color backGround) {
		StringBuffer sb = new StringBuffer("<html>");
		if (foreGround != null || backGround != null) {
			sb.append("<font");
		}
		if (foreGround != null) {
			sb.append(COLOR_TAG);
			sb.append(Integer.toHexString(foreGround.getRGB() & 0xffffff));
			sb.append("\"");
		}
		if (backGround != null) {
			sb.append(BGCOLOR_TAG);
			sb.append(Integer.toHexString(backGround.getRGB() & 0xffffff));
			sb.append("\"");
		}
		if (foreGround != null || backGround != null) {
			sb.append(">");
		}

		if (italic) {
			sb.append("<i>");
		}
		if (bold) {
			sb.append("<b>");
		}
		sb.append(text);
		if (bold) {
			sb.append("</b>");
		}
		if (italic) {
			sb.append("</i>");
		}
		
		sb.append("</html>");
		return sb.toString();
	}
	public static void main(String[] args) {
		String text = "hello";
		System.out.println(setTags(text, true, false, null, null));
		System.out.println(setTags(text, false, true, null, null));
		String html3 = setTags(text, false, false, Color.red, Color.lightGray);
		System.out.println(html3);
		System.out.println(getBgColor(html3));
		String html4 = setTags(text, false, true, Color.red, Color.blue);
		System.out.println(html4);
		System.out.println(getBgColor(html4));
		
	}
	/**
	 * @param displayName
	 * @return
	 */
	public static String getBgColor(String html) {
		int startIndex = html.toLowerCase().indexOf(BGCOLOR_TAG);
		if (startIndex < 0) {
			return null;
		}
		startIndex += BGCOLOR_TAG.length();

		int endIndex = html.indexOf('"', startIndex);
		String colorCode = html.substring(startIndex, endIndex);

		return colorCode;
	}
}
