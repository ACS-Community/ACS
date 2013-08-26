/*
 * Created on Oct 25, 2005 by mschilli
 */
package alma.acs.commandcenter.util;

import java.util.StringTokenizer;


/**
 * Purpose of this class is to resolve placeholders
 * in a source string like "my name is ?".
 * <p>
 * Invocation of the {@link #toString(String[])} method will resolve
 * the placeholders to the elements of the string array.
 *  
 * If the string array contains too few elements, placeholders characters will
 * be left in the output string. If the string array contains too many elements,
 * the superfluous elements will not be inserted in the output string.</p>
 *
 * 
 * <p><strong>Note: </strong>
 * Relationship between {@link alma.acs.commandcenter.util.PreparedString}
 * and {@link alma.acs.commandcenter.util.VariableString}:
 * 
 * {@link alma.acs.commandcenter.util.PreparedString} uses <em>positional</em>
 * parameters to replace placeholders with the elements of a string array.
 * {@link alma.acs.commandcenter.util.VariableString} uses <em>named</em>
 * parameters to replace placeholders with real values from a map.</p>
 */
public class PreparedString {

	String[] pattern;

	/**
	 * Constructs an instance with the given source string and "?" as
	 * the placeholder character for later insertions.
	 * 
	 * @param p pattern like "my name is ?"
	 */
	public PreparedString(String p) {
		this (p, '?');
	}
	
	/**
	 * Constructs an instance with the given source string and the given
	 * placeholder character.
	 * @param p pattern like "my name is ?" (if the placeholder is "?")
	 */
	public PreparedString(String p, char placeholder) {
		StringTokenizer t = new StringTokenizer(p, Character.toString(placeholder), true);
		this.pattern = new String[t.countTokens()];
		int i = 0;
		while (t.hasMoreTokens()) {
			pattern[i++] = t.nextToken();
		}
	}

	// ================================================
	// API
	// ================================================

	/**
	 * Returns a version of the original source string where the
	 * placeholders have been replaced with the given string elements. 
	 */
	public String toString (String[] pieces) {
		StringBuffer buf = new StringBuffer();
		int j = 0;
		for (int i = 0; i < pattern.length; i++) {
			if (pattern[i].equals("?") && j < pieces.length)
				buf.append(pieces[j++]);
			else
				buf.append(pattern[i]);
		}
		return buf.toString();
	}

	@Override
	public String toString () {
		return toString(new String[]{});
	}
}
