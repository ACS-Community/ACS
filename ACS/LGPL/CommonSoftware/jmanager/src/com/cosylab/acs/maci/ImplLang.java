/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci;

/**
 * Implementation language enum.
 * @author msekoranja
 */
public enum ImplLang {
	java, cpp, py, not_specified;

	/**
	 * Get enum out of string (also checks handles invalid string).
	 * @param implLang
	 * @return
	 */
	public static ImplLang fromString(String implLang)
	{
		try {
			return valueOf(implLang);
		} catch (Throwable th) {
			return not_specified;
		}
	}

}
