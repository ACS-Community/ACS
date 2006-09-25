package cern.gp.beans.editors.support;

import java.awt.Font;
import java.util.HashMap;

/**
 * A small helper class that caches derived fonts, instead of creating a new Font every time the method
 * {@link java.awt.Font#deriveFont(int)} is called
 * @author Vito Baggiolini
 * @version $Revision: 1.2 $, $Date: 2006/09/25 08:52:36 $, $Author: acaproni $
 * @since 2.0.6
 */
class CachedFont  {
	private final Font original;
	private HashMap derivedFonts = new HashMap();
	
	/**
	 * a constructor with the first font used, (must not be plain)
	 * @param original, must be non-null
	 */
	public CachedFont(Font original) {
		if (original == null) {
			throw new NullPointerException("must pass a non-null font");
		}
		derivedFonts.put(getStyleKey(original.getStyle()), original);
		this.original = original;
	}

	/**
	 * return a font with the style applied, such as
	 * defined in {@link Font#getStyle()}
	 * @param style
	 * @return a (cached) instance of the font
	 */
	public Font getWithStyle(int style) {
		String styleKey = getStyleKey(style);
		Font derived = (Font) derivedFonts.get(styleKey);
		if (derived == null) {
			derived = original.deriveFont(style);
			derivedFonts.put(styleKey, derived);
		}
		return derived;
	}
	private final String getStyleKey(int style) {
		return Integer.toString(style);
	}
 	
	
	/**
	 * @return Returns the original font passed in the constructor.
	 */
	public Font getOriginal() {
		return original;
	}

}
