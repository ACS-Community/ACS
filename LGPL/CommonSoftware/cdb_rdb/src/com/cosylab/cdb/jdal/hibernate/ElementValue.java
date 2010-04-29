/**
 * 
 */
package com.cosylab.cdb.jdal.hibernate;

/**
 * Plain XML element with value.
 * @author msekoranja
 */
public class ElementValue implements ElementValueFeature {

	/**
	 * Element value;
	 */
	private String _elementValue;
	
	/**
	 * Constructor.
	 */
	public ElementValue(String elementValue) {
		this._elementValue = elementValue;
	}

	/* (non-Javadoc)
	 * @see com.cosylab.cdb.jdal.hibernate.ElementValueFeature#getElementValue()
	 */
	public String getElementValue() {
		return _elementValue;
	}

	
}
