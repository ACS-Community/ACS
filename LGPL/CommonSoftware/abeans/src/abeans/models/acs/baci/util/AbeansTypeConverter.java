/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.baci.util;

/**
 * Interface defining converter between plug and Abeans types. 
 * Example of usage: ACS CORBA long property is acually Java int propertry,
 * and Abeans supports only long integer property.
 * Therefore Integer has to be converted to Long, and vice versa. 
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public interface AbeansTypeConverter {
	
	/**
	 * Converts value to Abeans object.
	 * @param value	value to be converted to Abeans object.
	 * @return	Abeans object.
	 */
	public Object toAbeansType(Object value);

	/**
	 * Converts value from Abeans object.
	 * @param value	value to be converted from Abeans object.
	 * @return	non-Abeans object.
	 */
	public Object fromAbeansType(Object value);

}
