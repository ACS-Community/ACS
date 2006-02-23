/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.baci.util.async;

/**
 * Helper methods for history access.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public interface HistoryProducer {

	/**
	 * Creates new instance of history value holder object.
	 * @return	new instance of history value holder object.
	 */
	public Object getHistoryValueHolder();

	/**
	 * Extracts value held by history value holder object.
	 * @param	history value holder object.
	 * @return	value held by history value holder object.
	 */
	public Object extractHistoryValueHolder(Object holder);

}
