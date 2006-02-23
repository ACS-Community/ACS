/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.baci.util.async;

/**
 * Callback singleton factory (always returns the same callback).
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public interface CallbackSingletonFactory {
	
	/**
	 * Returns callback implementation.
	 * @return callback implementation.
	 */
	public CallbackImplementation getCallback();

}
