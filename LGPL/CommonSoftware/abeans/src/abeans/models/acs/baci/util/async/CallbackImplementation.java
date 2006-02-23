/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.baci.util.async;

import abeans.engine.ResponseType;

/**
 * Callback interface.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public interface CallbackImplementation extends CallbackRequestManager
{

	/**
	 * Returns callback instance.
	 * @return callback instance.
	 */
	public alma.ACS.abeans.Callback getCallback();

	/**
	 * Returns callback response type.
	 * @return callback response type.
	 */
	public ResponseType getResponseType();
}
