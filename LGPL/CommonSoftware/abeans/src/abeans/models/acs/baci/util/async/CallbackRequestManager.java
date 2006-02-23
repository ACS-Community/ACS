/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.baci.util.async;

import abeans.engine.Request;

/**
 * Callback handler manager async. callback request registration and deregistration.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public interface CallbackRequestManager {

	/**
	 * Registers async. callback request.
	 * @param request	request to be regiestered.
	 * @param callbackHandler	callback handler to be notifies when callback is received, non-<code>null</code>.
	 * @return	request id, or 0 on regiestration failure.
	 */
	public int registerCallbackRequest(Request request, CallbackHandler callbackHandler);
	
	/**
	 * Deregisters async. callback request
	 * @param requestId	request id of the request to be deregistered.
	 */
	public void deregisterCallbackRequest(int requestId);
}
