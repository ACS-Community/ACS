/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.baci.util.async;

import abeans.engine.Request;
import alma.ACSErr.abeans.Completion;

/**
 * Callback handler.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public interface CallbackHandler {

	/**
	 * Callback notification - called immediately when handler has bren registered to the manager.
	 * @param requestId	request id.
	 * @param request request for which callback was received.
	 * @param requestManager request manager where this request is registered.
	 */
	public void registered(int requestId, Request request, CallbackRequestManager requestManager);

	/**
	 * Callback notification - called immediately when handler has been deregistered from the manager.
	 * @param requestId	request id.
	 * @param request request for which callback was received.
	 * @param requestManager request manager where this request was registered.
	 */
	public void deregistered(int requestId, Request request, CallbackRequestManager requestManager);

	/**
	 * Callback notification - working.
	 * @param requestId	request id.
	 * @param value	value received by callback, <code>null</code> if CBvoid.
	 * @param completion completion by callback.
	 * @param request request for which callback was received.
	 * @param requestManager request manager where this request is registered.
	 */
	public void working(int requestId, Object value, Completion completion, Request request, CallbackRequestManager requestManager);
	
	/**
	 * Callback notification - done.
	 * @param requestId	request id.
	 * @param value	value received by callback, <code>null</code> if CBvoid.
	 * @param completion completion by callback.
	 * @param request request for which callback was received.
	 * @param requestManager request manager where this request is registered.
	 */
	public void done(int requestId, Object value, Completion completion, Request request, CallbackRequestManager requestManager);
	
}
