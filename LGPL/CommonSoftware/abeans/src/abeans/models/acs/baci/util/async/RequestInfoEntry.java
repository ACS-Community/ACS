/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.baci.util.async;

import abeans.engine.Request;

/**
 * Class helper containing request data elements.
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public class RequestInfoEntry
{
	/**
	 * Manager CORBA referece.
	 */
	public Request request;
	
	/**
	 * ClientInfo structure.
	 */
	public CallbackHandler callbackHandler;

	/**
	 * Default constructor.
	 */
	public RequestInfoEntry(Request request, CallbackHandler callbackHandler)
	{
		this.request = request;
		this.callbackHandler = callbackHandler;
	}
}

