/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.baci.util.async;

import alma.ACSErr.abeans.Completion;
import abeans.core.Identifiable;
import abeans.core.Identifier;
import abeans.core.IdentifierSupport;
import abeans.engine.Request;
import abeans.engine.RequestCallback;
import abeans.engine.RequestResponseFactory;
import abeans.engine.Response;
import abeans.engine.ResponseType;
import abeans.pluggable.acs.maci.DatabaseProxyImpl;

/**
 * Default implementation of DefaultCallbackHandlerImpl.
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public class DefaultCallbackHandlerImpl implements CallbackHandler, Identifiable
{
	/**
	 * Identifier.
	 */
	protected transient Identifier id;

	/**
	 * Request response factory.
	 */
	protected RequestResponseFactory rrf = null;

	/**
	 * Request callback.
	 */
	protected RequestCallback rcb = null;
	
	/**
	 * Constructor. 
	 * @param rrf request response factory.
	 * @param rcb request callback.
	 */
	public DefaultCallbackHandlerImpl(RequestResponseFactory rrf, RequestCallback rcb)
	{
		this.rrf = rrf;
		this.rcb = rcb;

	}

	/**
	 * @see abeans.models.acs.baci.util.async.CallbackHandler#registered(int, abeans.engine.Request, abeans.models.acs.baci.util.async.CallbackRequestManager)
	 */
	public void registered(int requestId, Request request, CallbackRequestManager requestManager)
	{
		// noop
	}

	/**
	 * @see abeans.models.acs.baci.util.async.CallbackHandler#deegistered(int, abeans.engine.Request, abeans.models.acs.baci.util.async.CallbackRequestManager)
	 */
	public void deregistered(int requestId, Request request, CallbackRequestManager requestManager)
	{
		// noop
	}

	/**
	 * @see abeans.models.acs.baci.util.async.CallbackHandler#working(int, java.lang.Object, alma.ACSErr.abeans.Completion, abeans.engine.Request, abeans.models.acs.baci.util.async.CallbackRequestManager)
	 */
	public void working(int requestId, Object value, Completion completion,
						Request request, CallbackRequestManager requestManager)
	{
		// TODO determine response type (or is it not needed at all and performance loss) 
		ResponseType responseType;
		if (value == null)
			responseType = ResponseType.VOID;
		else
			responseType = ResponseType.OBJECT;
		
		Response response = rrf.createResponse(responseType, request, DatabaseProxyImpl.NO_ERROR_TYPE, DatabaseProxyImpl.NO_ERROR_CODE, System.currentTimeMillis());
		
		DatabaseProxyImpl.handleCompletion(this, request, completion, response);
		
		response.setValueAsObject(value);
		
		request.addResponse(response);
		rcb.requestNewResponse(response);

	}

	/**
	 * @see abeans.models.acs.baci.util.async.CallbackHandler#done(int, java.lang.Object, alma.ACSErr.abeans.Completion, abeans.engine.Request, abeans.models.acs.baci.util.async.CallbackRequestManager)
	 */
	public void done(int requestId, Object value, Completion completion,
						Request request, CallbackRequestManager requestManager)
	{
		requestManager.deregisterCallbackRequest(requestId);
	
		working(requestId, value, completion, request, requestManager);

		rcb.requestEnds(request);
	}

	/**
	 * @see abeans.core.Identifiable#getIdentifier()
	 */
	public Identifier getIdentifier()
	{
		if (id == null)
			id = new IdentifierSupport("Default Callback Handler", getClass().getName(), Identifier.PLUG);
		return id;
	}

	/**
	 * @see abeans.core.Identifiable#isDebug()
	 */
	public boolean isDebug()
	{
		return false;
	}

}
