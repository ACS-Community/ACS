/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.baci.util.async;

import alma.ACSErr.abeans.Completion;
import abeans.core.Identifier;
import abeans.core.IdentifierSupport;
import abeans.engine.Request;
import abeans.engine.RequestCallback;
import abeans.engine.RequestResponseFactory;
import abeans.engine.Response;
import abeans.engine.ResponseType;
import abeans.models.acs.baci.util.AbeansTypeConverter;
import abeans.pluggable.acs.maci.DatabaseProxyImpl;

/**
 * Callback handler implementation for properties which require value mapping.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public class PropertyValueCallbackHandlerImpl extends DefaultCallbackHandlerImpl
{

	/**
	 * Response type of this monitor.
	 */
	protected ResponseType responseType;

	/**
	 * Instance converting responses to Abeans type.
	 */
	protected AbeansTypeConverter converter;

	/**
	 * Constructor. 
	 * @param rrf request response factory.
	 * @param rcb request callback.
	 */
	public PropertyValueCallbackHandlerImpl(RequestResponseFactory rrf, RequestCallback rcb, AbeansTypeConverter converter, ResponseType responseType)
	{
		super(rrf, rcb);
		this.converter = converter;
		this.responseType = responseType;
		
		// TODO enum monitor workaround
		// pattern support
		if (responseType == ResponseType.LONG)
			this.responseType = ResponseType.OBJECT;
	}

	/**
	 * @see abeans.models.acs.baci.util.async.CallbackHandler#working(int, java.lang.Object, alma.ACSErr.abeans.Completion, abeans.engine.Request, abeans.models.acs.baci.util.async.CallbackRequestManager)
	 */
	public void working(int requestId, Object value, Completion completion,
						Request request, CallbackRequestManager requestManager)
	{
		
		Response response = rrf.createResponse(responseType, request, DatabaseProxyImpl.NO_ERROR_TYPE, DatabaseProxyImpl.NO_ERROR_CODE, System.currentTimeMillis());
		
		DatabaseProxyImpl.handleCompletion(this, request, completion, response);

		// also convert the value
		response.setValueAsObject(converter.toAbeansType(value));
		
		request.addResponse(response);
		rcb.requestNewResponse(response);

	}

	/**
	 * @see abeans.core.Identifiable#getIdentifier()
	 */
	public Identifier getIdentifier() {
		if (id == null)
			id = new IdentifierSupport("Property Callback Handler", getClass().getName(), Identifier.PLUG);
		return id;
	}


}
