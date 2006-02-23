/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.baci;

import java.util.logging.Level;

import abeans.core.defaults.MessageLogEntry;
import abeans.datatypes.AbeansDataExchangeException;
import abeans.datatypes.UpdateableUtilities;
import abeans.engine.EngineConstants;
import abeans.engine.Request;
import abeans.engine.RequestException;
import abeans.engine.RequestInterceptor;
import abeans.engine.Response;
import abeans.engine.ResponseType;
import abeans.engine.SimpleRequest;
import abeans.models.RequestUtilities;
import abeans.models.ResponseMulticastingEvent;
import abeans.models.acs.baci.util.async.CallbackImplementation;
import abeans.pluggable.Proxy;
import abeans.pluggable.RemoteException;
import abeans.pluggable.RemoteInfo;
import abeans.pluggable.acs.maci.NarrowCORBAProxy;

import com.cosylab.datatypes.AsynchronousAccess;
import com.cosylab.datatypes.ResponseListener;
import com.cosylab.util.NameValueList;

/**
 * This is a class containing convenience methods for invoking methods.
 * 
 * @author	Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 * @see		abeans.datatypes.CharacteristicContextUtilities
 */
public final class InvokeUtilities
{
	/**
	 * Multiplication factor for timeout delta trigger. If 0 calls are not timeouted. 
	 */
	public static final long ACS_TIMEOUT_MULTIPLIER=2;
	/**
	 * A constant denoting the request property that has <code>CallbackImplementation</code> value. 
	 */
	public static final String CB_KEY = "callback";
	 
	/**
	 * Implementation of async. request response dispatcher.
	 */
	private static class ResponseMulticastingEventImpl extends ResponseMulticastingEvent
	{
		/**
		 * Constructs a new event instance
		 * @param source	the object that will be the event source, non-<code>null</code>
		 * @param req		the request for which this event is being dispatched, non-<code>null</code>
		 * @param ri		the optional request interceptor for checking the request state, can 
		 * 					be <code>null</code>
		 */
		public ResponseMulticastingEventImpl(AsynchronousAccess source, Request req, RequestInterceptor ri)
		{
			super(source, req, ri);
		}

		/**
		 * Returns the listeners to which the event will be dispatched.
		 * 
		 * @return			a list of listeners, non-<code>null</code>
		 */
		protected ResponseListener[] getListeners()
		{
			return ((AsynchronousAccess)getSource()).getResponseListeners();
		}

	}

	/**
	 * Private constructor. This is a non-instantiable utility class.
	 */
	private InvokeUtilities()
	{
	}


	/**
	 * Invokes synch. method.
	 * 
	 * @param		parentInfo		the remote info to which the characteristic name and the get 	
	 * 								query will be appended; this remote info will be used as the 
	 * 								request target; non-<code>null</code>
	 * @param		method			the name of the mathod to be invoked, in string form,
	 * 								non-<code>null</code>
	 * @param		proxy			the proxy of the <code>invoker</code> that must be passed 
	 * 								to the pluggable layer with the request, non-<code>null</code>
	 * @param		timeout			the timeout to use with the request, in milliseconds, or 
	 * 								0 if the request is not to be timed
	 * @param		interceptor		the interceptor that will be passed the request for pre-submittal
	 * 								and post-submittal processing, may be <code>null</code> in which
	 * 								case no such processing will occur
	 * @param		invoker			the instance of modeling element on which method
	 * 								will be invoked (e.g. <code>Abean</code> or <code>Lbean</code>),
	 * 								non-<code>null</code>
	 * @return						return value
	 * @throws		RequestException
	 * 								thrown in two cases: if either the request sumittal fails or,
	 * 								if the <code>interceptor</code> post-subittal check detects an
	 * 								error, the <code>interceptor</code> raises an exception
	 */	
	public static Object invokeSync(RemoteInfo parentInfo, String method, Proxy proxy, long timeout, RequestInterceptor interceptor, Invokeable invoker, Object[] params) throws RequestException
	{
		assert (method != null);
		assert (method.length() != 0);
		assert (proxy != null);

		if (parentInfo == null || interceptor == null || invoker == null)
		{
				try
				{
					return invokeSync(method, (NarrowCORBAProxy)proxy, params);
				}
				catch (Throwable t)
				{
					if (invoker == null)
						throw new RuntimeException("Failed to invoke method '" + method + "'.", t);
						
					// exception needs request object
					SimpleRequest sr = invoker.getDatabase().createSimpleRequest(true);
					sr.setProxy(proxy);
					NameValueList nvl = new NameValueList(2);
					nvl.put(UpdateableUtilities.LATEST_REQUEST_INDEX_KEY, invoker);
					nvl.put("type", ResponseType.OBJECT);
					sr.setProperties(nvl);
					sr.setTarget(parentInfo.createQuery(method));
					sr.setTimeoutDelta(timeout*ACS_TIMEOUT_MULTIPLIER);
					sr.setParameters(params);
					throw new RequestException(invoker, "Failed to invoke method '" + method + "'.", sr, t);
				}
		}

		assert (parentInfo != null);
		assert (timeout >= 0);
		assert (invoker != null);

		// TODO logging references - prevents GC to finalize
		if (invoker.isDebug())
			new MessageLogEntry(invoker, "InvokeUtilities::invokeSync", new Object[] { parentInfo, method, proxy, new Long(timeout), interceptor, invoker, params }).dispatch();
		
		SimpleRequest sr = null;
		try
		{
			sr = invoker.getDatabase().createSimpleRequest(true);
			sr.setProxy(proxy);
			NameValueList nvl = new NameValueList(2);
			nvl.put(UpdateableUtilities.LATEST_REQUEST_INDEX_KEY, invoker);
			nvl.put("type", ResponseType.OBJECT);
			sr.setProperties(nvl);
			sr.setTarget(parentInfo.createQuery(method));
			sr.setTimeoutDelta(timeout);
			sr.setParameters(params);
			if (interceptor != null) interceptor.checkRequestBeforeSubmittal(sr);
			RequestUtilities.doSubmit(invoker.getDatabase(), sr);
			if (interceptor != null) interceptor.checkRequestAfterSubmittal(sr);
			if (interceptor != null) interceptor.checkRequestForErrors(sr, true);
			Response rp = sr.getLastResponse();
			Object retVal = rp.getValueAsObject();
			if (invoker.isDebug()) new MessageLogEntry(invoker, "InvokeUtilities::invokeSync", "Exiting.", Level.FINEST).dispatch();
			return retVal;
		} catch (RequestException re)
		{
			throw new RequestException(invoker, "Exception while executing Abeans Engine synchronous method.", sr, re);
		}	
	} 


	/**
	 * Invokes synch. method without any remote info, parent, database,... completely w/o db.
	 * 
	 * @param		method			the name of the mathod to be invoked, in string form,
	 * 								non-<code>null</code>
	 * @param		proxy			the proxy of the <code>invoker</code> that must be passed 
	 * 								to the pluggable layer with the request, non-<code>null</code>
	 * @return						return value
	 * @throws		RemoteException
	 * 								thrown in two cases: if either the request sumittal fails or,
	 * 								if the <code>interceptor</code> post-subittal check detects an
	 * 								error, the <code>interceptor</code> raises an exception
	 */	
	public static Object invokeSync(String method, NarrowCORBAProxy proxy, Object[] params) throws RemoteException
	{
		assert (method != null);
		assert (method.length() != 0);
		assert (proxy != null);
		
		return proxy.invoke(method, params);
	} 

	/**
	 * Invokes a method asynchronously.
	 * 
	 * @param		info		the remote info of the object no which the method will be invoked, non-<code>null</code>
	 * @param		method		the name of the mathod to be invoked, in string form,
	 * 							non-<code>null</code>
	 * @param		proxy		the proxy of the <code>invoker</code>, that will be passed in
	 * 							the request to the pluggable layer
	 * @param		timeout		the timeout in milliseconds to use, 0 will disable request timing,
	 * 							non-negative
	 * @param		interceptor	the interceptor used for pre-submittal and post-submittal request
	 * 							processing, may be <code>null</code>
	 * @param		requestor	the instance of modeling element on which method
	 * 							will be invoked (e.g. <code>Abean</code> or <code>Lbean</code>),
	 * 							non-<code>null</code>
	 * @param		cb			callback, non-<code>null</code>
	 * @param		params		parameters
	 * @return					the request that defines the asynchronous call
	 * @throws		RequestException	
	 * 							thrown in two cases: when the request submittal fails, or, if 
	 * 							<code>interceptor</code> is non-<code>null</code> and throws an
	 * 							exception during post-submittal request examination.	
	 */
	public static Request invokeAsync(RemoteInfo info, String method, Proxy proxy, long timeout, RequestInterceptor interceptor, Invokeable requestor, CallbackImplementation cb, Object[] params) throws RequestException
	{
		assert (info != null);
		assert (proxy != null);
		assert (method != null);
		assert (timeout >= 0);
		assert (requestor != null);
		assert (cb != null);
		
		// TODO logging references - prevents GC to finalize
		if (requestor.isDebug())
			new MessageLogEntry(requestor, "InvokeUtilities::invokeAsync", new Object[] { info, method, proxy, new Long(timeout), interceptor, requestor, cb, params }).dispatch();
		
		SimpleRequest sr = null;
		try
		{
			sr = requestor.getDatabase().createSimpleRequest(false);
			sr.setProxy(proxy);
			NameValueList nvl = new NameValueList(3);
			nvl.put(UpdateableUtilities.LATEST_REQUEST_INDEX_KEY, requestor);
			// type is obtained using callback interface
			//nvl.put("type", rt);
			//nvl.put("type", ResponseType.OBJECT);
			nvl.put(CB_KEY, cb);
			nvl.put(EngineConstants.ASYNC_KEY, Boolean.valueOf(true));
			sr.setProperties(nvl);
			sr.setTimeoutDelta(timeout*ACS_TIMEOUT_MULTIPLIER);
			sr.setParameters(params);
			sr.setTarget(info.createQuery(method));
			sr.setRequestCallback(new ResponseMulticastingEventImpl(requestor, sr, interceptor));
			if (interceptor != null) interceptor.checkRequestBeforeSubmittal(sr);
			RequestUtilities.doSubmit(requestor.getDatabase(), sr);
			if (interceptor != null) interceptor.checkRequestAfterSubmittal(sr);
			if (requestor.isDebug()) new MessageLogEntry(requestor, "InvokeUtilities::invokeAsync", "Exited.", Level.FINE).dispatch();
			return sr;
		} catch (RequestException re)
		{
			throw new RequestException(requestor, "Exception while executing Abeans Engine asynchronous method.", sr, re);
		}
	}

	/**
	 * Accesses a characteristic the type of which will be determined at runtime.
	 * This is known as a dynamic characteristic in datatypes. 
	 * See class javadoc to see a description of the
	 * standard characteristic get processing implemented by this method.
	 * 
	 * @param		parentInfo		the remote info to which the characteristic name and the get 	
	 * 								query will be appended; this remote info will be used as the 
	 * 								request target; non-<code>null</code>
	 * @param		name			the name of the characteristics, in string form,
	 * 								non-<code>null</code>
	 * @param		proxy			the proxy of the <code>invoker</code> that must be passed 
	 * 								to the pluggable layer with the request, non-<code>null</code>
	 * @param		timeout			the timeout to use with the request, in milliseconds, or 
	 * 								0 if the request is not to be timed
	 * @param		interceptor		the interceptor that will be passed the request for pre-submittal
	 * 								and post-submittal processing, may be <code>null</code> in which
	 * 								case no such processing will occur
	 * @param		invoker			the modeling element that owns the characteristic; the database of this
	 * 								element will be used to submit the request; this element will
	 * 								be used as the latest request index key, non-<code>null</code>
	 * @return						the characteristic value
	 * @throws		AbeansDataExchangeException
	 * 								thrown in two cases: if either the request sumittal fails or,
	 * 								if the <code>interceptor</code> post-subittal check detects an
	 * 								error, the <code>interceptor</code> raises an exception
	 */	
	public static Object getCharacteristic(RemoteInfo parentInfo, String name, Proxy proxy, long timeout, RequestInterceptor interceptor, Invokeable invoker) throws AbeansDataExchangeException
	{
		assert (parentInfo != null);
		assert (name != null);
		assert (name.length() != 0);
		assert (proxy != null);
		assert (timeout >= 0);
		assert (invoker != null);
		
		// TODO logging references - prevents GC to finalize
		if (invoker.isDebug())
			new MessageLogEntry(invoker, "InvokeUtilities::getCharacteristic", new Object[] { parentInfo, name, proxy, new Long(timeout), interceptor, invoker }).dispatch();
		
		try
		{
			SimpleRequest sr = invoker.getDatabase().createSimpleRequest(true);
			sr.setProxy(proxy);
			NameValueList nvl = new NameValueList(2);
			nvl.put(UpdateableUtilities.LATEST_REQUEST_INDEX_KEY, invoker);
			nvl.put("type", ResponseType.OBJECT);
			sr.setProperties(nvl);
			sr.setTarget(parentInfo.createHierarchyAndQuery(name, "get"));
			sr.setTimeoutDelta(timeout*ACS_TIMEOUT_MULTIPLIER);
			if (interceptor != null) interceptor.checkRequestBeforeSubmittal(sr);
			RequestUtilities.doSubmit(invoker.getDatabase(), sr);
			if (interceptor != null) interceptor.checkRequestAfterSubmittal(sr);
			if (interceptor != null) interceptor.checkRequestForErrors(sr, true);
			Response rp = sr.getLastResponse();
			Object retVal = rp.getValueAsObject();
			if (invoker.isDebug()) new MessageLogEntry(invoker, "InvokeUtilities::getCharacteristic", "Read characteristc '" + name + "', value is " + retVal + ".", Level.FINE).dispatch();
			if (invoker.isDebug()) new MessageLogEntry(invoker, "InvokeUtilities::getCharacteristic", "Exiting.", Level.FINEST).dispatch();
			return retVal;
		} catch (RequestException re)
		{
			throw new AbeansDataExchangeException(invoker, "Exception while executing Abeans Engine characteristic get reqeust for generic type (dynamic) characteristic.", re);
		}	
	} 
}
