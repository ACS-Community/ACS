/*
 * @@COPYRIGHT@@
 */

package abeans.pluggable.acs.maci;

import java.net.URI;
import java.util.Map;
import java.util.logging.Level;

import org.omg.CORBA.Any;
import org.omg.CORBA.TCKind;

import com.cosylab.datatypes.AbstractProperty;
import com.cosylab.datatypes.Condition;
import com.cosylab.datatypes.NumericProperty;
import com.cosylab.datatypes.PatternAbstractProperty;

import alma.ACS.abeans.CBDescIn;
import alma.ACS.abeans.CBvoidImpl;
import alma.ACSErr.abeans.Completion;
import alma.ACSErr.abeans.CompletionHolder;
import alma.ACS.abeans.Monitor;
import alma.ACS.abeans.Ppattern;
import alma.ACSErr.abeans.ErrorTrace;
import abeans.core.AssertionFailed;
import abeans.core.Identifiable;
import abeans.core.Identifier;
import abeans.core.IdentifierSupport;
import abeans.core.defaults.MessageLogEntry;
import abeans.datatypes.Constants;
import abeans.datatypes.DynamicValueConstants;
import abeans.engine.CompletionException;
import abeans.engine.EngineConstants;
import abeans.engine.GroupRequest;
import abeans.engine.Request;
import abeans.engine.RequestCallback;
import abeans.engine.RequestException;
import abeans.engine.RequestResponseFactory;
import abeans.engine.RequestType;
import abeans.engine.Response;
import abeans.engine.ResponseType;
import abeans.engine.SimpleRequest;
import abeans.models.acs.baci.Characteristics;
import abeans.models.acs.baci.CharacteristicComponent;
import abeans.models.acs.baci.InvokeUtilities;
import abeans.models.acs.baci.TypelessProperty;
import abeans.models.acs.baci.util.TimeConverter;
import abeans.models.acs.baci.util.async.CallbackImplementation;
import abeans.models.acs.baci.util.async.DefaultCallbackHandlerImpl;
import abeans.models.acs.baci.util.async.HistoryIteratorProxyImpl;
import abeans.models.acs.baci.util.async.MonitorProxyImpl;
import abeans.models.acs.baci.util.async.PropertyValueCallbackHandlerImpl;
import abeans.pluggable.DatabaseProxy;
import abeans.pluggable.NarrowConstants;
import abeans.pluggable.RemoteException;
import abeans.pluggable.RemoteInfo;

/**
 * The BACI-aware database proxy implementation of the MACI plug.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class DatabaseProxyImpl implements DatabaseProxy, Identifiable
{
	private ACSPlug plug = null;
	private Identifier id = null;
	private boolean debug = false;
	private String schemeString = null;

	// callbacks
	private RequestResponseFactory rrf = null;
	private RequestCallback cb = null;

	/**
	 * Completion type that indicates absence of errors.
	 */
	public static final int NO_ERROR_TYPE = 0;
	
	/**
	 * Completion code that indicates absence of errors.
	 */
	public static final int NO_ERROR_CODE = 0;

	/**
	 * Synch. get query name.
	 */
	private static final String GET_SYNC = "get_sync";

	/**
	 * Synch. set query name.
	 */
	private static final String SET_SYNC = "set_sync";

	/**
	 * History query name.
	 */
	public static final String NAME_HISTORY = "history";

	/**
	 * Characteristics query name.
	 */
	public static final String NAME_CHARACTERISTICS = "find_characteristic";

	/**
	 * Get characteristic query name.
	 */
	public static final String NAME_GET_CHARACTERISTIC = "get_characteristic_by_name";
	
	/**
	 * The multiplier with which the monitor request task timer will be multiplied to
	 * determine which timeout delta interval, in milliseconds, is to be set in the
	 * monitor query request. If this is 0, the monitor query requests will not be 
	 * timed.
	 */
	private static final long MONITOR_TIMEOUT_MULTIPLIER = 2;

	/**
	 * Creates a new instance of the database implementation. This is
	 * called automatically by the plug which passes itself as a parameter.
	 * 
	 * @param plug the plug instantiating this instance, non-<code>null</code>
	 */
	public DatabaseProxyImpl(ACSPlug plug)  throws RemoteException
	{
		assert (plug != null);
		this.plug = plug;		
		this.schemeString = "abeans-" + plug.getName();
	}

	/**
	 * This method delegates directly to the <code>process()</code> method.
	 * 
	 * @param	req				the request that is submitted and will be 
	 * 							processed, non-<code>null</code>
	 * @throws RemoteException if the processing fails
	 * @see 	abeans.pluggable.DatabaseProxy#submit(Request)
	 */
	public void submit(Request req) throws RemoteException
	{
		assert (req != null);
		try
		{
			process(req);
		} catch (RequestException re)
		{
			throw new RemoteException(this, "Exception during delegation of the request.", re);
		}
	}

	/**
	 * Stops a repeated request. The list of active requests that have scheduled
	 * tasks is first searched for the request; when found, the corresponding
	 * request task is cancelled.
	 * 
	 * @param	req				the request to stop, non-<code>null</code>
	 * @throws RemoteException if the stop operation fails
	 * @see 	abeans.pluggable.DatabaseProxy#stop(Request)
	 */
	public void stop(Request req) throws RemoteException
	{
		assert (req != null);
	}
	
	/**
	 * This method is invoked by the plug to set the request-response factory
	 * for this database implementation.
	 * @param rrf the factory to use, non-<code>null</code>
	 * @see abeans.pluggable.DatabaseProxy#setRequestResponseFactory(RequestResponseFactory)
	 */
	public void setRequestResponseFactory(RequestResponseFactory rrf)
	{
		assert (rrf != null);
		this.rrf = rrf;
	}

	/**
	 * This method is invoked by the plug to set the request callback
	 * for this database implementation.
	 * 
	 * @param	cb	the new callback to be used, non-<code>null</code>
	 * @see 	abeans.pluggable.DatabaseProxy#setRequestResponseCallback(RequestCallback)
	 */
	public void setRequestResponseCallback(RequestCallback cb)
	{
		assert (cb != null);
		this.cb = cb;
	}

	/**
	 * @see abeans.pluggable.DatabaseProxy#resume()
	 */
	public void resume()
	{
		// no operation
	}

	/**
	 * @see abeans.pluggable.DatabaseProxy#suspend()
	 */
	public void suspend()
	{
		// no operation
	}

	/**
	 * Returns the request callback through which the simulated objects may
	 * inform the Abeans Engine about the new responses or the change of status
	 * of the request.
	 * 
	 * @return the Abeans Engine database
	 */
	public RequestCallback getRequestCallback()
	{
		return cb;
	}
	
	/**
	 * Returns the factory for producing new responses. 
	 * 
	 * @return response factory used by simulated 
	 * 			entities to generate responses
	 */
	public RequestResponseFactory getRequestResponseFactory()
	{
		return rrf;
	}

	/**
	 * Returns identifier for this class.
	 * 
	 * @return the identifier object for this instance
	 * @see 	abeans.core.Identifiable#getIdentifier()
	 */
	public Identifier getIdentifier()
	{
		if (id == null) {
			id = new IdentifierSupport("MACI Database Proxy", "MACIDBProxy", Identifier.PLUG);
		}
		return id;
	}

	/**
	 * Processes the request. The database cannot process any request directly, all
	 * requests must be directed either to the query child of the database or to
	 * the container child. The processing starts by resolving the target contained
	 * within the request. If the resolution succeeds, the request is forwarded to
	 * the resolved simulated entity. Otherwise the processing fails.
	 * 
	 * @param	request				the request to process, non-<code>null</code>
	 * @throws RemoteException 	if the name resolution fails
	 * @throws RequestException 	if the request is malformed
	 */
	private void process(Request request) throws RemoteException, RequestException
	{
		assert (request != null);
	
		if (isDebug())
			new MessageLogEntry(this, "process", request.toString(), Level.FINE).dispatch();
	
		if (request instanceof GroupRequest)
			throw new RequestException(this, "Currently do not know how to handle group requests.", request);

		SimpleRequest sr = (SimpleRequest)request;
		
		if (sr.getTarget() == null)
			throw new RequestException(this, "A request has been submitted that has a null target specification.", request);
			
		if (sr.getTarget().getName() == null || sr.getTarget().getName().length()==0)
			throw new RequestException(this, "A request has been submitted that contains an empty name in the target specification.", request);
		

		URI uri = sr.getTarget().toURI();
		
		// check the scheme, if we can handle the request
		if (!checkURI(uri))
			throw new RequestException(this, "Unsupported URI: " + uri.toString(), request);

		if (isDebug())
			new MessageLogEntry(this, "process", uri.toString(), Level.FINE).dispatch();

		String path = uri.getPath().substring(1);
		String query = uri.getQuery();

		//
		// get proxy
		//
		if (request.getProxy() == null)
		{
			RequestException re = new RequestException(this, "Non-null proxy required, but null proxy given.", request);
			re.caughtIn(this, "process");
			throw re;
		}
		else if (!(request.getProxy() instanceof NarrowCORBAProxy))
		{
			RequestException re = new RequestException(this, "Only 'NarrowCORBAProxy' proxies are supported by this plug.", request);
			re.caughtIn(this, "process");
			throw re;
		}

		NarrowCORBAProxy proxy = (NarrowCORBAProxy)request.getProxy();

		if (proxy.getOwner() instanceof TypelessProperty)
		{
			processPropertyRequest(request, proxy, path, query);		
		}
		else if (query.equals(NarrowConstants.GET_QUERY) &&
				 proxy.getOwner() instanceof CharacteristicComponent)
		{
			processGetCharacteristicQuery(request, proxy, null, path, query);		
		}
		else
		{
			processInternal(request, proxy, path, query);
		}

	}

	/**
	 * Processes the property query request. The request is first started, the context producer
	 * is checked. The request is then checked for correctness. A new response
	 * is created with the type defined by the type of property to which this query 
	 * belongs. The result is inserted into the response, which is submitted back
	 * to the database. Because the request is single, this completes the 
	 * request.
	 * 
	 * @param	Request				the request to process, non-<code>null</code>
	 * @param	proxy				narrow CORBA proxy, non-<code>null</code>
	 * @param 	path				path to be read (property or characteristic), non-<code>null</code>
	 * @param	query				query (method to be invoked) on object, non-<code>null</code>
	 * @throws	RemoteException 	when the get operation on the property fails
	 * @throws	RequestException	if the request is malformed
	 */
	private void processPropertyRequest(Request request, NarrowCORBAProxy proxy, String path, String query) throws RemoteException, RequestException
	{
		assert (request != null);

		boolean onChildRequest = false;
		
		TypelessProperty property = (TypelessProperty)proxy.getOwner();
		RemoteInfo ri = property.getRemoteInfo();
		if (ri != null)
		{
			String proxyPath = ri.toURI().getPath().substring(1);
			if (!proxyPath.equals(path))
			{
				if (!path.startsWith(proxyPath))
				{
					RequestException re = new RequestException(this, "Request has wrong remote info which is not consistant with proxy: " + proxyPath + " ~ " + path + ".", request);
					request.setError(re);
					getRequestCallback().requestEnds(request);
					throw re;
				}
				else
				{
					// we have got a request on property child
					onChildRequest = true;
				}
			}
		
		}
		else
		{
			// TODO query for name, and try to do sth with it...
		}

		if (onChildRequest)
		{
			if (query.equals(NarrowConstants.GET_QUERY))
				processGetCharacteristicQuery(request, proxy, property, path, query);
			else
			{
				RequestException re = new RequestException(this, "Invalid query '" + query + "' on property child '" + path + "'.", request);
				request.setError(re);
				getRequestCallback().requestEnds(request);
				throw re;
			}
		}
		else if (query.equals(NarrowConstants.GET_QUERY) ||
			query.equals(Constants.Q_GET_ASYNC))
		{
			processGetQuery(request, proxy, path, query, property);
		}
		else if (query.equals(NarrowConstants.SET_QUERY) ||
				 query.equals(Constants.Q_SET_ASYNC))
		{
			processSetQuery(request, proxy, path, query, property);
		}
		else if (query.equals(NarrowConstants.MONITOR_QUERY))
		{
			processMonitorQuery(request, proxy, property);
		}
		else if (query.equals(NAME_HISTORY))
		{
			processHistoryQuery(request, proxy, property);
		}
		else if (query.equals(NarrowConstants.SETTABLE_QUERY))
		{
			// check request type
			if (request.getRequestType() != null && request.getRequestType() == RequestType.REPEATED_REQUEST)
			{
				RequestException re = new RequestException(this, "'" + query + "' query allows only single requests.", request);
				re.caughtIn(this, "processPropertyRequest");
				request.setError(re);
				getRequestCallback().requestEnds(request);
				throw re;
			}
			// if not specified, set it
			else if (request.getRequestType() == null)
			{
				request.setRequestType(RequestType.SINGLE_REQUEST);
			}
			
			// start request
			getRequestCallback().requestStarts(request);
			
			// create response and read settable value
			Response response = rrf.createResponse(ResponseType.OBJECT, request, NO_ERROR_TYPE, NO_ERROR_CODE, System.currentTimeMillis());
			response.setValueAsObject(new Boolean(property.isSettable()));
			request.addResponse(response);
			
			// dispatch request
			getRequestCallback().requestNewResponse(response);
			getRequestCallback().requestEnds(request);
		}
		
		// delegate actions
		else
		{
			processInternal(request, proxy, path, query);
		}
	}

	/**
	 * Processes the monitor query request.
	 * 
	 * @param	request				the request to process, non-<code>null</code>
	 * @param	proxy				narrow CORBA proxy, non-<code>null</code>
	 * @param	property			Abeans property model implementation to monitor, non-<code>null</code>
	 * @throws	RemoteException 	when the get operation on the property fails
	 * @throws	RequestException	if the request is malformed
	 */
	private void processMonitorQuery(Request request, NarrowCORBAProxy proxy, TypelessProperty property) throws RemoteException, RequestException
	{
		assert (request != null);
		assert (proxy != null);
		assert (property != null);
	
		if (request.getRequestType() != null && request.getRequestType() != RequestType.REPEATED_REQUEST)
		{
			RequestException re = new RequestException(this, "Monitor query only supports repeated requests.", request);
			re.caughtIn(this, "processMonitorQuery");
			re.putValue("request", request);
			throw re;
		}
		else if (request.getRequestType() == null) request.setRequestType(RequestType.REPEATED_REQUEST);
		
		// read property default timer trigger characteristic
		long defaultTimerTrigger = 1000;
		try
		{
			Object retVal = proxy.invoke("_get_" + Characteristics.NAME_DEFAULT_TIMER_TRIGGER, null);
			// 100-ns -> ms;
			defaultTimerTrigger = ((Long)retVal).longValue()/10000;
		}
		catch (RemoteException re)
		{
			// noop
		}
		
		// set requestTimeout to enable timeouts, if not already set
		if (request.getTimeoutDelta() == 0)
			request.setTimeoutDelta(defaultTimerTrigger * MONITOR_TIMEOUT_MULTIPLIER);
		
		// get property callback implementation
		CallbackImplementation cbi = property.getCallback();
		
		MonitorProxyImpl monitorImpl = new MonitorProxyImpl(rrf, cb, property, cbi.getResponseType(), defaultTimerTrigger);
		
		int requestId = cbi.registerCallbackRequest(request, monitorImpl);
		
		long normalTimeout = 5*defaultTimerTrigger;
		CBDescIn descIn = new CBDescIn(normalTimeout, normalTimeout, requestId);
		
		Object retVal = proxy.invoke("create_monitor", new Object[] { cbi.getCallback(), descIn });
		
		Monitor monitor = (Monitor)retVal;
		monitorImpl.setUp(monitor);
		
		request.setProxy(monitorImpl);

		// proxy must be set before requestStarted callback is sent to the database
		getRequestCallback().requestStarts(request);

	}

	/**
	 * Processes the history query request.
	 * 
	 * @param	request				the request to process, non-<code>null</code>
	 * @param	proxy				narrow CORBA proxy, non-<code>null</code>
	 * @param	property			Abeans property model implementation to query for history, non-<code>null</code>
	 * @throws	RemoteException 	when the get operation on the property fails
	 * @throws	RequestException	if the request is malformed
	 */
	private void processHistoryQuery(Request request, NarrowCORBAProxy proxy, TypelessProperty property) throws RemoteException, RequestException
	{
		assert (request != null);
		assert (proxy != null);
		assert (property != null);
	

		if (request.getRequestType() != null && request.getRequestType() != RequestType.SINGLE_REQUEST)
		{
			RequestException re = new RequestException(this, "History query only supports single requests.", request);
			re.caughtIn(this, "processHistoryQuery");
			re.putValue("request", request);
			throw re;
		}
		else if (request.getRequestType() == null) request.setRequestType(RequestType.SINGLE_REQUEST);

		// read history properties
		long startTime = 0;
		long stopTime = 0;
		int maximumHistoryElements = HistoryIteratorProxyImpl.MAXIMUM_ELEMENTS;
		Map properties = request.getProperties();
		if (properties != null)
		{
			Long longObj = (Long)properties.get("startTime");
			if (longObj != null)
				startTime = longObj.longValue();

			longObj = (Long)properties.get("stopTime");
			if (longObj != null)
				stopTime = longObj.longValue();

			Integer intObj = (Integer)properties.get("maximumHistoryElements");
			if (intObj != null)
				maximumHistoryElements = intObj.intValue();

			// property "deltaTime" (Long) is ignored
		}

		// set history iterator proxy		
		request.setProxy(new HistoryIteratorProxyImpl(property, proxy,
													  maximumHistoryElements, startTime, stopTime,
													  request, getRequestCallback()));

		// proxy must be set before requestStarted callback is sent to the database
		getRequestCallback().requestStarts(request);
	}

	/**
	 * Processes the get characteristic query request. The request is first started, the context producer
	 * is checked. The request is then checked for correctness. A new response
	 * is created with the type defined by the type of property to which this query 
	 * belongs. The result is inserted into the response, which is submitted back
	 * to the database. Because the request is single, this completes the 
	 * request.
	 * 
	 * @param	Request				the request to process, non-<code>null</code>
	 * @param	proxy				narrow CORBA proxy, non-<code>null</code>
	 * @param 	path				path to be read, non-<code>null</code>
	 * @param	query				query (method to be invoked) on object, non-<code>null</code>
	 * @param	property			Abeans property model implementation to property,
	 * 								if query is requested on non-property it equals <code>null</code> 
	 * @throws	RemoteException 	when the get operation on the property fails
	 * @throws	RequestException	if the request is malformed
	 */
	private void processGetCharacteristicQuery(Request request, NarrowCORBAProxy proxy, TypelessProperty property, String path, String query) throws RemoteException, RequestException
	{
		assert (request != null);

		String proxyPath = path.substring(0, path.lastIndexOf("/"));
		String characteristic = path.substring(proxyPath.length()+1);

		// handle characteristics seperately
		if (characteristic.equals(NarrowConstants.CHARACTERISTICS_QUERY))
		{
			processGetCharacteristicsQuery(request, proxy, path);
			return;
		}
		
		getRequestCallback().requestStarts(request);
	
		if (request.getRequestType() != null && request.getRequestType() == RequestType.REPEATED_REQUEST)
		{
			RequestException re = new RequestException(this, "'get' query allows only single requests.", request);
			re.caughtIn(this, "processGetCharacteristicQuery");
			request.setError(re);
			getRequestCallback().requestEnds(request);
			throw re;
		}
		else if (request.getRequestType() == null)
		{
			request.setRequestType(RequestType.SINGLE_REQUEST);
		}

		//
		// determine characteristic query
		//

		Object[] parameters = null;

		path = proxyPath;

		String mappedCharacteristic = mapCharacteristicName(characteristic);
		if (mappedCharacteristic == null)
		{
			// unknown characteristics, try get_characteristic_by_name
			query = NAME_GET_CHARACTERISTIC;
			mappedCharacteristic = characteristic;
			parameters = new Object[] { characteristic };
		}
		else
		{
			
			// special case: for RO properties return minimum/maximum as graphMin/graphMax
			// (maybe this should be moved to mapCharacteristicName)
			if (property != null && !property.isSettable())
			{
				if (mappedCharacteristic.equals(Characteristics.NAME_MAXIMUM))
					mappedCharacteristic = Characteristics.NAME_GRAPH_MAX;
				else if (mappedCharacteristic.equals(Characteristics.NAME_MINIMUM))
					mappedCharacteristic = Characteristics.NAME_GRAPH_MIN;						 
			}
			
			// characteristic is known, map to accessor
			query = "_get_" + mappedCharacteristic;
		}

		//
		// determine response type
		//
		ResponseType responseType = null;
		Map properties = request.getProperties();
		if (properties != null)
			responseType = (ResponseType)properties.get("type");
			
		if (responseType == null)
		{
			RequestException re = new RequestException(this, "'get' query reqires ('type', RequestType) entry in request properties.", request);
			re.caughtIn(this, "processGetCharacteristicQuery");
			request.setError(re);
			getRequestCallback().requestEnds(request);
			throw re;
		}
		
		try
		{
			Object retVal = null;
			

			// pattern is a special case
			if (property instanceof Ppattern)
			{
				if (mappedCharacteristic.equals(Characteristics.NAME_GRAPH_MAX) ||
					mappedCharacteristic.equals(Characteristics.NAME_MAXIMUM))
					retVal = new Integer(Integer.MAX_VALUE);
				else if (mappedCharacteristic.equals(Characteristics.NAME_GRAPH_MIN) ||
						 mappedCharacteristic.equals(Characteristics.NAME_MINIMUM))
					retVal = new Integer(Integer.MIN_VALUE);
			}
			
			// ACS has no core support for C_SCALE_TYPE
			if (mappedCharacteristic.equals(NumericProperty.C_SCALE_TYPE))
					retVal = "linear";
			
			// TODO using settable tweak, put attribute to the directory
			if (property != null &&
			    mappedCharacteristic.equals(DynamicValueConstants.SETTABLE_QUERY)) {
					retVal = Boolean.valueOf(property.isSettable());
					parameters = null;
			}
			
			if (retVal == null)
				retVal = proxy.invoke(query, parameters);

			// do the type mapping for pattern
			if (mappedCharacteristic.equals(Characteristics.NAME_RESOLUTION) &&
			    property instanceof Ppattern && retVal != null)
			{
				retVal = com.cosylab.util.BitSetUtilities.forLong(((Number)retVal).longValue());
			}

			// extract value from Any
			if (parameters != null)
			{
				// TODO this is not perfectly clean code...
				// ACS returns only string any
				responseType = ResponseType.STRING;
				retVal = extractAny((Any)retVal);
			}
			
			Response response = rrf.createResponse(responseType, request, NO_ERROR_TYPE, NO_ERROR_CODE, System.currentTimeMillis());
			
			// mapping for property
			if (retVal != null && property != null)
			{
				if (mappedCharacteristic.equals(Characteristics.NAME_DEFAULT_VALUE) ||
					mappedCharacteristic.equals(Characteristics.NAME_GRAPH_MAX) ||
					mappedCharacteristic.equals(Characteristics.NAME_GRAPH_MIN) ||
					mappedCharacteristic.equals(Characteristics.NAME_MAXIMUM) ||
					mappedCharacteristic.equals(Characteristics.NAME_MINIMUM) ||
					mappedCharacteristic.equals(Characteristics.NAME_MIN_STEP) ||
					mappedCharacteristic.equals(Characteristics.NAME_MINIMAL_DELTA_TRIGGER) ||
		
					mappedCharacteristic.equals(Characteristics.NAME_ALARM_HIGH_OFF) ||
					mappedCharacteristic.equals(Characteristics.NAME_ALARM_HIGH_ON) ||
					mappedCharacteristic.equals(Characteristics.NAME_ALARM_LOW_OFF) ||
					mappedCharacteristic.equals(Characteristics.NAME_ALARM_LOW_ON))
				retVal = property.toAbeansType(retVal);

				// map conditions
				else if (mappedCharacteristic.equals(Characteristics.NAME_WHEN_SET) ||
						 mappedCharacteristic.equals(Characteristics.NAME_WHEN_CLEARED))
				{
						alma.ACS.abeans.Condition[] returnedCondition = (alma.ACS.abeans.Condition[])retVal;
						Condition[] condition = new Condition[returnedCondition.length];
						for (int i = 0; i < condition.length; i++)
							if (returnedCondition[i] == alma.ACS.abeans.Condition.RED)
								condition[i] = Condition.ERROR;
							else if (returnedCondition[i] == alma.ACS.abeans.Condition.YELLOW)
								condition[i] = Condition.WARNING;
							else if (returnedCondition[i] == alma.ACS.abeans.Condition.GREEN)
								condition[i] = Condition.OK;
							else //if (returnedCondition[i] == alma.ACS.abeans.Condition.GREY)
								condition[i] = Condition.UNUSED;
						retVal = condition;
				}

			}
					
			response.setValueAsObject(retVal);
			
			request.addResponse(response);
			getRequestCallback().requestNewResponse(response);

			getRequestCallback().requestEnds(request);

		} catch (Exception re)
		{
			CompletionException ce = new CompletionException(this, "Failed to get the characteristic value '" + characteristic + "' of the '" + path + "'.", request, re);
			ce.caughtIn(this, "processGetCharacteristicQuery");
			ce.putValue("request", request);

			Response response = rrf.createResponse(responseType, request, NO_ERROR_TYPE, NO_ERROR_CODE, System.currentTimeMillis());
			response.setError(ce);
			request.addResponse(response);
			getRequestCallback().requestEnds(request);
			return;
		}
		
	}

	/**
	 * Processes the get characteristics query request
	 * (queries for all the characteristics names of the object).
	 * The request is first started, the context producer is checked.
	 * The request is then checked for correctness. A new response
	 * is created with the type defined by the type of property to which this query 
	 * belongs. The result is inserted into the response, which is submitted back
	 * to the database. Because the request is single, this completes the 
	 * request.
	 * 
	 * @param	Request				the request to process, non-<code>null</code>
	 * @param	proxy				narrow CORBA proxy, non-<code>null</code>
	 * @param 	path				path to be read, non-<code>null</code>
	 * @throws	RemoteException 	when the get operation on the property fails
	 * @throws	RequestException	if the request is malformed
	 */
	private void processGetCharacteristicsQuery(Request request, NarrowCORBAProxy proxy, String path) throws RemoteException, RequestException
	{
		assert (request != null);
		
		getRequestCallback().requestStarts(request);
	
		if (request.getRequestType() != null && request.getRequestType() == RequestType.REPEATED_REQUEST)
		{
			RequestException re = new RequestException(this, "'get' query allows only single requests.", request);
			re.caughtIn(this, "processGetCharacteristicsQuery");
			request.setError(re);
			getRequestCallback().requestEnds(request);
			throw re;
		}
		else if (request.getRequestType() == null)
		{
			request.setRequestType(RequestType.SINGLE_REQUEST);
		}
	
		ResponseType responseType = ResponseType.STRING_SEQ;


		try
		{
			Object[] parameters = (Object[])request.getParameters();
			
			// all
			if (parameters == null || parameters.length == 0)
				parameters = new Object[] { "*" };
				
			Object retVal = proxy.invoke(NAME_CHARACTERISTICS, parameters);

			Response response = rrf.createResponse(responseType, request, NO_ERROR_TYPE, NO_ERROR_CODE, System.currentTimeMillis());

			response.setValueAsObject(retVal);
			
			request.addResponse(response);
			getRequestCallback().requestNewResponse(response);
	
			getRequestCallback().requestEnds(request);

		} catch (Exception re)
		{
			CompletionException ce = new CompletionException(this, "Failed to get the characteristics of the '" + path + "'.", request, re);
			ce.caughtIn(this, "processGetCharacteristicsQuery");
			ce.putValue("request", request);

			Response response = rrf.createResponse(responseType, request, NO_ERROR_TYPE, NO_ERROR_CODE, System.currentTimeMillis());
			response.setError(ce);
			request.addResponse(response);
			getRequestCallback().requestEnds(request);
			return;
		}
		
	}

	/**
	 * Processes the get characteristic query request. The request is first started, the context producer
	 * is checked. The request is then checked for correctness. A new response
	 * is created with the type defined by the type of property to which this query 
	 * belongs. The result is inserted into the response, which is submitted back
	 * to the database. Because the request is single, this completes the 
	 * request.
	 * 
	 * @param	Request				the request to process, non-<code>null</code>
	 * @param	proxy				narrow CORBA proxy, non-<code>null</code>
	 * @param 	path				path to be read (property or characteristic), non-<code>null</code>
	 * @param	query				query (method to be invoked) on object, non-<code>null</code>
	 * @param	property			Abeans property model implementation, non-<code>null</code>
	 * @throws	RemoteException 	when the get operation on the property fails
	 * @throws	RequestException	if the request is malformed
	 */
	private void processGetQuery(Request request, NarrowCORBAProxy proxy, String path, String query, TypelessProperty property) throws RemoteException, RequestException
	{
		assert (request != null);
		
		getRequestCallback().requestStarts(request);
	
		if (request.getRequestType() != null && request.getRequestType() == RequestType.REPEATED_REQUEST)
		{
			RequestException re = new RequestException(this, "'get' query allows only single requests.", request);
			re.caughtIn(this, "processGetQuery");
			request.setError(re);
			getRequestCallback().requestEnds(request);
			throw re;
		}
		else if (request.getRequestType() == null)
		{
			request.setRequestType(RequestType.SINGLE_REQUEST);
		}

		if (query.equalsIgnoreCase(NarrowConstants.GET_QUERY))
			query = GET_SYNC;

		// we have got a characteristic, fix query and path

		//
		// determine response type
		//
		ResponseType responseType = ResponseType.OBJECT;
		Map properties = request.getProperties();
		if (properties != null)
		{
			responseType = (ResponseType)properties.get("type");
			if (responseType == null)
				responseType = ResponseType.OBJECT;
		}
			
		try
		{

			if (query.equals(GET_SYNC))
			{
				
				CompletionHolder completionHolder = new CompletionHolder();
				
				Object[] parameters = new Object[] { completionHolder };
				Object retVal = proxy.invoke(query, parameters);

				Response response = rrf.createResponse(responseType, request, NO_ERROR_TYPE, NO_ERROR_CODE, System.currentTimeMillis());
				
				handleCompletion(this, request, completionHolder.value, response);
				
				response.setValueAsObject(property.toAbeansType(retVal));
				
				request.addResponse(response);
				getRequestCallback().requestNewResponse(response);

				getRequestCallback().requestEnds(request);

			}
			else if (query.equals(Constants.Q_GET_ASYNC))
			{
				
				// get property callback implementation
				CallbackImplementation cbi = property.getCallback();
			
				// TODO do not create handler every time, if possible
				PropertyValueCallbackHandlerImpl handler = new PropertyValueCallbackHandlerImpl(rrf, cb, property, cbi.getResponseType());
			
				int requestId = cbi.registerCallbackRequest(request, handler);
			
				long normalTimeout = request.getTimeoutDelta() * 10000;
				CBDescIn descIn = new CBDescIn(normalTimeout, normalTimeout, requestId);
			
				proxy.invoke("get_async", new Object[] { cbi.getCallback(), descIn });

				// callback handler will generate the response
			
			}
			
		} catch (Exception re)
		{
			CompletionException ce = new CompletionException(this, "Failed to get the value of the '" + path + "'.", request, re);
			ce.caughtIn(this, "processGetQuery");
			ce.putValue("request", request);

			Response response = rrf.createResponse(responseType, request, NO_ERROR_TYPE, NO_ERROR_CODE, System.currentTimeMillis());
			response.setError(ce);
			request.addResponse(response);
			getRequestCallback().requestEnds(request);
			return;
		}
		
	}

	/**
	 * Processes the set characteristic query request. The request is first started, the context producer
	 * is checked. The request is then checked for correctness. A new response
	 * is created with the type defined by the type of property to which this query 
	 * belongs. The result is inserted into the response, which is submitted back
	 * to the database. Because the request is single, this completes the 
	 * request.
	 * 
	 * @param	Request				the request to process, non-<code>null</code>
	 * @param	proxy				narrow CORBA proxy, non-<code>null</code>
	 * @param 	path				path to be read (property or characteristic), non-<code>null</code>
	 * @param	query				query (method to be invoked) on object, non-<code>null</code>
	 * @param	property			Abeans property model implementation, non-<code>null</code>
	 * @throws	RemoteException 	when the get operation on the property fails
	 * @throws	RequestException	if the request is malformed
	 */
	private void processSetQuery(Request request, NarrowCORBAProxy proxy, String path, String query, TypelessProperty property) throws RemoteException, RequestException
	{
		assert (request != null);
		
		getRequestCallback().requestStarts(request);
	
		if (request.getRequestType() != null && request.getRequestType() == RequestType.REPEATED_REQUEST)
		{
			RequestException re = new RequestException(this, "'set' query allows only single requests.", request);
			re.caughtIn(this, "processSetQuery");
			request.setError(re);
			getRequestCallback().requestEnds(request);
			throw re;
		}
		else if (request.getRequestType() == null)
		{
			request.setRequestType(RequestType.SINGLE_REQUEST);
		}

		if (query.equalsIgnoreCase(NarrowConstants.SET_QUERY))
			query = SET_SYNC;

		try
		{
			Object value = request.getParameters()[0];
			value = property.fromAbeansType(value);

			if (query.equals(SET_SYNC))
			{
				
				Object retVal = proxy.invoke(query, new Object[] { value });

				Response response = rrf.createResponse(ResponseType.VOID, request, NO_ERROR_TYPE, NO_ERROR_CODE, System.currentTimeMillis());
				
				handleCompletion(this, request, (Completion)retVal, response);

				request.addResponse(response);
				getRequestCallback().requestNewResponse(response);

				getRequestCallback().requestEnds(request);

			}
			else if (query.equals(Constants.Q_SET_ASYNC))
			{
				
				// create void callback implementation
				CallbackImplementation cbi = CBvoidImpl.getInstance(); 
			
				// TODO do not create handler every time, if possible
				DefaultCallbackHandlerImpl handler = new DefaultCallbackHandlerImpl(rrf, cb);
			
				int requestId = cbi.registerCallbackRequest(request, handler);
			
				long normalTimeout = request.getTimeoutDelta() * 10000;
				CBDescIn descIn = new CBDescIn(normalTimeout, normalTimeout, requestId);
			
				proxy.invoke("set_async", new Object[] { value, cbi.getCallback(), descIn });
				
				// callback handler will generate the response
			}
			
		} catch (Exception re)
		{
			CompletionException ce = new CompletionException(this, "Failed to set the value of the '" + path + "'.", request, re);
			ce.caughtIn(this, "processSetQuery");
			ce.putValue("request", request);

			Response response = rrf.createResponse(ResponseType.VOID, request, NO_ERROR_TYPE, NO_ERROR_CODE, System.currentTimeMillis());
			response.setError(ce);
			request.addResponse(response);
			getRequestCallback().requestEnds(request);
			return;
		}
		
	}

	/**
	 * Processes the request. The request is first started, the context producer
	 * is checked. The request is then checked for correctness. A new response
	 * is created with the type defined by the type of property to which this query 
	 * belongs. The result is inserted into the response, which is submitted back
	 * to the database. Because the request is single, this completes the 
	 * request.
	 * 
	 * @param	request				the request to process, non-<code>null</code>
	 * @param	path				object on which to process request, non-<code>null</code>
	 * @param	query				query (method to be invoked) on object, non-<code>null</code>
	 * @throws	RemoteException 	when the get operation on the property fails
	 * @throws	RequestException	if the request is malformed
	 */
	private void processInternal(Request request, NarrowCORBAProxy proxy, String path, String query) throws RemoteException, RequestException
	{
		assert (request != null);
		
		getRequestCallback().requestStarts(request);
	
		if (request.getRequestType() != null && request.getRequestType() == RequestType.REPEATED_REQUEST)
		{
			RequestException re = new RequestException(this, "Only single requests are supported.", request);
			re.caughtIn(this, "processInternal");
			request.setError(re);
			getRequestCallback().requestEnds(request);
			throw re;
		}
		else if (request.getRequestType() == null)
		{
			request.setRequestType(RequestType.SINGLE_REQUEST);
		}

		//
		// determine response type
		//
		ResponseType responseType = ResponseType.VOID;
		Map properties = request.getProperties();
		if (properties != null)
		{
			responseType = (ResponseType)properties.get("type");
			if (responseType == null)
				responseType = ResponseType.VOID;
		}

		boolean isAsync = false;
		if (properties != null)
		{
			Boolean asyncVal = (Boolean)properties.get(EngineConstants.ASYNC_KEY);
			if (asyncVal != null)
				isAsync = asyncVal.booleanValue();
		}
		
		try
		{
			if (!isAsync)
			{
				Object[] parameters = request.getParameters();
				Object retVal = proxy.invoke(query, parameters);

				if (retVal != null)
					responseType = ResponseType.OBJECT;
					
				Response response = rrf.createResponse(responseType, request, NO_ERROR_TYPE, NO_ERROR_CODE, System.currentTimeMillis());
	
				// completion check
				if (!isAsync)
				{
					// return value
					if (retVal instanceof Completion)			
						handleCompletion(this, request, (Completion)retVal, response);
					// last parameter
					else if (parameters != null && parameters.length > 0 && 
							 parameters[parameters.length-1] instanceof CompletionHolder)
						handleCompletion(this, request, ((CompletionHolder)parameters[parameters.length-1]).value, response);
				}
	
				response.setValueAsObject(retVal);

				request.addResponse(response);
				getRequestCallback().requestNewResponse(response);

				getRequestCallback().requestEnds(request);
			}
			else
			{

				//CBvoidImpl.getInstance(); 
				CallbackImplementation cbi = (CallbackImplementation)properties.get(InvokeUtilities.CB_KEY);
				
				if (cbi == null)
				{
					AssertionFailed af = new AssertionFailed(this, "No callback implementation property whith key 'callback' given.");
					af.caughtIn(this, "processInternal");
					af.putValue("cbi", cbi);
					throw af;
				}
			
				// TODO do not create handler every time, if possible
				DefaultCallbackHandlerImpl handler = new DefaultCallbackHandlerImpl(rrf, cb);
			
				int requestId = cbi.registerCallbackRequest(request, handler);
			
				long normalTimeout = request.getTimeoutDelta() * 10000;
				CBDescIn descIn = new CBDescIn(normalTimeout, normalTimeout, requestId);
			
				// generate parameters
				Object[] givenParameters = request.getParameters();
				Object[] parameters = null;
				if (givenParameters != null && givenParameters.length > 0)
				{				
					// copy parameters
					parameters = new Object[givenParameters.length+2];
					System.arraycopy(givenParameters, 0, parameters, 0, givenParameters.length);
					
					// set CB params (as last)
					parameters[givenParameters.length] = cbi.getCallback();
					parameters[givenParameters.length+1] = descIn;
				}
				else
					parameters = new Object[] { cbi.getCallback(), descIn };
				
				proxy.invoke(query, parameters);
				
				// callback handler will generate the response
			}
				
		} catch (Exception re)
		{
			CompletionException ce = new CompletionException(this, "Failed to process '" + query + "' on '" + path + "'.", request, re);
			ce.caughtIn(this, "processInternal");
			ce.putValue("request", request);

			Response response = rrf.createResponse(responseType, request, NO_ERROR_TYPE, NO_ERROR_CODE, System.currentTimeMillis());
			response.setError(ce);
			request.addResponse(response);
			getRequestCallback().requestEnds(request);
			return;
		}
		
	}


	/**
	 * Checks the protocol prefix in URI syntax., taking into account the value
	 * 
	 * @return 	true if the protocol prefix of the given plug is supported, otherwise false
	 */
	protected boolean checkURI(final URI uri)
	{
		return uri.getScheme().equals(schemeString);
	}

	/**
	 * Sets the debug attribute for this instance. If <code>true</code>,
	 * all received requests will be sent to the message log.
	 * 
	 * @param debug value of the debug flag
	 */
	public void setDebug(boolean value)
	{
		debug = value;
	}

	/**
	 * Returns the value of debug flag.
	 * 
	 * @return <code>true</code>
	 * @see	#setDebug
	 */
	public boolean isDebug()
	{
		return debug;
	}
	
	/**
	 * Maps Abeans to ACS characteristic name 
	 * @param characteristic Abeans characteristic name.
	 * @return ACS characteristic name, <code>null</code> if characteristic is unknown
	 */
	public static String mapCharacteristicName(String characteristic)
	{

		// do the mapping
		if (characteristic.equals(NarrowConstants.NAME_UNITS))
			return Characteristics.NAME_UNITS;
		else if (characteristic.equals(NarrowConstants.NAME_DESCRIPTION))
			return Characteristics.NAME_DESCRIPTION;
		else if (characteristic.equals(NarrowConstants.NAME_MINIMUM))
			return Characteristics.NAME_MINIMUM;
		else if (characteristic.equals(NarrowConstants.NAME_MAXIMUM))
			return Characteristics.NAME_MAXIMUM;
		else if (characteristic.equals(NarrowConstants.NAME_FORMAT))
			return Characteristics.NAME_FORMAT;

		else if (characteristic.equals(AbstractProperty.C_DISPLAY_NAME))
			return Characteristics.NAME_DISPLAY_NAME;

		else if (characteristic.equals(NumericProperty.C_RESOLUTION))
			return Characteristics.NAME_RESOLUTION;
		else if (characteristic.equals(NumericProperty.C_GRAPH_MAX))
			return Characteristics.NAME_GRAPH_MAX;
		else if (characteristic.equals(NumericProperty.C_GRAPH_MIN))
			return Characteristics.NAME_GRAPH_MIN;

		else if (characteristic.equals(Characteristics.NAME_DEFAULT_VALUE))
			return Characteristics.NAME_DEFAULT_VALUE;
		else if (characteristic.equals(Characteristics.NAME_DEFAULT_TIMER_TRIGGER))
			return Characteristics.NAME_DEFAULT_TIMER_TRIGGER;
		else if (characteristic.equals(Characteristics.NAME_MINIMAL_TIMER_TRIGGER))
			return Characteristics.NAME_MINIMAL_TIMER_TRIGGER;
		else if (characteristic.equals(Characteristics.NAME_MINIMAL_DELTA_TRIGGER))
			return Characteristics.NAME_MINIMAL_DELTA_TRIGGER;

		else if (characteristic.equals(PatternAbstractProperty.C_BIT_DESCRIPTIONS))
			return Characteristics.NAME_BIT_DESCRIPTION;
		else if (characteristic.equals(PatternAbstractProperty.C_CONDITION_WHEN_SET))
			return Characteristics.NAME_WHEN_SET;
		else if (characteristic.equals(PatternAbstractProperty.C_CONDITION_WHEN_CLEARED))
			return Characteristics.NAME_WHEN_CLEARED;
		else if (characteristic.equals(PatternAbstractProperty.C_BIT_MASK))
			return Characteristics.NAME_RESOLUTION;

		// TODO defined constants would not harm
		else if (characteristic.equals("minStep"))
			return Characteristics.NAME_MIN_STEP;
		else if (characteristic.equals("alarmHighOn"))
			return Characteristics.NAME_ALARM_HIGH_ON;
		else if (characteristic.equals("alarmHighOff"))
			return Characteristics.NAME_ALARM_HIGH_OFF;
		else if (characteristic.equals("alarmLowOn"))
			return Characteristics.NAME_ALARM_LOW_ON;
		else if (characteristic.equals("alarmLowOff"))
			return Characteristics.NAME_ALARM_LOW_OFF;

		else if (characteristic.equals(NumericProperty.C_SCALE_TYPE))
			return NumericProperty.C_SCALE_TYPE;

		/*
		// TODO support these characteristics (currenly they are not used by CosyBeans)

		public static final String NAME_SEQUENCE_LENGTH = "sequenceLength";
	
		public static final String CHARACTERISTICS_QUERY = "characteristics";
	
		public static String C_POSITION= "position";
		
		*/

		return null;
	}
	
	
	
	/**
	 * Check completion for error condition, if found error condition
	 * with full stack trace is set.
	 * 
	 *  
	 * @param verifier	object issuing this verification.
	 * @param request	request generating the completion.
	 * @param completion	completion ADT to be checked.
	 * @//throws CompletionException	thrown if error condition is found. 
	 */
	public static void handleCompletion(Identifiable verifier, Request request, Completion completion, Response response) //throws CompletionException
	{
		response.setTime(TimeConverter.toJava(completion.timeStamp));

		response.setType(completion.type);
		response.setCode(completion.code);

		// stack trace from previousError
		CompletionException stackTraceException = null;
		ErrorTrace[] errorStack = completion.previousError; 
		while (errorStack != null && errorStack.length != 0)
		{
			ErrorTrace errorTrace = errorStack[0];

			// make you own exception, which allows setting host, lineno, process, etc.
			RemoteCompletionException rce = new RemoteCompletionException(verifier, "<no description at this time>", request, stackTraceException);
			rce.setHost(errorTrace.host);
			rce.setTimestamp(TimeConverter.toJava(errorTrace.timeStamp));
			rce.caughtIn(errorTrace.routine, errorTrace.lineNum);
			
			// put other values
			rce.putValue("severity", errorTrace.severity);
			rce.putValue("process", errorTrace.process);
			rce.putValue("thread", errorTrace.thread);
			rce.putValue("file", errorTrace.file);
			rce.putValue("errorType", new Integer(errorTrace.errorType));
			rce.putValue("errorCode", new Integer(errorTrace.errorCode));
			
			// add data
			for (int i = 0; i < errorTrace.data.length; i++)
				rce.putValue(errorTrace.data[i].name, errorTrace.data[i].value);
			
			stackTraceException = rce;
			errorStack = errorTrace.previousError;
		}

		// set error condition, if any
		response.setError(stackTraceException);
		
	}

	/**
	 * Extract Java primitive from <code>org.omg.CORBA.Any</code>.
	 * @param value	CORBA Any, non-<code>null</code>
	 * @return	extracted Java primitive from <code>value</code> object
	 */
	private static Object extractAny(Any value)
	{
		// TODO arrays, sequences
		
		if (value == null)
			return null;

		switch(value.type().kind().value())
		{
			case TCKind._tk_any:
				return extractAny(value.extract_any());
			case TCKind._tk_void:
			case TCKind._tk_null:
				return null;
			/*
			case TCKind._tk_objref:
				return value.extract_Object();
			case TCKind._tk_struct:
			case TCKind._tk_except:
			case TCKind._tk_enum:
			case TCKind._tk_alias:
				return extractTypedef(value);
			*/

			case TCKind._tk_double:
				return new Double(value.extract_double());
			case TCKind._tk_float:
				return new Float(value.extract_float());
			case TCKind._tk_octet:
				return new Byte(value.extract_octet());
			case TCKind._tk_longlong:
				return new Long(value.extract_longlong());
			case TCKind._tk_ulonglong:
				return new Long(value.extract_ulonglong());
			case TCKind._tk_long:
				return new Integer(value.extract_long());
			case TCKind._tk_ulong:
				return new Integer(value.extract_ulong());
			case TCKind._tk_short:
				return new Short(value.extract_short());
			case TCKind._tk_ushort:
				return new Short(value.extract_ushort());
			case TCKind._tk_string:
				return value.extract_string();
			case TCKind._tk_char:
				return new Character(value.extract_char());
			case TCKind._tk_boolean:
				return new Boolean(value.extract_boolean());
			default:
				return null;
		}
	}

}



