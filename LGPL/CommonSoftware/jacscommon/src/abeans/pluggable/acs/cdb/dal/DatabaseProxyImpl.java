/*
 * @@COPYRIGHT@@
 */

package abeans.pluggable.acs.cdb.dal;

import java.net.URI;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.logging.Level;

import com.cosylab.CDB.DAOOperations;
import com.cosylab.datatypes.DataExchangeException;

import abeans.core.Identifiable;
import abeans.core.Identifier;
import abeans.core.IdentifierSupport;
import abeans.core.defaults.MessageLogEntry;
import abeans.engine.CompletionException;
import abeans.engine.GroupRequest;
import abeans.engine.Request;
import abeans.engine.RequestCallback;
import abeans.engine.RequestException;
import abeans.engine.RequestResponseFactory;
import abeans.engine.RequestType;
import abeans.engine.Response;
import abeans.engine.ResponseType;
import abeans.engine.SimpleRequest;
import abeans.pluggable.DatabaseProxy;
import abeans.pluggable.NarrowConstants;
import abeans.pluggable.RemoteException;
import abeans.pluggable.simulator.RequestTask;

/**
 * The database proxy implementation of the CDBDAL plug.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class DatabaseProxyImpl implements DatabaseProxy, Identifiable
{
	private Identifier id = null;
	private boolean debug = false;
	private String schemeString = null;

	// callbacks
	private RequestResponseFactory rrf = null;
	private RequestCallback cb = null;

	// TODO no monitor support
	// monitor support
	private ArrayList timerTasks = new ArrayList(100);
	//private java.util.Timer t = new Timer();

	/**
	 * Completion type that indicates absence of errors.
	 */
	public static final int NO_ERROR_TYPE = 0;
	
	/**
	 * Completion code that indicates absence of errors.
	 */
	public static final int NO_ERROR_CODE = 0;

	/**
	 * Creates a new instance of the database implementation. This is
	 * called automatically by the plug which passes itself as a parameter.
	 * 
	 * @param plug the plug instantiating this instance, non-<code>null</code>
	 */
	public DatabaseProxyImpl(CDBDALPlug plug)  throws RemoteException
	{
		assert (plug != null);
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
	 * request task is cencelled.
	 * 
	 * @param	req				the request to stop, non-<code>null</code>
	 * @throws RemoteException if the stop operation fails
	 * @see 	abeans.pluggable.DatabaseProxy#stop(Request)
	 */
	public void stop(Request req) throws RemoteException
	{
		assert (req != null);
		
		RequestTask rt = getTaskForRequest(req);
		
		if (isDebug())
			new MessageLogEntry(this, "stop", "Stopping request task '" + rt + "' for request: " + req, Level.FINE).dispatch();
			
		if (rt != null) rt.cancel();
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
	 * First cancels all request tasks being scheduled, then delegates to
	 * super destroy.
	 */
	public void destroy()
	{
		if (isDebug())
			new MessageLogEntry(this, "destroy", "Cancelling " + timerTasks.size() + " remaining timer tasks.", Level.FINE).dispatch();
		
		synchronized (timerTasks)
		{
			for (int i = 0; i < timerTasks.size(); i++)
			{
				RequestTask rt = (RequestTask)timerTasks.get(i);
				rt.cancel();
			}
		}
		
		//t.cancel();
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
			id = new IdentifierSupport("CDB DAL Database Proxy", "CDBDALDBProxy", Identifier.PLUG);
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

		if (query.equals(NarrowConstants.GET_QUERY) ||
			query.equals(abeans.datatypes.Constants.Q_GET_ASYNC))
		{
				processGetQuery(request, path);
		}
		else if (query.equals(NarrowConstants.SET_QUERY) ||
	 			  query.equals(abeans.datatypes.Constants.Q_SET_ASYNC))
	 	{
				throw new RequestException(this, "Currently do not know how to handle set queries.", request);
		}

		else if (query.equals(NarrowConstants.MONITOR_QUERY))
		{
				throw new RequestException(this, "Currently do not know how to handle monitor queries.", request);
		}
		else
		{
				throw new RequestException(this, "Unsupported query '"+query+"'.", request);
		}
		
	}

	/**
	 * Processes the get quey request. The request is first started, the context producer
	 * is checked. The request is then checked for correctness. A new response
	 * is created with the type defined by the type of property to which this query 
	 * belongs. The result is inserted into the response, which is submitted back
	 * to the database. Because the request is single, this completes the 
	 * request.
	 * 
	 * @param	request				the request to process, non-<code>null</code>
	 * @param	path				path to be read from CDB, non-<code>null</code>
	 * @throws RemoteException 	when the get operation on the property fails
	 * @throws RequestException	if the request is malformed
	 */
	private void processGetQuery(Request request, String path) throws RemoteException, RequestException
	{
		assert (request != null);
/*
 		// these two parameters are ignored
 		long timeout = request.getTimeoutDelta();
		request.isBlocking()
*/
		
		getRequestCallback().requestStarts(request);
	
		if (request.getRequestType() != null && request.getRequestType() == RequestType.REPEATED_REQUEST)
		{
			RequestException re = new RequestException(this, "'get' query allows only single requests.", request);
			request.setError(re);
			getRequestCallback().requestEnds(request);
			throw re;
		}
		else if (request.getRequestType() == null)
		{
			request.setRequestType(RequestType.SINGLE_REQUEST);
		}

		//
		// get DAO via proxy
		//
		DAOProxy proxy = (DAOProxy)request.getProxy();
		if (proxy == null)
		{
			RequestException re = new RequestException(this, "'get' query reqires non-null proxy, but null proxy given.", request);
			request.setError(re);
			getRequestCallback().requestEnds(request);
			throw re;
		}
		DAOOperations dao = proxy.getDao();
		
		
		//
		// determine if this is a characteristic (field) or query request
		//
		String field = null;
		String curl = proxy.getCurl();
		if (!curl.equalsIgnoreCase(path))
		{
			if (!path.startsWith(curl))
			{
				RequestException re = new RequestException(this, "Request has wrong CURL, it is not consistant with proxy: " + curl + " ~ " + path + ".", request);
				request.setError(re);
				getRequestCallback().requestEnds(request);
				throw re;
			}
			else
			{
				// we have got a characteristic (field)
				field = path.substring(curl.length()+1);
			}
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
			request.setError(re);
			getRequestCallback().requestEnds(request);
			throw re;
		}
		
		if (field == null)
			// this means get list of my children
			field = Constants.ATTRIBUTES;


		Response response = rrf.createResponse(responseType, request, NO_ERROR_TYPE, NO_ERROR_CODE, System.currentTimeMillis());
		try
		{
			if (responseType == ResponseType.DOUBLE)
			{
				response.setValueAsObject(new Double(dao.get_double(field)));
			}
			else if (responseType == ResponseType.LONG)
			{
				response.setValueAsObject(new Long(dao.get_long(field)));
			}
			else if (responseType == ResponseType.STRING)
			{
				response.setValueAsObject(dao.get_string(field));
			}
			else if (responseType == ResponseType.DOUBLE_SEQ)
			{
				response.setValueAsObject(dao.get_double_seq(field));
			} 
			else if (responseType == ResponseType.LONG_SEQ)
			{
				response.setValueAsObject(dao.get_long_seq(field));
			} 
			else if (responseType == ResponseType.STRING_SEQ)
			{
			    // special case
			    if (field.equals(Constants.SUBNODES))
					response.setValueAsObject(getSubNodes(proxy));
			    else
			        response.setValueAsObject(dao.get_string_seq(field));
			} 
			else if (responseType == ResponseType.OBJECT)
			{
				response.setValueAsObject(dao.get_field_data(field));
			}
			else
			{
				DataExchangeException dee = new DataExchangeException(this, "Unknown response type.");
				throw dee;
			}
			
		} catch (Exception re)
		{
			CompletionException ce = new CompletionException(this, "Failed to get the value of the '" + path + "'.", request, re);
			ce.putValue("request", request);
			ce.putValue("field", field);
			response.setError(ce);
			request.addResponse(response);
			getRequestCallback().requestEnds(request);
			return;
		}
		
		request.addResponse(response);
		getRequestCallback().requestNewResponse(response);
	}

	/**
	 * Get all subnodes of the current proxy.
	 * @param proxy		proxy whose subnodes to return.	
	 * @return	array of subnodes.
	 */
	private static String[] getSubNodes(DAOProxy proxy)
	{
	    ArrayList subnodes = new ArrayList();
	    
	    LinkedList stack = new LinkedList();
		stack.addLast(proxy.getCurl());
		while (!stack.isEmpty())
		{
		    String parentNode = stack.removeLast().toString();
		    
			String nodes = proxy.getDal().list_nodes(parentNode);
			if (nodes.length() > 0)
			{
			    StringTokenizer tokenizer = new StringTokenizer(nodes);
			    while (tokenizer.hasMoreTokens())
			    {
			        String nodeName = tokenizer.nextToken();
			        if (nodeName.endsWith(".xml"))
			            continue;
			        
			        String fullName = parentNode + "/" + nodeName;
			        stack.addLast(fullName);
			        // strip off relative path
			        subnodes.add(fullName.substring(proxy.getCurl().length()+1));
			    }
			}
		}				
	    
		String[] retVal = new String[subnodes.size()];
		subnodes.toArray(retVal);
		return retVal;
	}
	

	/**
	 * Schedules a task for multiple repetitions.
	 * 
	 * @param rt		the task to be scheduled, non-<code>null</code>
	 * @param delay	the delay until the first execution of the task
	 * 		   			in milliseconds, greater than 0
	 * @param rate 	the time interval in milliseconds between the 
	 * 		   			subsequent repetitions, greater than 0
	 */
	/*
	public void schedule(RequestTask rt, long delay, long rate)
	{
		assert (rt != null);
		assert (delay > 0);
		assert (rate > 0);
		
		synchronized(timerTasks)
		{
			timerTasks.add(rt);
		}

		t.schedule(rt, delay, rate);
	}
	*/
			
	/**
	 * Cancels the existing scheduled task, used when the repeated requests are
	 * stopped.
	 * 
	 * @param rt	the request task to cancel, non-<code>null</code>
	 */
	public void cancelScheduled(RequestTask rt)
	{
		assert (rt != null);
		
		synchronized(timerTasks)
		{
			timerTasks.remove(rt);
		}
	}
	
	/**
	 * Returns a request that caused a request task to be scheduled in the 
	 * database proxy implementation. Non-blocking repeated requests will 
	 * usually cause such tasks to be generated.
	 * 
	 * @param	req		the request for which the task is to be searched for, non-<code>null</code>
	 * @return 		the task currently in the scheduler
	 */
	RequestTask getTaskForRequest(Request req)
	{
		assert (req != null);
		
		RequestTask[] list = null;
		synchronized(timerTasks)
		{
			list = new RequestTask[timerTasks.size()];
			timerTasks.toArray(list);
		}
		for (int i = 0; i < list.length; i++)
		{
			if (list[i].getRequest() == req) 
			{
				return list[i];
			}
		}
		return null;
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
	

}
