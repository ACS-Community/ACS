/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.baci.util.async;

import alma.ACSErr.abeans.Completion;
import alma.ACS.abeans.Monitor;
import abeans.core.Identifier;
import abeans.core.IdentifierSupport;
import abeans.datatypes.MonitorProxy;
import abeans.engine.Request;
import abeans.engine.RequestCallback;
import abeans.engine.RequestException;
import abeans.engine.RequestResponseFactory;
import abeans.engine.ResponseType;
import abeans.models.acs.baci.util.AbeansTypeConverter;
import abeans.models.acs.baci.util.LongHolder;
import abeans.pluggable.RemoteException;

/**
 * Monitor proxy implementation
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public class MonitorProxyImpl extends PropertyValueCallbackHandlerImpl implements MonitorProxy
{

	/**
	 * Remote monitor object.
	 */
	protected Monitor delegate;

	/**
	 * Destruction status.
	 */
	protected boolean destroying = false;

	/**
	 * Default timer trigger.
	 */
	protected long defaultTimerTrigger;

	/**
	 * Callback request ID.
	 */
	protected int requestId;

	/**
	 * Callback request manager.
	 */
	protected CallbackRequestManager requestManager;

	/**
	 * Callback request.
	 */
	protected Request request;

	/**
	 * Constructor. 
	 * @param rrf request response factory.
	 * @param rcb request callback.
	 * @param converter instance converting responses to Abeans type.
	 * @param delegate remote monitor implementation to delegate requests.
	 * @param defaultTimerTrigger default timer trigger value.
	 */
	public MonitorProxyImpl(RequestResponseFactory rrf, RequestCallback rcb, AbeansTypeConverter converter, ResponseType responseType, long defaultTimerTrigger)
	{
		super(rrf, rcb, converter, responseType);
		this.defaultTimerTrigger = defaultTimerTrigger;
	}

	/**
	 * @see abeans.models.acs.baci.util.async.CallbackHandler#registered(int, abeans.engine.Request, abeans.models.acs.baci.util.async.CallbackRequestManager)
	 */
	public void registered(int requestId, Request request, CallbackRequestManager requestManager)
	{
		this.requestId = requestId;
		this.request = request;
		this.requestManager = requestManager;
	}

	/**
	 * @see abeans.models.acs.baci.util.async.CallbackHandler#done(int, java.lang.Object, alma.ACSErr.abeans.Completion, abeans.engine.Request, abeans.models.acs.baci.util.async.CallbackRequestManager)
	 */
	public void done(int requestId, Object value, Completion completion,
						Request request, CallbackRequestManager requestManager)
	{

		// do not destroy monitor, recovery can bring it back to life...
		// requestManager.deregisterCallbackRequest(requestId);
	
		working(requestId, value, completion, request, requestManager);

		// do no destroy monitor, recovery can bring it back to life...
		// rcb.requestEnds(request);

		/*
		// remote is destroying itself...
		if (!destroying)
		{
			remoteDestroy = true;
			
			// TODO parent MonitorImpl.destory() has to be called, this will also call destroy() on this proxy
		}
		*/
	}

	/**
	 * @see abeans.core.Identifiable#getIdentifier()
	 */
	public Identifier getIdentifier() {
		if (id == null)
			id = new IdentifierSupport("Monitor Proxy Implementation", getClass().getName(), Identifier.PLUG);
		return id;
	}


	/**
	 * @see abeans.datatypes.MonitorProxy#destroy()
	 */
	public synchronized void destroy() throws RemoteException
	{
		if (destroying)
			return;
			
		destroying = true;
		
		try
		{
			if (delegate != null)
				delegate.destroy();
		}
		catch (Throwable re)
		{
			RemoteException r = new RemoteException(this, "Failed call remote 'destroy()' method.", re);
			r.caughtIn(this, "destroy");
			throw r;
		}
		finally
		{
			// TODO what about waiting for 'done'...
			requestManager.deregisterCallbackRequest(requestId);
			rcb.requestEnds(request);
		}
		

	}

	/**
	 * @see abeans.datatypes.MonitorProxy#getDefaultTimerTrigger()
	 */
	public long getDefaultTimerTrigger() throws RemoteException
	{
		return defaultTimerTrigger;
	}

	/**
	 * @see abeans.datatypes.MonitorProxy#getTimerTrigger()
	 */
	public long getTimerTrigger() throws RemoteException
	{
		LongHolder longHolder = new LongHolder();
		try
		{
			delegate.get_timer_trigger(longHolder);
		}
		catch (RequestException re)
		{
			RemoteException r = new RemoteException(this, "Failed call remote 'get_timer_trigger(out long)' method.", re);
			r.caughtIn(this, "getDefaultTimerTrigger");
			throw r;
		}
		
		// 100-th ns -> ms
		return longHolder.value/10000;
	}

	/**
	 * @see abeans.datatypes.MonitorProxy#resume()
	 */
	public void resume() throws RemoteException
	{
		try
		{
			delegate.resume();
		}
		catch (RequestException re)
		{
			RemoteException r = new RemoteException(this, "Failed call remote 'resume()' method.", re);
			r.caughtIn(this, "resume");
			throw r;
		}
	}

	/**
	 * @see abeans.datatypes.MonitorProxy#setTimerTrigger(long)
	 */
	public void setTimerTrigger(long timeInMS) throws RemoteException
	{
		try
		{
			// ms -> 100-th ns
			delegate.set_timer_trigger(timeInMS*10000);
		}
		catch (RequestException re)
		{
			RemoteException r = new RemoteException(this, "Failed call remote 'set_timer_trigger(long)' method.", re);
			r.caughtIn(this, "setTimerTrigger");
			throw r;
		}
	}

	/**
	 * @see abeans.datatypes.MonitorProxy#suspend()
	 */
	public void suspend() throws RemoteException
	{
		try
		{
			delegate.suspend();
		}
		catch (RequestException re)
		{
			RemoteException r = new RemoteException(this, "Failed call remote 'suspend()' method.", re);
			r.caughtIn(this, "suspend");
			throw r;
		}
	}

	/**
	 * Set delegate.
	 * @param monitor delegate.
	 */
	public void setUp(Monitor monitor)
	{
		delegate = monitor;
	}

}
