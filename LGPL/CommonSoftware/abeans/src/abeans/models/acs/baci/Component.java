/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.baci;

import java.beans.Beans;

import javax.swing.event.EventListenerList;

import com.cosylab.datatypes.DataExchangeException;
import com.cosylab.datatypes.ResponseListener;
import com.cosylab.lifecycle.LifecycleAdapter;
import com.cosylab.lifecycle.LifecycleEvent;
import com.cosylab.util.CommonException;

import abeans.core.AssertionFailed;
import abeans.core.InitializationException;
import abeans.core.Node;
import abeans.engine.RequestException;
import abeans.framework.ApplicationContext;
import abeans.models.Abean;
import abeans.models.Family;
import abeans.pluggable.PlugException;
import abeans.pluggable.Proxy;
import abeans.pluggable.RemoteException;
import abeans.pluggable.RemoteInfo;
import abeans.pluggable.acs.ACSAbeansEngine;
import abeans.pluggable.acs.maci.NarrowCORBAProxy;
import alma.ACS.abeans.ComponentStates;

/**
 * Base class representing an component.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public class Component extends Abean implements Invokeable, ProxyContainer {

	/**
	 * No name string.
	 */
	private static final String NO_NAME = "<unnamed>";

	/**
	 * Name of the component.
	 */
	protected String name = NO_NAME;

	/**
	 * Event listeners.
	 */
	protected EventListenerList listeners = new EventListenerList();

	/**
	 * Lifecycle listener. 
	 */
	private ChildLifecycleListener lifecycleListener = new ChildLifecycleListener();
	
	/**
	 * Private implementation of <code>ChildLifecycleListener</code>.
	 */
	private class ChildLifecycleListener extends LifecycleAdapter
	{
		/**
		 * @see com.cosylab.lifecycle.LifecycleListener#destroying(LifecycleEvent)
		 */
		public void destroying(LifecycleEvent event) throws CommonException {
			if (event.getSource() instanceof Node) removeChild((Node)event.getSource());
		}

		/**
		 * @see com.cosylab.lifecycle.LifecycleListener#initialized(LifecycleEvent)
		 */
		public void initialized(LifecycleEvent event) throws CommonException {
			if (event.getSource() instanceof Node) addChild((Node)event.getSource());
		}

	}

	/**
	 * Default constructor.
	 */
	public Component() {
		super();
		if (Beans.isDesignTime())
			name = getClass().getName() + " bean";
	}

	/**
	 * @param parent
	 * @param info
	 * @throws InitializationException
	 */
	public Component(Family parent, RemoteInfo info) throws InitializationException {
		super(parent, info);
		internalConstructorConnect();
	}

	/**
	 * @see abeans.models.Linkable#initialize(abeans.pluggable.Proxy)
	 */
	public synchronized void initialize(Proxy proxy)
		throws RemoteException, AssertionFailed {

		// take ownership over NarrowCORBAProxy
		if (proxy instanceof NarrowCORBAProxy)
			((NarrowCORBAProxy)proxy).setOwner(this);

		if (proxy != null && getRemoteInfo() != null)
			name = getRemoteInfo().toURI().getPath().substring(1);
		else
			name = NO_NAME;

		super.initialize(proxy);
	}

	/**
	 * @see com.cosylab.datatypes.AsynchronousAccess#addResponseListener(com.cosylab.datatypes.ResponseListener)
	 */
	public void addResponseListener(ResponseListener l) {
		if (l == null) return;
		listeners.add(ResponseListener.class, l);
	}

	/**
	 * @see com.cosylab.datatypes.AsynchronousAccess#removeResponseListener(com.cosylab.datatypes.ResponseListener)
	 */
	public void removeResponseListener(ResponseListener l) {
		if (l == null) return;
		listeners.remove(ResponseListener.class, l);
	}

	/**
	 * @see com.cosylab.datatypes.AsynchronousAccess#getResponseListeners()
	 */
	public ResponseListener[] getResponseListeners() {
		return (ResponseListener[])listeners.getListeners(ResponseListener.class);
	}

	/**
	 * Get name of the component.
	 * @return name of the component.
	 * @throws DataExchangeException
	 */
	public String getName() throws DataExchangeException {
		return name;
		//return CharacteristicContextUtilities.getStringCharacteristic(getRemoteInfo(), "name", remote, getDefaultTimeout(), interceptor, this);
		//return (String)InvokeUtilities.invokeSync(getRemoteInfo(), "_get_name", remote, getDefaultTimeout(), this, this, null);
	}

	/**
	 * Get component state.
	 * @return component state.
	 * @throws RequestException
	 */
	public ComponentStates getComponentState() throws RequestException {
		return (ComponentStates)InvokeUtilities.invokeSync(getRemoteInfo(), "_get_componentState", remote, getDefaultTimeout(), this, this, null);
	}

	/**
	 * @see abeans.models.acs.baci.ProxyContainer#getProxy()
	 */
	public Proxy getProxy() {
		return remote;
	}

	/**
	 * @see abeans.models.acs.baci.ProxyContainer#setProxy(abeans.pluggable.Proxy)
	 */
	public void setProxy(Proxy proxy) {
		try
		{
			initialize(proxy);
		}
		catch (Exception ex)
		{
			// noop
			ex.printStackTrace();
			// TODO handle exception
		}
	}

	/**
	 * Get component lifecycle listener.
	 * @return component lifecycle listener.
	 */
	public ChildLifecycleListener getLifecycleListener() {
		return lifecycleListener;
	}


	/************************************************************************/
	/******************** [ Visual composition support ] ********************/
	/************************************************************************/
	
	/**
	 * Application context (needed to obtain bean parent family and creating <code>remoteInfo</code>).
	 */
	protected ApplicationContext applicationContext = ACSAbeansEngine.getDefaultApplicationContext();
	
	/**
	 * Bean remote name (translated later by <code>applicationContext</code> to <code>remoteInfo</code>).
	 */
	protected String remoteName;

	/**
	 * Returns application context.
	 * @return application context.
	 */
	public ApplicationContext getApplicationContext() {
		return applicationContext;
	}

	/**
	 * Sets application context.
	 * @param context applicationContext
	 */
	public void setApplicationContext(ApplicationContext context) throws PlugException, InitializationException {
		applicationContext = context;
		checkConnectionParameters();
	}

	/**
	 * Returns bean remote name.
	 * @return bean remote name.
	 */
	public String getRemoteName() {
		return remoteName;
	}

	/**
	 * Sets bean remote name.
	 * @param string bean remote name.
	 */
	public void setRemoteName(String string) throws PlugException, InitializationException {
		remoteName = string;
		checkConnectionParameters();
	}
		
	/**
	 * Checks all required connection parameters and connects if all conditions are satisfied.
	 */
	protected void checkConnectionParameters() throws PlugException, InitializationException
	{		
		if (remoteName != null && applicationContext != null)
		{
			// disconnect if necessary
			if (isLinked() && !Beans.isDesignTime())
				disconnect();
			
			super.setParent(applicationContext.getDefaultFamily());
			super.setRemoteInfo(applicationContext.createRemoteInfo(remoteName));
			
			// connect
			if (!Beans.isDesignTime())
				internalConstructorConnect();
		}
	}

}
