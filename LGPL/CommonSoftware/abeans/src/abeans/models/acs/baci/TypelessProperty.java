/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.baci;

import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Map;

import javax.swing.event.EventListenerList;

import abeans.core.ArrayTreeTraversal;
import abeans.core.AssertionFailed;
import abeans.core.Node;
import abeans.core.NodeVisitor;
import abeans.core.PolicyList;
import abeans.core.QoSProvider;
import abeans.core.Root;
import abeans.core.TreeTraversal;
import abeans.datatypes.CharacteristicContextUtilities;
import abeans.datatypes.DynamicValueUtilities;
import abeans.datatypes.Monitor;
import abeans.datatypes.MonitorListener;
import abeans.datatypes.UpdateableUtilities;
import abeans.engine.Database;
import abeans.engine.RequestInterceptor;
import abeans.models.Abean;
import abeans.models.DefaultTimeoutPolicy;
import abeans.models.Lbean;
import abeans.models.acs.baci.util.AbeansTypeConverter;
import abeans.models.acs.baci.util.async.CallbackSingletonFactory;
import abeans.models.acs.baci.util.async.HistoryProducer;
import abeans.pluggable.NarrowConstants;
import abeans.pluggable.Proxy;
import abeans.pluggable.RemoteException;
import abeans.pluggable.RemoteInfo;
import abeans.pluggable.acs.maci.NarrowCORBAProxy;

import com.cosylab.datatypes.DataAccess;
import com.cosylab.datatypes.DataExchangeException;
import com.cosylab.datatypes.DynamicValueListener;
import com.cosylab.datatypes.HistoryConstraints;
import com.cosylab.datatypes.HistoryIterator;
import com.cosylab.datatypes.ResponseListener;
import com.cosylab.lifecycle.ExceptionHandler;
import com.cosylab.lifecycle.LifecycleAdapter;
import com.cosylab.lifecycle.LifecycleEvent;
import com.cosylab.lifecycle.LifecycleListener;
import com.cosylab.lifecycle.LifecyclePhase;
import com.cosylab.lifecycle.LifecycleReporter;
import com.cosylab.lifecycle.LifecycleReporterSupport;
import com.cosylab.util.CommonException;
import com.cosylab.util.NameValueList;

/**
 * Base class of all properties. It maps to baci::TypelessProperty interface.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */

public abstract class TypelessProperty extends Lbean
	implements abeans.datatypes.TypelessProperty, LifecycleReporter, QoSProvider, NarrowConstants,
			   Invokeable, ProxyContainer, AbeansTypeConverter, CallbackSingletonFactory, HistoryProducer
{

	/**
	 * Name for unknown properties, i.e. properties returned by the server. 
	 */
	private static final String NA_NAME = "n/a";
	
	/**
	 * Name for unknown properties, i.e. properties returned by the server. 
	 */
	private static Component offshootParent = new Component();

	/**
	 * Property remote info.
	 */
	protected RemoteInfo remoteInfo = null;

	/**
	 * Destroyed state.
	 */
	private boolean isDestroyed = false;

	/**
	 * Default timeout time in ms, 0 means not timeout.
	 */
	private long defaultTimeout = 0;

	/**
	 * Request interceptor.
	 */
	protected RequestInterceptor interceptor = null;

	/**
	 * Property name.
	 */
	protected String propertyName = null;

	/**
	 * Parent <code>Component</code>.
	 */
	protected Component parentComponent = null;

	/**
	 * Event listeners.
	 */
	protected EventListenerList listeners = new EventListenerList();
	
	/**
	 * Buffer for dynamic value listeners. 
	 */
	private ArrayList listenersBuffer = new ArrayList();
	
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
	 * Lifecycle execption handler.
	 */
	public class LCExceptionHandler implements ExceptionHandler
	{
		/**
		 * @see com.cosylab.lifecycle.ExceptionHandler#exceptionCaught(Object, CommonException)
		 */
		public boolean exceptionCaught(Object source, CommonException exception)
		{
			AssertionFailed af = new AssertionFailed(TypelessProperty.this,
					"Cannot inform the listener about the lifecycle phase change.",
					exception);
			af.putValue("source", source);
			af.caughtIn(this, "exceptionCaught");
			return true;
		}
	}
	
	/**
	 * Lifecycle reporter support implementation;
	 */
	protected LifecycleReporterSupport lifecycleSupport;


	/**
	 * Default constructor of TypelessProperty. 
	 */
	public TypelessProperty()
	{
		// noop
		super(offshootParent, NA_NAME, NA_NAME);
	}

	/**
	 * Constructor of TypelessProperty. 
	 * Constructor is given two arguments: the parent of the property and property name.  
	 * 
	 * @param parentComponent	parent component of this property.
	 * @param propertyName		name of the property.
	 */
	public TypelessProperty(Component parentComponent, String propertyName)
	{
		
		super(parentComponent, propertyName, propertyName);

		this.parentComponent = parentComponent;
		this.propertyName = propertyName;

		this.interceptor = parentComponent;
		
		if (parentComponent.getRemoteInfo() != null)
			this.remoteInfo = parentComponent.getRemoteInfo().createHierarchy(propertyName);

		// create recycable lifecycle reporter
		lifecycleSupport = new LifecycleReporterSupport(this, true, LifecyclePhase.BEFORE_INITIALIZATION);

		// register parent component as lifecycle listener
		addLifecycleListener(parentComponent.getLifecycleListener());
	}

	/**
	 * destroy method comment.
	 */
	public synchronized void destroy() {
		if (isDestroyed()) return;

		// destroy monitors
		Node[] ch = getChildren();
		for (int i = 0; i < ch.length; i++)
			ch[i].destroy();

		super.destroy();
		isDestroyed = true;
		
	}

	/**
	 * @see abeans.models.Linkable#initialize(abeans.pluggable.Proxy)
	 */
	public void initialize(Proxy proxy) throws RemoteException {

		// revive the property
		if (proxy != null && parentComponent != null && isDestroyed())
		{
			// update remote info
			if (parentComponent.getRemoteInfo() != null)
				this.remoteInfo = parentComponent.getRemoteInfo().createHierarchy(propertyName);
			setParent(parentComponent);
			isDestroyed = false;
		}

		// initialize remote info
		if (remoteInfo == null &&  parentComponent != null && parentComponent.getRemoteInfo() != null)
			this.remoteInfo = parentComponent.getRemoteInfo().createHierarchy(propertyName);

		if (lifecycleSupport != null)
			try
			{
				if (proxy != null)
					lifecycleSupport.fireIncrementalInitializing(null, new LCExceptionHandler());
				else
					lifecycleSupport.fireIncrementalDestroying(null, new LCExceptionHandler());
			} catch (CommonException ie)
			{
				AssertionFailed af = new AssertionFailed(this, "Exception thrown while informing lifecycle listeners about (de)initialization start.", ie);
				af.caughtIn(this, "initialize");
				af.putValue("proxy", proxy);
				// do not throw
			}
		
		// take ownership over NarrowCORBAProxy
		if (proxy instanceof NarrowCORBAProxy)
			((NarrowCORBAProxy)proxy).setOwner(this);
	
		super.initialize(proxy);
		newProxySet();

		if (lifecycleSupport != null)
			try
			{
				if (proxy != null)
					lifecycleSupport.fireIncrementalInitialized(null, new LCExceptionHandler());
				else
					lifecycleSupport.fireIncrementalDestroyed(null, new LCExceptionHandler());
			} catch (CommonException ie)
			{
				AssertionFailed af = new AssertionFailed(this, "Exception thrown while informing lifecycle listeners about (de)initialization end.", ie);
				af.caughtIn(this, "initialize");
				af.putValue("proxy", proxy);
				// do not throw
			}

	}

	/**
	 * Called everytime new proxy was set.
	 */
	protected void newProxySet() {
	
		Object[] l;
		synchronized(listenersBuffer) {
			l = listenersBuffer.toArray();
		}
	
		if (l==null) return;
	
		// set buffered dynamic listeners
		for (int i = 0; i < l.length; i++)
			DynamicValueUtilities.addDynamicValueListener(this, remote, getRemoteInfo(), interceptor, (DynamicValueListener)l[i], getDefaultDataAccess(), lifecycleListener);
	
	}

	/**
	 * @see abeans.datatypes.TypelessProperty#createMonitor(com.cosylab.datatypes.DataAccess, abeans.datatypes.MonitorListener)
	 */
	public Monitor createMonitor(DataAccess access, MonitorListener l)
		throws DataExchangeException {
		if (access == null) access = getDefaultDataAccess();
		assert (access != null);
		return DynamicValueUtilities.createMonitor(this, remote, getRemoteInfo(), interceptor, l, access, lifecycleListener);	
	}

	/**
	 * @see abeans.datatypes.TypelessProperty#getDatabase()
	 */
	public Database getDatabase() {
		if (getParent() instanceof Abean)
			return ((Abean)getParent()).getDatabase();
		else
			return null;
	}

	/**
	 * @see com.cosylab.datatypes.AbstractProperty#getUniqueName()
	 */
	public String getUniqueName() {
		if (getRemoteInfo() == null) return null;
		return getRemoteInfo().toURI().toString();
	}

	/**
	 * @see com.cosylab.datatypes.Updateable#getLatestRequest()
	 */
	public Object getLatestRequest() {
		return UpdateableUtilities.getLatestRequest(getDatabase(), this);
	}

	/**
	 * @see com.cosylab.datatypes.Updateable#getLatestResponse()
	 */
	public Object getLatestResponse() {
		return UpdateableUtilities.getLatestResponse(getDatabase(), this);
	}

	/**
	 * @see com.cosylab.datatypes.Updateable#getLatestSuccess()
	 */
	public boolean getLatestSuccess() {
		return UpdateableUtilities.getLatestSuccess(getDatabase(), this);
	}

	/**
	 * @see abeans.core.Node#getTreeTraversal(abeans.core.NodeVisitor)
	 */
	public TreeTraversal getTreeTraversal(NodeVisitor visitor) {
		return new ArrayTreeTraversal(this, visitor);
	}

	/**
	 * @see com.cosylab.datatypes.HistoryAccess#getHistory(com.cosylab.datatypes.HistoryConstraints)
	 */
	public HistoryIterator getHistory(HistoryConstraints hc)
		throws DataExchangeException {
		return DynamicValueUtilities.getHistory(this, remote, getRemoteInfo(), interceptor, hc, lifecycleListener);
	}

	/**
	 * @see com.cosylab.datatypes.DataAccess#addDynamicValueListener(com.cosylab.datatypes.DynamicValueListener)
	 */
	public void addDynamicValueListener(DynamicValueListener l) {
		synchronized (listenersBuffer) {
			listenersBuffer.add(l);
			if (remote != null) 
				DynamicValueUtilities.addDynamicValueListener(this, remote, getRemoteInfo(), interceptor, l, getDefaultDataAccess(), lifecycleListener);
		}
	}

	/**
	 * @see com.cosylab.datatypes.DataAccess#removeDynamicValueListener(com.cosylab.datatypes.DynamicValueListener)
	 */
	public void removeDynamicValueListener(DynamicValueListener l) {
		synchronized (listenersBuffer) {
			listenersBuffer.remove(l);
			if (remote != null)
				DynamicValueUtilities.removeDynamicValueListener(this, l);
			
		}
	}

	/**
	 * @see com.cosylab.datatypes.DataAccess#getDynamicValueListeners()
	 */
	public DynamicValueListener[] getDynamicValueListeners() {
		return DynamicValueUtilities.getDynamicValueListeners(this);
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
	 * @see com.cosylab.datatypes.CharacteristicContext#getCharacteristicNames()
	 */
	public String[] getCharacteristicNames() throws DataExchangeException {
		return (String[])InvokeUtilities.getCharacteristic(getRemoteInfo(), CHARACTERISTICS_QUERY, remote, getDefaultTimeout(), interceptor, this);
	}

	/**
	 * @see com.cosylab.datatypes.CharacteristicContext#getCharacteristics(java.lang.String[])
	 */
	public Map getCharacteristics(String[] names) throws DataExchangeException {
		Map map = new NameValueList();

		if (names != null)
		{
			for (int i = 0; i < names.length; i++)
			{
				try {
					Object value = getCharacteristic(names[i]);
					map.put(names[i], value);
				} catch (DataExchangeException e) {	}
			}
		}

		return map;
	}

	/**
	 * @see com.cosylab.datatypes.CharacteristicContext#getCharacteristic(java.lang.String)
	 */
	public Object getCharacteristic(String name) throws DataExchangeException {
		return CharacteristicContextUtilities.getCharacteristic(getRemoteInfo(), name, remote, getDefaultTimeout(), interceptor, this);
	}

	/**
	 * @see com.cosylab.datatypes.CharacteristicContext#addPropertyChangeListener(java.beans.PropertyChangeListener)
	 */
	public void addPropertyChangeListener(PropertyChangeListener l) {
		// noop
	}

	/**
	 * @see com.cosylab.datatypes.CharacteristicContext#removePropertyChangeListener(java.beans.PropertyChangeListener)
	 */
	public void removePropertyChangeListener(PropertyChangeListener l) {
		// noop
	}

	/**
	 * @see com.cosylab.datatypes.ValueUpdateable#getLatestReceivedValueAsObject()
	 */
	public Object getLatestReceivedValueAsObject() {
		return UpdateableUtilities.getLatestReceivedValueAsObject(getDatabase(), this);
	}

	/**
	 * @see com.cosylab.datatypes.ValueUpdateable#getLatestValueChangeTimestamp()
	 */
	public long getLatestValueChangeTimestamp() {
		return UpdateableUtilities.getLatestValueChangeTimestamp(this);
	}

	/**
	 * @see com.cosylab.datatypes.ValueUpdateable#getLatestValueRequest()
	 */
	public Object getLatestValueRequest() {
		return UpdateableUtilities.getLatestValueRequest(getDatabase(), this);
	}

	/**
	 * @see com.cosylab.datatypes.ValueUpdateable#getLatestValueResponse()
	 */
	public Object getLatestValueResponse() {
		return UpdateableUtilities.getLatestValueResponse(getDatabase(), this);
	}

	/**
	 * @see com.cosylab.datatypes.ValueUpdateable#getLatestValueSuccess()
	 */
	public boolean getLatestValueSuccess() {
		return UpdateableUtilities.getLatestValueSuccess(getDatabase(), this);
	}

	/**
	 * @see com.cosylab.datatypes.ValueUpdateable#getLatestValueUpdateTimestamp()
	 */
	public long getLatestValueUpdateTimestamp() {
		return UpdateableUtilities.getLatestValueUpdateTimestamp(getDatabase(), this);
	}

	/**
	 * @see com.cosylab.datatypes.AbstractProperty#getDescription()
	 */
	public String getDescription() throws DataExchangeException {
		return CharacteristicContextUtilities.getStringCharacteristic(getRemoteInfo(), NAME_DESCRIPTION, remote, getDefaultTimeout(), interceptor, this);
	}

	/**
	 * @see com.cosylab.datatypes.NumericProperty#getFormat()
	 */
	public String getFormat() throws DataExchangeException {
		return CharacteristicContextUtilities.getStringCharacteristic(getRemoteInfo(), NAME_FORMAT, remote, getDefaultTimeout(), interceptor, this);
	}

	/**
	 * @see com.cosylab.datatypes.NumericProperty#getUnits()
	 */
	public String getUnits() throws DataExchangeException {
		return CharacteristicContextUtilities.getStringCharacteristic(getRemoteInfo(), NAME_UNITS, remote, getDefaultTimeout(), interceptor, this);
	}

	/**
	 * Returns property remote info.
	 * @return property remote info
	 */
	public RemoteInfo getRemoteInfo() {
		return remoteInfo;
	}

	/**
	 * Returns default timeout time in ms.
	 * @return default timeout time in ms.
	 */
	public long getDefaultTimeout()
	{
		return defaultTimeout;
	}
	
	/**
	 * @see abeans.core.QoSProvider#getSupportedPolicies()
	 */
	public Class[] getSupportedPolicies() {
		return new Class[] { DefaultTimeoutPolicy.class };
	}

	/**
	 * @see abeans.core.QoSProvider#installPolicies(abeans.core.PolicyList)
	 */
	public PolicyList installPolicies(PolicyList policies) {
		
		DefaultTimeoutPolicy dtp = (DefaultTimeoutPolicy)policies.getPolicy(DefaultTimeoutPolicy.NAME);

		// honored policies
		PolicyList honoredPolicies = new PolicyList();
		
		if (dtp!=null)
		{	
			defaultTimeout = dtp.getValue();
			honoredPolicies.addPolicy(dtp);
		}

		return honoredPolicies;
	}

	/**
	 * @see abeans.core.QoSProvider#refreshHierarchicalPolicies()
	 */
	public void refreshHierarchicalPolicies() {
		DefaultTimeoutPolicy dtp = (DefaultTimeoutPolicy)Root.getPolicyManager().getEffectivePolicy(this, DefaultTimeoutPolicy.NAME);	
		if (dtp != null)
			defaultTimeout = dtp.getValue();
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
	 * @see com.cosylab.lifecycle.LifecycleReporter#addLifecycleListener(LifecycleListener)
	 */
	public void addLifecycleListener(LifecycleListener l) {
		lifecycleSupport.addLifecycleListener(l);
	}

	/**
	 * @see com.cosylab.lifecycle.LifecycleReporter#getLifecyclePhase()
	 */
	public LifecyclePhase getLifecyclePhase() {
		return lifecycleSupport.getLifecyclePhase();
	}

	/**
	 * @see com.cosylab.lifecycle.LifecyclePhase#isDestroying()
	 */
	public boolean isDestroying() {
		return lifecycleSupport.getLifecyclePhase().isDestroying();
	}

	/**
	 * @see com.cosylab.lifecycle.LifecycleReporter#isRecyclable()
	 */
	public boolean isRecyclable() {
		return lifecycleSupport.isRecyclable();
	}

	/**
	 * @see com.cosylab.lifecycle.LifecycleReporter#removeLifecycleListener(LifecycleListener)
	 */
	public void removeLifecycleListener(LifecycleListener l) {
		lifecycleSupport.removeLifecycleListener(l);
	}

	/**
	 * Override <code>NodeSupport</code> method to be able to revive the property.
	 * @see abeans.core.Node#isDestroyed()
	 */
	public boolean isDestroyed() {
		return isDestroyed;
	}

}
