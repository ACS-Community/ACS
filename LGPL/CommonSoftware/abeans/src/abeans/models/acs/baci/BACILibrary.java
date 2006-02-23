/*
 * Copyright (c) 2003 by Cosylab d.o.o.
 *
 * The full license specifying the redistribution, modification, usage and other
 * rights and obligations is included with the distribution of this project in
 * the file license.html. If the license is not included you may find a copy at
 * http://www.cosylab.com/legal/abeans_license.htm or may write to Cosylab, d.o.o.
 *
 * THIS SOFTWARE IS PROVIDED AS-IS WITHOUT WARRANTY OF ANY KIND, NOT EVEN THE
 * IMPLIED WARRANTY OF MERCHANTABILITY. THE AUTHOR OF THIS SOFTWARE, ASSUMES
 * _NO_ RESPONSIBILITY FOR ANY CONSEQUENCE RESULTING FROM THE USE, MODIFICATION,
 * OR REDISTRIBUTION OF THIS SOFTWARE.
 */

package abeans.models.acs.baci;

import abeans.core.AssertionFailed;
import abeans.core.ComponentInitializationException;
import abeans.core.ComponentManager;
import abeans.core.ComponentSupport;
import abeans.core.Identifier;
import abeans.core.IllegalComponentStateException;
import abeans.core.Policy;
import abeans.core.PolicyManager;
import abeans.core.Root;

import abeans.datatypes.DynamicValueDescriptor;
import abeans.datatypes.UpdateableUtilities;

import abeans.engine.Database;
import abeans.engine.IndexingPolicy;
import abeans.engine.LatestIndexingComparator;

import abeans.framework.ApplicationContext;

import abeans.models.Abean;
import abeans.models.Connectable;
import abeans.models.DefaultFamily;
import abeans.models.DistributedDirectory;
import abeans.models.Family;
import abeans.models.Library;
import abeans.models.Linkable;
import abeans.models.ModelsLayer;

import abeans.models.meta.AbeansDirectory;
import abeans.models.meta.ConnectableRealization;
import abeans.models.meta.InstantiationException;
import abeans.models.meta.LinkableRealization;
import abeans.models.meta.ModelingElementDescriptor;
import abeans.models.meta.NamespaceDescriptor;

import abeans.pluggable.RemoteInfo;

import com.cosylab.lifecycle.LifecycleAdapter;
import com.cosylab.lifecycle.LifecycleEvent;

import com.cosylab.util.CommonException;

import java.beans.BeanInfo;

import javax.naming.NamingException;


/**
 * BACI Library implementation.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public class BACILibrary extends ComponentSupport implements Library

{
	/**
	 * Component descriptor.
	 */
	private abeans.core.ComponentDescriptor cdesc = null;
	
	/**
	 * Abeans directory. 
	 */
	private AbeansDirectory directory;
	
	/**
	 * List of supported modeling types.
	 */
	private static final Class[] modelingElementDescriptorTypes = {
		NamespaceDescriptor.class, ComponentDescriptor.class,
		DynamicValueDescriptor.class
	};

	/**
	 * Lifecycle listener implementation.
	 */
	private class LifeListener extends LifecycleAdapter
	{
		/**
		 * @see com.cosylab.lifecycle.LifecycleAdapter#initialized(LifecycleEvent)
		 */
		public void initialized(LifecycleEvent event) throws CommonException
		{
			ApplicationContext ctx = (ApplicationContext)event.getSource();

			// add latest value indexing to the database
			Database db = ctx.getDatabase();
			IndexingPolicy policy = new IndexingPolicy(LatestIndexingComparator.class);

			try {
				db.createIndex(UpdateableUtilities.LATEST_VALUE_INDEX,
				    UpdateableUtilities.LATEST_VALUE_INDEX_KEY, policy);
			} catch (Exception e) {
				AssertionFailed af = new AssertionFailed(BACILibrary.this,
					    "Could not create latest value index.", e);
				af.putValue("ctx", ctx);
				af.putValue("db", db);
				af.putValue("policy", policy);
				throw af;
			}

			try {
				db.createIndex(UpdateableUtilities.LATEST_REQUEST_INDEX,
				    UpdateableUtilities.LATEST_REQUEST_INDEX_KEY, policy);
			} catch (Exception e) {
				AssertionFailed af = new AssertionFailed(BACILibrary.this,
					    "Could not create latest request index.", e);
				af.putValue("ctx", ctx);
				af.putValue("db", db);
				af.putValue("policy", policy);
				throw af;
			}
		}
	}

	/**
	 * Default constructor.
	 */
	public BACILibrary()
	{
		super("BACILibrary", "BACILibrary", Identifier.MODEL);
		cdesc = new abeans.core.ComponentDescriptor(getClass(), Library.class, 1,
			    "Library that models a control system following BACI design.",
			    true, false, null);
	}

	/**
	 * This method is called to inform the <code>Contractor</code> that the
	 * contract has been broken. The method is called only if the initiator of
	 * the contract breaking is not this contract holder. An optional policy
	 * argument may indicate the reason why the contract has been broken (the
	 * policy object represents the mandatory policy requested that could not
	 * be met by the provider).
	 *
	 * @param source the <code>Node</code> that broke the contract
	 * @param policy the mandatory policy request that cannot be met     by the
	 *        provider; can be <code>null</code>
	 */
	public void contractBroken(abeans.core.Node source, Policy policy)
	{
	}

	/**
	 * Called by the <code>PolicyManager</code> to inform the
	 * <code>Contractor</code> that the contract has been changed. This can
	 * happen if some other <code>Node</code> has requested a new set of
	 * policies to be installed or if the <code>QoSProvider</code> is unable
	 * to meet the policies because of its internal state. The change in
	 * policy can only occur to that policies which the provider is not forced
	 * to comply with.
	 *
	 * @param newContract a new contract that has been modified because of the
	 *        new policy request
	 */
	public void contractChanged(PolicyManager.Contract newContract)
	{
	}

	/**
	 * Returns default family for the gived application context.
	 * 
	 * @param ctx application context for with default family should be returned
	 * @return abeans.models.Family
	 */
	public Family createDefaultFamily(ApplicationContext ctx)
	{
		return new DefaultFamily(ctx);
	}

	/**
	 * @param ctx abeans.framework.ApplicationContext
	 */
	public void deregisterApplication(ApplicationContext ctx)
	{
	}

	/**
	 * Returns an instance of the <code>ComponentDescriptor</code> that
	 * parametrizes this component. The component should create an immutable
	 * descriptor instance only once and always return the same instance.
	 *
	 * @return a description of this component
	 */
	public abeans.core.ComponentDescriptor getComponentDescriptor()
	{
		return cdesc;
	}

	/**
	 * Returns component name.
	 * @return component name.
	 */
	public String getName()
	{
		return "BACI";
	}

	/**
	 * Initializes the component. This includes any processing neccessary by
	 * the service when it is passed the parent (<code>manager</code>
	 * parameter) and the state. Functionally this means that the component
	 * will try to harmonize its state with that passed as the parameter and
	 * will throw an exception if the state interpretation is unsuccessfull.
	 * The component may use the <code>Root</code> to make any queries that it
	 * needs on the abeans system.
	 *
	 * @param manager the manager that will be the parent of this component and
	 *        the initiator of the install procedure. The manager will add the
	 *        component to the collection <b>after</b> initialize completes.
	 * @param state a state that should be installed by this component. If
	 *        unable to do so, the component should raise the exception. If
	 *        the component is stateless, the state must be <code>null</code>
	 *        or the component should raise an exception.
	 * @param cdesc a component descriptor identifying the component from which
	 *        the     state originates. If <code>null</code>, the
	 *        <code>state</code>     must also be <code>null</code>. This
	 *        semantics indicates that the     component should initialize to
	 *        its default state (for instance because     it is the first
	 *        component receiving state transfer from no previous component).
	 *
	 * @exception ComponentInitializationException thrown when the component
	 *            determines, by querying the <code>manager</code> and
	 *            <code>Root</code> that the  conditions for the successfull
	 *            installation are not fulfilled. The  exception should
	 *            contain an explanation of the failure.
	 * @exception IllegalComponentStateException thrown when the
	 *            <code>state</code> parameter cannot be interpreted by the
	 *            component or if it is not <code>null</code> even though the
	 *            component is stateless
	 * @throws NullPointerException DOCUMENT ME!
	 */
	public void initialize(ComponentManager manager, Object state, abeans.core.ComponentDescriptor cdesc)
		throws ComponentInitializationException, IllegalComponentStateException
	{
		if (manager == null) {
			throw new NullPointerException("manager");
		}

		if (state != null || cdesc != null) {
			throw new IllegalComponentStateException(this,
			    "State and component descriptors must be null - the library does not support state transfers.");
		}

		setParent(manager);
	}

	/**
	 * Insert the method's description here. Creation date: (7/6/01 3:57:54 PM)
	 *
	 * @param ctx abeans.framework.ApplicationContext
	 */
	public void registerApplication(ApplicationContext ctx)
	{
		// add itself to the application context lifecycle listener list
		ctx.addLifecycleListener(new LifeListener());
	}

	/**
	 * Helper method returning (and caching) abeans directory.
	 * @return abeans directory
	 */
	private AbeansDirectory getDirectory()
	{
		if (directory == null) {
			try {
				ModelsLayer ml = Root.getModels();
				DistributedDirectory dd = (DistributedDirectory)ml.getDistributedService(DistributedDirectory.class);
				directory = dd.getInitialContext();
			} catch (AssertionFailed e) {
				e.printStackTrace();
				return null;
			}
		}

		return directory;
	}

	/**
	 * @see abeans.models.meta.Reflective#getDescriptor(abeans.models.Connectable)
	 */
	public ConnectableRealization getDescriptor(Connectable target)
	{
		ModelingElementDescriptor descriptor = getDescriptor(target.getRemoteInfo());

		if (descriptor instanceof ConnectableRealization) {
			return (ConnectableRealization)descriptor;
		}

		return null;
	}

	/**
	 * @see abeans.models.meta.Reflective#getDescriptor(abeans.models.Linkable)
	 */
	public LinkableRealization getDescriptor(Linkable target)
	{
		if (target instanceof TypelessProperty)
		{
			RemoteInfo remoteInfo = ((TypelessProperty)target).getRemoteInfo();
			ModelingElementDescriptor descriptor = getDescriptor(remoteInfo);
	
			if (descriptor instanceof LinkableRealization) {
				return (LinkableRealization)descriptor;
			}
		}
		
		return null;
	}

	/**
	 * @see abeans.models.meta.Reflective#getDescriptor(RemoteInfo)
	 */
	public ModelingElementDescriptor getDescriptor(RemoteInfo target)
	{
		AbeansDirectory ad = getDirectory();
		Object o = null;

		try {
			o = ad.lookup(target);
		} catch (NamingException e) {
			return null;
		}

		// Also fails if o == null
		if (!(o instanceof ModelingElementDescriptor)) {
			return null;
		}

		return (ModelingElementDescriptor)o;
	}

	/**
	 * @see abeans.models.meta.Reflective#getModelingElementDescriptorTypes()
	 */
	public Class[] getModelingElementDescriptorTypes()
	{
		return modelingElementDescriptorTypes;
	}

	/**
	 * @see abeans.models.meta.Reflective#newBeanInfoInstance(abeans.models.meta.ConnectableRealization)
	 */
	public BeanInfo newBeanInfoInstance(ConnectableRealization cd)
	{
		return null;
	}

	/**
	 * @see abeans.models.meta.Reflective#newBeanInfoInstance(abeans.models.meta.LinkableRealization)
	 */
	public BeanInfo newBeanInfoInstance(LinkableRealization ld)
	{
		return null;
	}

	/**
	 * Attempts to create new instance of object (Abean). This method queries
	 * the directory in order to obtain the neccessary information, then
	 * attempts to instantiate the object.
	 *
	 * @see abeans.models.meta.Reflective#newModelInstance(RemoteInfo)
	 */
	public Abean newModelInstance(RemoteInfo target) throws InstantiationException
	{
		// Replace since we need exception to tell something went wrong
		// ModelingElementDescriptor med = getDescriptor(target);
		AbeansDirectory ad = getDirectory();
		Object o = null;

		try {
			o = ad.lookup(target);
		} catch (NamingException e) {
			throw new InstantiationException(this,
			    "Error occured while querying target '" + target + "'", e);
		}

		ModelingElementDescriptor med;

		// Also fails if o == null
		if (!(o instanceof ModelingElementDescriptor)) {
			throw new InstantiationException(this,
			    "Invalid descriptor obtained for '" + target + "'");
		}

		med = (ModelingElementDescriptor)o;

		if (!(med instanceof abeans.models.acs.baci.ComponentDescriptor)) {
			throw new InstantiationException(this,
			    "Cannot create new model instance for '" + target
			    + "', since it is not defined in directory.");
		}

		abeans.models.acs.baci.ComponentDescriptor cd = (abeans.models.acs.baci.ComponentDescriptor)med;
		Class c = cd.getType();

		if (c == null) {
			throw new InstantiationException(this,
			    "Cannot create new model instance for '" + target
			    + "', does not appear to be connected.");
		}

		Component component = null;

		try {
			component = (Component)c.newInstance();
		} catch (java.lang.InstantiationException e) {
			throw new InstantiationException(this,
			    "Error while creating new model instance for '" + target + "'",
			    e);
		} catch (IllegalAccessException e) {
			throw new InstantiationException(this,
			    "Error while creating new model instance for '" + target + "'",
			    e);
		}

		return component;
	}

	/**
	 * @see abeans.models.meta.Reflective#newProperty(abeans.pluggable.RemoteInfo, abeans.models.Family)
	 */
	public abeans.datatypes.TypelessProperty newProperty(RemoteInfo info, Family family)
		throws InstantiationException {
		// TODO to be implemented
		return null;
	}

}
