/*
 * @@COPYRIGHT@@
 */

package abeans.pluggable.acs.maci;

import java.util.logging.Level;

import javax.naming.Context;
import javax.naming.NamingException;

import abeans.core.ComponentDescriptor;
import abeans.core.ComponentInitializationException;
import abeans.core.ComponentManager;
import abeans.core.ComponentSupport;
import abeans.core.Identifier;
import abeans.core.IllegalComponentStateException;
import abeans.core.Root;
import abeans.core.defaults.MessageLogEntry;
import abeans.models.DistributedDirectory;
import abeans.models.meta.AbeansDirectory;
import abeans.models.meta.ContextRepresentable;
import abeans.models.meta.NamespaceDescriptor;
import abeans.models.meta.TargetPart;
import abeans.pluggable.RemoteDirectory;

/**
 * This class is the implementation of the <code>RemoteDirectory</code> interface defined
 * on the pluggable layer. This is a directory service that integrates into 
 * <code>DistributedDirectory</code> defined on the models layer. This implementation 
 * is a JNDI context that is bound into distributed directory under the name of the 
 * ACS plug (i.e. "abeans-ACS").
 * 
 * @author	Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class ACSRemoteDirectory extends ComponentSupport implements RemoteDirectory
{	
	/**
	 * This component descriptor.
	 */
	private ComponentDescriptor cdesc = null;
	
	/**
	 * This directory root context.
	 */
	private AbeansDirectory context = null;

	/**
	 * Entry that will represent this directory in the Abeans modeling layer
	 * <code>DistributedDirectory</code>
	 */
	private class Descriptor extends NamespaceDescriptor implements ContextRepresentable
	{
		/**
		 * Creates a new instance of the descriptor.
		 */
		public Descriptor()
		{
			super(TargetPart.SCHEMA);
		}
		
		/**
		 * Returns the implementation of the context that will be bound into the
		 * distributed directory service.
		 * 
		 * @return		the context that will become part of the distributed directory
		 */
		public Context getContext()
		{
			return context;
		}

	}

	/**
	 * Creates a new instance of this class.
	 * 
	 * @throws NamingException	not thrown by this implementation
	 */
	public ACSRemoteDirectory() throws NamingException
	{
		super("ACSRemoteDirectory", "ACSRDir", Identifier.PLUG);
	}
	
	/**
	 * Returns the component descriptor for this entry.
	 * 
	 * @return		component descriptor for this
	 */
	public ComponentDescriptor getComponentDescriptor()
	{
		if (cdesc == null)
			cdesc = new ComponentDescriptor(ACSRemoteDirectory.class, ACSRemoteDirectory.class, 1, "Directory implementation for ACS plug.", true, true, null);
		return cdesc;
	}

	/**
	 * Initializes this remote directory. This involves resolving the modeling layer
	 * <code>DistributedDirectory</code> and adding a descriptor for this <code>RemoteDirectory</code>
	 * into it. If the distributed directory service is not available, the installation of 
	 * this component fails. This is a stateless service that does not support state transfers.
	 * 
	 * @param	manager		the manager parent of this, non-<code>null</code>
	 * @param	state		must be <code>null</code>
	 * @param	cdesc		must be <code>null</code>, this component does not support state transfers
	 * @throws	IllegalComponentStateException	
	 * 						if the <code>state</code> or <code>cdesc</code> parameters are not 
	 * 						<code>null</code>
	 * @throws	ComponentInitializationException
	 * 						if the <code>manager</code> parameter is <code>null</code> or the
	 * 						<code>DistributedDirectory</code> service is not present in the 	
	 * 						modeling layer
	 */
	public void initialize(ComponentManager manager, Object state, ComponentDescriptor cdesc) throws IllegalComponentStateException, ComponentInitializationException
	{
		if (manager == null) 
		{
			ComponentInitializationException cie = new ComponentInitializationException(this, "Parameter 'manager' passed to initialize() was null.");
			cie.putValue("state", state);
			cie.putValue("cdesc", cdesc);
			cie.setHelpID("exception_internal");
			throw cie;
		}
		setParent(manager);
		if (state != null || cdesc != null)
		{
			IllegalComponentStateException icse = new IllegalComponentStateException(this, "Cannot interpret a non-null component state.");
			icse.putValue("state", state);
			icse.putValue("cdesc", cdesc);
			icse.putValue("manager", manager);
			icse.setHelpID("exception_internal");
			throw icse;
		}
		
		context = new AbeansDirectory(TargetPart.SCHEMA);
		
		DistributedDirectory dd = (DistributedDirectory)Root.getModels().getDistributedService(DistributedDirectory.class);
		if (dd == null) 
		{
			ComponentInitializationException cie = new ComponentInitializationException(this, "'DistributedDirectory' service is not installed in the modeling layer.");
			cie.putValue("manager", manager);
			throw cie;
		}
		try
		{
			dd.getInitialContext().bind("abeans-" + ACSPlug.PLUG_NAME, new Descriptor());
		} catch (NamingException ne)
		{
			ComponentInitializationException cie = new ComponentInitializationException(this, "Cannot bind Simulator remote directory into the 'DistributedDirectory'.", ne);
			cie.putValue("manager", manager);
			cie.putValue("dd", dd);
			throw cie;
		}
		
	}

	/**
	 * Destroys this remote directory by deregistering it from the distributed directory and
	 * calling super destroy.
	 */
	public void destroy()
	{
		DistributedDirectory dd = (DistributedDirectory)Root.getModels().getDistributedService(DistributedDirectory.class);
		if (dd != null) 
		{
			try
			{
				dd.getInitialContext().unbind("abeans-" + ACSPlug.PLUG_NAME);
			} catch (Exception e)
			{
				new MessageLogEntry(this, "destroy", "Could not unbind '" + this + "' from DistributedDirectory. Skipping...", e, Level.WARNING).dispatch();
			}
		}
		super.destroy();
	}

	/**
	 * @see abeans.pluggable.RemoteDirectory#getCacheLifetime()
	 */
	public long getCacheLifetime()
	{
		return 0;
	}

	/**
	 * @see abeans.pluggable.RemoteDirectory#setCacheLifetime(long)
	 */
	public void setCacheLifetime(long milliseconds)
	{
	}

	/**
	 * Returns the context that will represent this service in the distributed directory.
	 * 
	 * @return the context of this remote directory
	 */
	public Context getContext()
	{
		return context;
	}

}
