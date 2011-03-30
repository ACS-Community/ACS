/*
 * @@COPYRIGHT@@
 */

package com.cosylab.acs.maci.plug;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.CORBA.BAD_PARAM;
import org.omg.CORBA.NO_PERMISSION;
import org.omg.CORBA.NO_RESOURCES;
import org.omg.CORBA.Object;
import org.omg.CORBA.UNKNOWN;

import si.ijs.maci.AdministratorHelper;
import si.ijs.maci.Client;
import si.ijs.maci.ClientHelper;
import si.ijs.maci.ClientInfo;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.Container;
import si.ijs.maci.ContainerHelper;
import si.ijs.maci.ContainerInfo;
import si.ijs.maci.ManagerPOA;
import si.ijs.maci.SynchronousAdministratorHelper;
import si.ijs.maci.LoggingConfigurablePackage.LogLevels;
import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBlong;
import alma.ACSErrTypeCommon.IllegalArgumentEx;
import alma.ACSErrTypeCommon.wrappers.AcsJBadParameterEx;
import alma.ACSErrTypeCommon.wrappers.AcsJIllegalArgumentEx;
import alma.ACSErrTypeCommon.wrappers.AcsJUnexpectedExceptionEx;
import alma.ACSErrTypeOK.wrappers.ACSErrOKAcsJCompletion;
import alma.acs.exceptions.AcsJException;
import alma.acs.logging.ClientLogManager;
import alma.acs.logging.config.LogConfig;
import alma.acs.logging.config.LogConfigException;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.maci.loggingconfig.UnnamedLogger;
import alma.maciErrType.CannotDeactivateComponentEx;
import alma.maciErrType.CannotGetComponentEx;
import alma.maciErrType.CannotRegisterComponentEx;
import alma.maciErrType.ComponentConfigurationNotFoundEx;
import alma.maciErrType.ComponentDeactivationFailedEx;
import alma.maciErrType.ComponentDeactivationUncleanEx;
import alma.maciErrType.ComponentNotAlreadyActivatedEx;
import alma.maciErrType.ComponentSpecIncompatibleWithActiveComponentEx;
import alma.maciErrType.IncompleteComponentSpecEx;
import alma.maciErrType.InvalidComponentSpecEx;
import alma.maciErrType.NoDefaultComponentEx;
import alma.maciErrType.NoPermissionEx;
import alma.maciErrType.wrappers.AcsJCannotGetComponentEx;
import alma.maciErrType.wrappers.AcsJCannotRegisterComponentEx;
import alma.maciErrType.wrappers.AcsJComponentConfigurationNotFoundEx;
import alma.maciErrType.wrappers.AcsJComponentNotAlreadyActivatedEx;
import alma.maciErrType.wrappers.AcsJComponentSpecIncompatibleWithActiveComponentEx;
import alma.maciErrType.wrappers.AcsJIncompleteComponentSpecEx;
import alma.maciErrType.wrappers.AcsJInvalidComponentSpecEx;
import alma.maciErrType.wrappers.AcsJNoDefaultComponentEx;
import alma.maciErrType.wrappers.AcsJNoPermissionEx;

import com.cosylab.acs.maci.AccessRights;
import com.cosylab.acs.maci.BadParametersException;
import com.cosylab.acs.maci.Component;
import com.cosylab.acs.maci.ComponentSpec;
import com.cosylab.acs.maci.ComponentStatus;
import com.cosylab.acs.maci.CoreException;
import com.cosylab.acs.maci.HandleHelper;
import com.cosylab.acs.maci.Manager;
import com.cosylab.acs.maci.Manager.LongCompletionCallback;
import com.cosylab.acs.maci.NoDefaultComponentException;
import com.cosylab.acs.maci.NoResourcesException;
import com.cosylab.acs.maci.StatusHolder;
import com.cosylab.acs.maci.manager.CURLHelper;

/**
 * Manager is the central point of interaction between the components
 * and the clients that request MACI services. A Manager is
 * responsible for managing a domain.
 * Manager has the following functionality:
 *	<UL>
 *	<LI>It is the communication entry point.</LI>
 *	<LI>It performs as a name service, resolving CURLs into Component references.</LI>
 *	<LI>It delegates the Component life cycle management to the Container.</LI>
 *	<LI>It provides information about the whole domain.</LI>
 *	</UL>
 *
 * This class implements the IDL interface of Manager and
 * acts like a proxy, by delegating all requests 
 * to an implementation of the com.cosylab.acs.maci.Manager interface.
 *
 * @todo   Now the Manager interface does not use exceptions in many
 *         methods when there is a failure, but it returns a nil object.
 *         This class will have to be refactored when the Manager implementation
 *         (only used internally) will be changed.
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class ManagerProxyImpl extends ManagerPOA 
{

	// TODO @todo revise...
	private void reportException(Throwable th)
	{
		logger.log(Level.SEVERE, th.getMessage(), th);
	}
	
	/**
	 * Implementation of the manager to which all requests are delegated.
	 */
	private Manager manager;

	/**
	 * Logger.
	 */
	private Logger logger;

	/**
	 * Identifier.
	 */
	private AtomicInteger pendingRequests = new AtomicInteger(0);

	/**
	 * Construct a new Manager which will <code>manager</code> implementation.
	 * @param	manager		implementation of the manager, non-<code>null</code>.
	 * @param	logger		logger.
	 */
	public ManagerProxyImpl(Manager manager, Logger logger)
	{
		assert (manager != null);
		assert (logger != null);

		this.manager = manager;
		this.logger = logger;
	}

	/**
	 * Ping method so that clients can feel good about having an alive manager.
	 * @see si.ijs.maci.ManagerOperations#ping()
	 */
	public boolean ping() {
		return true;		
	}
	
	
	/**
	 * Return the fully qualified name of the domain, e.g., "antenna1.alma.nrao".
	 *
	 * @return the fully qualified name of the domain
	 */
	public String domain_name()
	{
		pendingRequests.incrementAndGet();
		try
		{
			return manager.getDomain();
		}
		catch (Throwable ex)
		{
			CoreException hce = new CoreException(ex.getMessage(), ex);
			reportException(hce);

			// rethrow CORBA specific
			throw new UNKNOWN(ex.getMessage());
		}
		finally
		{
			pendingRequests.decrementAndGet();
		}
	}

	/**
	 * Get all the information that the Manager has about its known Containers.
	 * To invoke this method, the caller must have INTROSPECT_MANAGER access rights, or it must be the object whose info it is requesting.
	 * Calling this function does not affect the internal state of the Manager.
	 *
	 * @param id Identification of the caller.
	 * @param h Handles of the containers whose information is requested. If this is an empty sequence, the name_wc parameter is used.
	 * @param name_wc Wildcard that the container's name must match in order for its information to be returned.
	 * @return A sequence of ContainerInfo structures containing the entire Manager's knowledge about the containers.
	 *		  If access is denied to a subset of objects, the handles to those objects are set to 0.
	 */
	public ContainerInfo[] get_container_info(int id, int[] h, String name_wc)
		throws NoPermissionEx
	{
		pendingRequests.incrementAndGet();
		try
		{
			// invalid info (replacement for null)
			final ContainerInfo invalidInfo = new ContainerInfo("<invalid>", 0, null, new int[0]);

			// returned value
			ContainerInfo[] retVal = null;

			// transform to CORBA specific
			com.cosylab.acs.maci.ContainerInfo[] infos = manager.getContainerInfo(id, h, name_wc);
			if (infos != null)
			{
				retVal = new ContainerInfo[infos.length];
				for (int i = 0; i < infos.length; i++)
					if (infos[i] == null)
						retVal[i] = invalidInfo;
					else
						retVal[i] = new ContainerInfo(infos[i].getName(),
													   infos[i].getHandle(),
													   (Container)((ContainerProxy)infos[i].getContainer()).getClient(),
													   infos[i].getComponents().toArray());
			}
			else
				retVal = new ContainerInfo[0];

			return retVal;
		}
		catch (BadParametersException bpe)
		{
			BadParametersException hbpe = new BadParametersException(bpe.getMessage(), bpe);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(bpe.getMessage());
		}
		catch (NoResourcesException nre)
		{
			NoResourcesException hnre = new NoResourcesException(nre.getMessage(), nre);
			reportException(hnre);

			// rethrow CORBA specific
			throw new NO_RESOURCES(nre.getMessage());
		}
		catch (AcsJNoPermissionEx npe)
		{
			//reportException(npe);
			
			// rethrow CORBA specific
			throw npe.toNoPermissionEx();
		}
		catch (Throwable ex)
		{
			CoreException hce = new CoreException(ex.getMessage(), ex);
			reportException(hce);

			// rethrow CORBA specific
			throw new UNKNOWN(ex.getMessage());
		}
		finally
		{
			pendingRequests.decrementAndGet();
		}
	}

	/**
	 * Get all the information that the Manager has about its known clients.
	 * To invoke this method, the caller must have INTROSPECT_MANAGER access rights, or it must be the object whose info it is requesting.
	 * Calling this function does not affect the internal state of the Manager.
	 *
	 * @param id Identification of the caller.
	 * @param h Handles of the clients whose information is requested. If this is an empty sequence, the name_wc parameter is used.
	 * @param name_wc Wildcard that the clients's name must match in order for its information to be returned.
	 * @return A sequence of ClientInfo structures containing the entire Manager's knowledge about the containers.
	 *		  If access is denied to a subset of objects, the handles to those objects are set to 0.
	 */
	public ClientInfo[] get_client_info(int id, int[] h, String name_wc)
		throws NoPermissionEx
	{
		pendingRequests.incrementAndGet();
		try
		{

			// invalid info (replacement for null)
			final ClientInfo invalidInfo = new ClientInfo(0, null, new int[0], "<invalid>", 0);

			// returned value
			ClientInfo[] retVal = null;

			// transform to CORBA specific
			com.cosylab.acs.maci.ClientInfo[] infos = manager.getClientInfo(id, h, name_wc);
			if (infos != null)
			{
				retVal = new ClientInfo[infos.length];
				for (int i = 0; i < infos.length; i++)
					if (infos[i] == null)
						retVal[i] = invalidInfo;
					else
						retVal[i] = new ClientInfo(infos[i].getHandle(),
												    ((ClientProxy)infos[i].getClient()).getClient(),
													infos[i].getComponents().toArray(),
													infos[i].getName(),
													mapAccessRights(infos[i].getAccessRights()));
			}
			else
				retVal = new ClientInfo[0];

			return retVal;
		}
		catch (BadParametersException bpe)
		{
			BadParametersException hbpe = new BadParametersException(bpe.getMessage(), bpe);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(bpe.getMessage());
		}
		catch (NoResourcesException nre)
		{
			NoResourcesException hnre = new NoResourcesException(nre.getMessage(), nre);
			reportException(hnre);

			// rethrow CORBA specific
			throw new NO_RESOURCES(nre.getMessage());
		}
		catch (AcsJNoPermissionEx npe)
		{
			//reportException(npe);
			
			// rethrow CORBA specific
			throw npe.toNoPermissionEx();
		}
		catch (Throwable ex)
		{
			CoreException hce = new CoreException(ex.getMessage(), ex);
			reportException(hce);

			// rethrow CORBA specific
			throw new UNKNOWN(ex.getMessage());
		}
		finally
		{
			pendingRequests.decrementAndGet();
		}
	}

	/**
	 * Get all the information that the Manager has about components.
	 * To invoke this method, the caller must have INTROSPECT_MANAGER access rights, or it must have adequate privileges to access the Component (the same as with the get_component method).
	 * Information about all components is returned, unless the active_only parameter is set to true,
	 * in which case only information about those components that are currently registered with the Manager
	 * and activated is returned.
	 * Calling this function does not affect the internal state of the Manager.
	 *
	 * @param id Identification of the caller.
	 * @param h Handles of the components whose information is requested. If this is an empty sequence, the name_wc and type_wc parameters are used.
	 * @param name_wc Wildcard that the Component's name must match in order for its information to be returned.
	 * @param type_wc Wildcard that the Component's type must match in order for its information to be returned.
	 * @param active_only
	 * @return A sequence of ComponentInfo structures containing the entire Manager's knowledge about the components.
	 *		  If access is denied to a subset of objects, the handles to those objects are set to 0.
	 */
	public ComponentInfo[] get_component_info(int id, int[] h, String name_wc, String type_wc, boolean active_only)
		throws NoPermissionEx
	{
		pendingRequests.incrementAndGet();
		try
		{

			// invalid info (replacement for null)
			final ComponentInfo invalidInfo = new ComponentInfo("<invalid>", "<invalid>", null, "<invalid>", new int[0], 0, "<invalid>", 0, 0, new String[0]);

			// returned value
			ComponentInfo[] retVal = null;

			// transform to CORBA specific
			com.cosylab.acs.maci.ComponentInfo[] infos = manager.getComponentInfo(id, h, name_wc, type_wc, active_only);
			if (infos != null)
			{
				retVal = new ComponentInfo[infos.length];
				for (int i = 0; i < infos.length; i++)
					if (infos[i] == null)
						retVal[i] = invalidInfo;
					else
					{
						Object obj = null;
						if (infos[i].getComponent() != null)
							obj = (Object)infos[i].getComponent().getObject();
						String[] interfaces;
						if (infos[i].getInterfaces() != null)
							interfaces = infos[i].getInterfaces();
						else
							interfaces = new String[0];
						retVal[i] = new ComponentInfo(infos[i].getType(),
												 infos[i].getCode(),
											     obj,
												 infos[i].getName(),
												 infos[i].getClients().toArray(),
												 infos[i].getContainer(),
												 infos[i].getContainerName(),
												 infos[i].getHandle(),
												 mapAccessRights(infos[i].getAccessRights()),
												 interfaces);
					}
			}
			else
				retVal = new ComponentInfo[0];

			return retVal;
		}
		catch (BadParametersException bpe)
		{
			BadParametersException hbpe = new BadParametersException(bpe.getMessage(), bpe);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(bpe.getMessage());
		}
		catch (NoResourcesException nre)
		{
			NoResourcesException hnre = new NoResourcesException(nre.getMessage(), nre);
			reportException(hnre);

			// rethrow CORBA specific
			throw new NO_RESOURCES(nre.getMessage());
		}
		catch (AcsJNoPermissionEx npe)
		{
			//reportException(npe);
			
			// rethrow CORBA specific
			throw npe.toNoPermissionEx();
		}
		catch (Throwable ex)
		{
			CoreException hce = new CoreException(ex.getMessage(), ex);
			reportException(hce);

			// rethrow CORBA specific
			throw new UNKNOWN(ex.getMessage());
		}
		finally
		{
			pendingRequests.decrementAndGet();
		}
	}

	/**
	 * Get a Component, activating it if necessary.
	 * The client represented by id (the handle)
	 * must have adequate access rights to access the Component. This is untrue of components:
	 * components always have unlimited access rights to other components.
	 *
	 * @param id Identification of the caller. If this is an invalid handle, or if the caller does not have enough access rights, a maciErrType::NoPermissionEx exception is raised.
	 * @param component_url CURL of the Component whose reference is to be retrieved.
	 * @param activate True if the Component is to be activated in case it does not exist. If set to False, and the Component does not exist, a nil reference is returned and status is set to COMPONENT_NOT_ACTIVATED.
	 * @return Reference to the Component. If the Component could not be activated, an exception is throw.
	 */
	public Object get_component(int id, String component_url, boolean activate)
	    throws NoPermissionEx, CannotGetComponentEx, ComponentNotAlreadyActivatedEx, ComponentConfigurationNotFoundEx
	{
		pendingRequests.incrementAndGet();
		try
		{
			// returned value
			Object retVal = null;

			// returned status
			StatusHolder statusHolder = new StatusHolder();

			// transform to CORBA specific
			URI uri = null;
			if (component_url != null)
				uri = CURLHelper.createURI(component_url);
			Component component = manager.getComponent(id, uri, activate, statusHolder);

			// extract Component CORBA reference
			if (component != null)
				retVal = (Object)component.getObject();

			/* This handles a failure in retrieving the component */
			/**
			 * @todo GCH 2006.10.11
			 *       notice that we can get a ComponentStatus != COMPONENT_ACTIVATED
			 *       also if the component is properly returned.
			 *       There is an incoherence here in the interfaces that shall be resolved.
			 *       We have to cleanup here or go back to return a status
			 *       to the caller.
			 *       My point is: the caller is interested in more than just 
			 *       getting the component of an indication of failure if not?
			 *       Is it interesting to know that the component was not activated,
			 *       presumably because already active? 
			 */ 
			if (component == null || component.getObject() == null)
			{
			    if (statusHolder.getStatus() == ComponentStatus.COMPONENT_NOT_ACTIVATED && !activate)
			    {
			    	AcsJComponentNotAlreadyActivatedEx ex = new AcsJComponentNotAlreadyActivatedEx();
			    	ex.setCURL(component_url);
			    	throw ex;
			    }
			    if (statusHolder.getStatus() == ComponentStatus.COMPONENT_DOES_NO_EXIST)
			    {
			    	AcsJComponentConfigurationNotFoundEx ex = new AcsJComponentConfigurationNotFoundEx();
			    	ex.setCURL(component_url);
			    	throw ex;
			    }
			    else
			    {
			    	AcsJCannotGetComponentEx ex = new AcsJCannotGetComponentEx();
			    	ex.setCURL(component_url);
			    	throw ex;
			    }
			}

			return retVal;
		}
		catch (URISyntaxException usi)
		{
			BadParametersException hbpe = new BadParametersException(usi.getMessage(), usi);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(usi.getMessage());
		}
		catch (BadParametersException bpe)
		{
			BadParametersException hbpe = new BadParametersException(bpe.getMessage(), bpe);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(bpe.getMessage());
		}
		catch (NoResourcesException nre)
		{
			NoResourcesException hnre = new NoResourcesException(nre.getMessage(), nre);
			reportException(hnre);

			// rethrow CORBA specific
			throw new NO_RESOURCES(nre.getMessage());
		}
		catch (AcsJCannotGetComponentEx cgce)
		{
			//reportException(cgce);

			// rethrow CORBA specific
			throw cgce.toCannotGetComponentEx();
		}
		catch (AcsJComponentNotAlreadyActivatedEx cnaae)
		{
			//reportException(cnaae);

			// rethrow CORBA specific
			throw cnaae.toComponentNotAlreadyActivatedEx();
		}
		catch (AcsJComponentConfigurationNotFoundEx ccnfe)
		{
			//reportException(ccnfe);

			// rethrow CORBA specific
			throw ccnfe.toComponentConfigurationNotFoundEx();
		}
		catch (AcsJNoPermissionEx npe)
		{
			//reportException(npe);
			
			// rethrow CORBA specific
			throw npe.toNoPermissionEx();
		}
		catch (Throwable ex)
		{
			CoreException hce = new CoreException(ex.getMessage(), ex);
			reportException(hce);

			// rethrow CORBA specific
			throw new UNKNOWN(ex.getMessage());
		}
		finally
		{
			pendingRequests.decrementAndGet();
		}
	}

	/**
	 * Get a component, do not activate it and also do not do any reference counting.
	 * The client represented by id (the handle)
	 * must have adequate access rights to access the Component. This is untrue of components:
	 * components always have unlimited access rights to other components.
	 *
	 * @param id Identification of the caller. If this is an invalid handle, or if the caller does not have enough access rights, a maciErrType::NoPermissionEx exception is raised.
	 * @param component_url CURL of the Component whose reference is to be retrieved.
	 * @return Reference to the Component.
	 */
	public Object get_component_non_sticky(int id, String component_url)
		throws NoPermissionEx, CannotGetComponentEx, ComponentNotAlreadyActivatedEx
	{
		pendingRequests.incrementAndGet();
		try
		{
			// returned value
			Object retVal = null;

			// transform to CORBA specific
			URI uri = null;
			if (component_url != null)
				uri = CURLHelper.createURI(component_url);
			Component component = manager.getComponentNonSticky(id, uri);

			// extract Component CORBA reference
			if (component != null)
				retVal = (Object)component.getObject();

			// @todo
			if (component == null)
				throw new AcsJComponentNotAlreadyActivatedEx();
			
			return retVal;
		}
		catch (URISyntaxException usi)
		{
			BadParametersException hbpe = new BadParametersException(usi.getMessage(), usi);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(usi.getMessage());
		}
		catch (BadParametersException bpe)
		{
			BadParametersException hbpe = new BadParametersException(bpe.getMessage(), bpe);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(bpe.getMessage());
		}
		catch (NoResourcesException nre)
		{
			NoResourcesException hnre = new NoResourcesException(nre.getMessage(), nre);
			reportException(hnre);

			// rethrow CORBA specific
			throw new NO_RESOURCES(nre.getMessage());
		} 
		catch (AcsJCannotGetComponentEx cgce)
		{
			//reportException(cgce);

			// rethrow CORBA specific
			throw cgce.toCannotGetComponentEx();
		}
		catch (AcsJComponentNotAlreadyActivatedEx cnaae)
		{
			//reportException(cnaae);

			// rethrow CORBA specific
			throw cnaae.toComponentNotAlreadyActivatedEx();
		}
		catch (AcsJNoPermissionEx npe)
		{
			//reportException(npe);
			
			// rethrow CORBA specific
			throw npe.toNoPermissionEx();
		}
		catch (Throwable ex)
		{
			CoreException hce = new CoreException(ex.getMessage(), ex);
			reportException(hce);

			// rethrow CORBA specific
			throw new UNKNOWN(ex.getMessage());
		}
		finally
		{
			pendingRequests.decrementAndGet();
		}
	}



	/**
	 * Login to MACI.
	 * Containers, Clients and Administrative clients call this function
	 * first to identify themselves with the Manager. The Manager authenticates them
	 * (through the authenticate function), and assigns them access rights and a handle,
	 * through which they will identify themselves at subsequent calls to the Manager.
	 *
	 * @param reference A reference to the Client.
	 * @return A ClientInfo structure with handle (h) and access fields filled-in.
	 * 			If the client with this name did not logout prior to calling login,
	 *			the components sequence in ClientInfo contains the handles of all components that
	 *			the client was using. (For Containers, the components sequence contains
	 *			handles of all components previously hosted by the Container.)
	 */
	public ClientInfo login(Client reference)
		throws NoPermissionEx
	{
		pendingRequests.incrementAndGet();
		try
		{

			// client proxy
			com.cosylab.acs.maci.Client clientProxy = null;

			// create approperiate proxies
			if (reference != null)
			{
				if (reference._is_a(ContainerHelper.id()))
				{
					clientProxy = new ContainerProxy(ContainerHelper.narrow(reference));
				}
				else if (reference._is_a(SynchronousAdministratorHelper.id()))
				{
					clientProxy = new SynchronousAdministratorProxy(SynchronousAdministratorHelper.narrow(reference));
				}
				else if (reference._is_a(AdministratorHelper.id()))
				{
					clientProxy = new AdministratorProxy(AdministratorHelper.narrow(reference));
				}
				else if (reference._is_a(ClientHelper.id()))
				{
					clientProxy = new ClientProxy(reference);
				}
				else
				{
					// this should never happen, but we are carefuly anyway
					BadParametersException af = new BadParametersException("Given reference does not implement 'maci::Client' interface.");
					reportException(af);

					throw new BAD_PARAM(af.getMessage());
				}
			}


			ClientInfo retVal = null;

			com.cosylab.acs.maci.ClientInfo info = manager.login(clientProxy);
			if (info != null)
				retVal = new ClientInfo(info.getHandle(),
										((ClientProxy)info.getClient()).getClient(),
										info.getComponents().toArray(),
										info.getName(),
										mapAccessRights(info.getAccessRights()));


			return retVal;
		}
		catch (BadParametersException bpe)
		{
			BadParametersException hbpe = new BadParametersException(bpe.getMessage(), bpe);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(bpe.getMessage());
		}
		catch (NoResourcesException nre)
		{
			NoResourcesException hnre = new NoResourcesException(nre.getMessage(), nre);
			reportException(hnre);

			// rethrow CORBA specific
			throw new NO_RESOURCES(nre.getMessage());
		}
		catch (AcsJNoPermissionEx npe)
		{
			//reportException(npe);
			
			// rethrow CORBA specific
			throw npe.toNoPermissionEx();
		}
		catch (Throwable ex)
		{
			CoreException hce = new CoreException(ex.getMessage(), ex);
			reportException(hce);

			// rethrow CORBA specific
			throw new UNKNOWN(ex.getMessage());
		}
		finally
		{
			pendingRequests.decrementAndGet();
		}
	}

	/**
	 * Logout from MACI.
	 *
	 * @param id Handle of the Client that is logging out
	 */
	public void logout(int id)
		throws NoPermissionEx
	{
		pendingRequests.incrementAndGet();
		try
		{
			// simply logout
			manager.logout(id);
		}
		catch (AcsJNoPermissionEx npe)
		{
			//reportException(npe);

			// rethrow adding context
			AcsJNoPermissionEx ex = new AcsJNoPermissionEx(npe);
			ex.setReason(npe.getReason());
			ex.setID(HandleHelper.toString(id));
			throw ex.toNoPermissionEx();
		}
		catch (BadParametersException bpe)
		{
			BadParametersException hbpe = new BadParametersException(bpe.getMessage(), bpe);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(bpe.getMessage());
		}
		catch (NoResourcesException nre)
		{
			NoResourcesException hnre = new NoResourcesException(nre.getMessage(), nre);
			reportException(hnre);

			// rethrow CORBA specific
			throw new NO_RESOURCES(nre.getMessage());
		}
		catch (Throwable ex)
		{
			CoreException hce = new CoreException(ex.getMessage(), ex);
			reportException(hce);

			// rethrow CORBA specific
			throw new UNKNOWN(ex.getMessage());
		}
		finally
		{
			pendingRequests.decrementAndGet();
		}
	}

	/**
	 * Register a CORBA object as a Component, assigning it a CURL and making it accessible through the Manager.
	 * The Component is treated as an immortal Component.
	 *
	 * @param id Identification of the caller. The caller must have the REGISTER_component access right to perform this operation.
	 * @param component_url CURL that will be assigned to the object. The CURL must be in the Manager's domain, otherwise a fundamental property of domains that one computer belongs to only one domain would be too easy to violate.
	 * @param type Type of the Component
	 * @param Component Reference to the CORBA object (Component).
	 * @return Returns the handle of the newly created Component.
	 */
	public int register_component(int id, String component_url, String type, Object component)
		throws NoPermissionEx, CannotRegisterComponentEx
	{
		pendingRequests.incrementAndGet();
		try
		{
			// simply register
			URI uri = null;
			if (component_url != null)
				uri = CURLHelper.createURI(component_url);
			return manager.registerComponent(id, uri, type, new ComponentProxy(component_url, component));
		}
		catch (URISyntaxException usi)
		{
			BadParametersException hbpe = new BadParametersException(usi.getMessage(), usi);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(usi.getMessage());
		}
		catch (BadParametersException bpe)
		{
			BadParametersException hbpe = new BadParametersException(bpe.getMessage(), bpe);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(bpe.getMessage());
		}
		catch (NoResourcesException nre)
		{
			NoResourcesException hnre = new NoResourcesException(nre.getMessage(), nre);
			reportException(hnre);

			// rethrow CORBA specific
			throw new NO_RESOURCES(nre.getMessage());
		} 
		catch (AcsJNoPermissionEx npe)
		{
			//reportException(npe);

			// rethrow CORBA specific
			throw npe.toNoPermissionEx();
		}
		catch (AcsJBadParameterEx bpe)
		{
			reportException(bpe);
			AcsJCannotRegisterComponentEx crce = new AcsJCannotRegisterComponentEx(bpe);
			// rethrow CORBA specific
			throw crce.toCannotRegisterComponentEx();
		}
		catch (Throwable ex)
		{
			CoreException hce = new CoreException(ex.getMessage(), ex);
			reportException(hce);

			// rethrow CORBA specific
			throw new UNKNOWN(ex.getMessage());
		}
		finally
		{
			pendingRequests.decrementAndGet();
		}
	}

	/**
	 * Change mortality state of an component.
	 * Compnent must be already active, otherwise ComponentNotAlreadyActivatedEx exception will be thrown.
	 * The caller must be an owner of an component or have administator rights,
	 * otherwise NoPermissionEx exception will be thrown.
	 * 
	 * @param id Identification of the caller. The caller must be an owner of an component or have administator rights.
	 * @param component_url The CURL of the component whose mortality to change.
	 * @param immortal_state New mortality state.
	 **/
	public void make_component_immortal(int id, String component_url, boolean immortal_state)
		throws NoPermissionEx, ComponentNotAlreadyActivatedEx
	{
		pendingRequests.incrementAndGet();
		try
		{
			// simply release Component
			URI uri = null;
			if (component_url != null)
				uri = CURLHelper.createURI(component_url);
			manager.makeComponentImmortal(id, uri, immortal_state);
		}
		catch (URISyntaxException usi)
		{
			BadParametersException hbpe = new BadParametersException(usi.getMessage(), usi);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(usi.getMessage());
		}
		catch (BadParametersException bpe)
		{
			BadParametersException hbpe = new BadParametersException(bpe.getMessage(), bpe);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(bpe.getMessage());
		}
		// @todo 
		catch (NoResourcesException nre)
		{
			NoResourcesException hnre = new NoResourcesException(nre.getMessage(), nre);
			reportException(hnre);

			// rethrow CORBA specific
			throw new AcsJComponentNotAlreadyActivatedEx().toComponentNotAlreadyActivatedEx();
		}
		catch (AcsJNoPermissionEx npe)
		{
			//reportException(npe);
			
			// rethrow CORBA specific
			throw npe.toNoPermissionEx();
		}
		catch (Throwable ex)
		{
			CoreException hce = new CoreException(ex.getMessage(), ex);
			reportException(hce);

			// rethrow CORBA specific
			throw new UNKNOWN(ex.getMessage());
		}
		finally
		{
			pendingRequests.decrementAndGet();
		}
	}
	
	public void release_component_async(int id, String component_url,
			CBlong callback, CBDescIn desc) throws NoPermissionEx
	{
		pendingRequests.incrementAndGet();
		try
		{
			// simply release Component
			URI uri = null;
			if (component_url != null)
				uri = CURLHelper.createURI(component_url);
			
			final CBlong fcallback = callback;
			final CBDescOut descOut = new CBDescOut(0, desc.id_tag);
			LongCompletionCallback lcc = null;
			if (callback != null)
			{
				lcc = new LongCompletionCallback() {
				
					public void failed(int result, Throwable exception) {
						if (exception instanceof AcsJException)
						{
							AcsJException aex = (AcsJException)exception;
							fcallback.done(result, aex.toAcsJCompletion().toCorbaCompletion(), descOut);
						
						}
						else
						{
							AcsJUnexpectedExceptionEx uex = new AcsJUnexpectedExceptionEx(exception);
							fcallback.done(result, uex.toAcsJCompletion().toCorbaCompletion(), descOut);
						}
					}
				
					public void done(int result) {
						fcallback.done(result, new ACSErrOKAcsJCompletion().toCorbaCompletion(), descOut);
					}
				};
			}

			manager.releaseComponentAsync(id, uri, lcc);
			
		}
		catch (AcsJNoPermissionEx nop)
		{
			reportException(nop);

			// rethrow CORBA specific
			throw nop.toNoPermissionEx();
		}
		catch (URISyntaxException usi)
		{
			BadParametersException hbpe = new BadParametersException(usi.getMessage(), usi);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(usi.getMessage());
		}
		catch (BadParametersException bpe)
		{
			BadParametersException hbpe = new BadParametersException(bpe.getMessage(), bpe);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(bpe.getMessage());
		}
		catch (NoResourcesException nre)
		{
			NoResourcesException hnre = new NoResourcesException(nre.getMessage(), nre);
			reportException(hnre);

			// rethrow CORBA specific
			throw new NO_RESOURCES(nre.getMessage());
		}
		catch (Throwable ex)
		{
			CoreException hce = new CoreException(ex.getMessage(), ex);
			reportException(hce);

			// rethrow CORBA specific
			throw new UNKNOWN(ex.getMessage());
		}
		finally
		{
			pendingRequests.decrementAndGet();
		}
	}

	/**
	 * Release a Component.
	 * In order for this operation to be possible, the caller represented by the id
	 * must have previously successfully requested the Component via a call to get_component.
	 * Releasing a Component more times than requesting it should be avoided, but it produces no errors.
	 *
	 * @param id Identification of the caller. The caller must have previously gotten the Component through get_component.
	 * @param component_url The CURL of the Component to be released.
	 * @return Number of clients that are still using the Component after the operation completed.
	 *		  This is a useful debugging tool.
	 */
	public int release_component(int id, String component_url)
		throws CannotDeactivateComponentEx, ComponentDeactivationUncleanEx,
		ComponentDeactivationFailedEx, NoPermissionEx
	{
		// TODO support ACS exceptions
		pendingRequests.incrementAndGet();
		try
		{
			// simply release Component
			URI uri = null;
			if (component_url != null)
				uri = CURLHelper.createURI(component_url);
			return manager.releaseComponent(id, uri);
		}
		catch (URISyntaxException usi)
		{
			BadParametersException hbpe = new BadParametersException(usi.getMessage(), usi);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(usi.getMessage());
		}
		catch (BadParametersException bpe)
		{
			BadParametersException hbpe = new BadParametersException(bpe.getMessage(), bpe);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(bpe.getMessage());
		}
		catch (NoResourcesException nre)
		{
			NoResourcesException hnre = new NoResourcesException(nre.getMessage(), nre);
			reportException(hnre);

			// rethrow CORBA specific
			throw new NO_RESOURCES(nre.getMessage());
		}
		catch (Throwable ex)
		{
			CoreException hce = new CoreException(ex.getMessage(), ex);
			reportException(hce);

			// rethrow CORBA specific
			throw new UNKNOWN(ex.getMessage());
		}
		finally
		{
			pendingRequests.decrementAndGet();
		}
	}


	/**
	 * Forcefully release a Component.
	 *
	 * @param id Identification of the caller. 
	 * @param component_url The CURL of the Component to be released.
	 * @return Number of clients that are still using the Component after the operation completed.
	 *		  This is a useful debugging tool.
	 */
	public int force_release_component(int id, String component_url)
		throws NoPermissionEx
	{
		pendingRequests.incrementAndGet();
		try
		{
			// simply release Component
			URI uri = null;
			if (component_url != null)
				uri = CURLHelper.createURI(component_url);
			return manager.forceReleaseComponent(id, uri);
		}
		catch (URISyntaxException usi)
		{
			BadParametersException hbpe = new BadParametersException(usi.getMessage(), usi);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(usi.getMessage());
		}
		catch (BadParametersException bpe)
		{
			BadParametersException hbpe = new BadParametersException(bpe.getMessage(), bpe);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(bpe.getMessage());
		}
		catch (NoResourcesException nre)
		{
			NoResourcesException hnre = new NoResourcesException(nre.getMessage(), nre);
			reportException(hnre);

			// rethrow CORBA specific
			throw new NO_RESOURCES(nre.getMessage());
		}
		catch (AcsJBadParameterEx bpe)
		{
			reportException(bpe);
			
			// rethrow CORBA specific
			//throw bpe.toBadParameterEx();
			throw new BAD_PARAM(bpe.getMessage());
		}
		catch (AcsJNoPermissionEx npe)
		{
			//reportException(npe);
			
			// rethrow CORBA specific
			throw npe.toNoPermissionEx();
		}
		catch (Throwable ex)
		{
			CoreException hce = new CoreException(ex.getMessage(), ex);
			reportException(hce);

			// rethrow CORBA specific
			throw new UNKNOWN(ex.getMessage());
		}
		finally
		{
			pendingRequests.decrementAndGet();
		}
	}

	/**
	 * Shutdown the Manager.
	 * <B>Warning:</B> This call will also deactivate all components active in the system, including startup and immortal components.
	 *
	 * @param id Identification of the caller. The caller must have the SHUTDOWN_SYSTEM access right.
	 * @param containers The code to send to shutdown methods of all Containers. If 0, the Container's shutdown methods are not called.
	 */
	public void shutdown(int id, int containers)
	{
		pendingRequests.incrementAndGet();
		try
		{
			// simply shutdown
			manager.shutdown(id, containers);
		}
		catch (BadParametersException bpe)
		{
			BadParametersException hbpe = new BadParametersException(bpe.getMessage(), bpe);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(bpe.getMessage());
		}
		catch (NoResourcesException nre)
		{
			NoResourcesException hnre = new NoResourcesException(nre.getMessage(), nre);
			reportException(hnre);

			// rethrow CORBA specific
			throw new NO_RESOURCES(nre.getMessage());
		}
		catch (AcsJNoPermissionEx npe)
		{
			//reportException(npe);
			
			// rethrow CORBA specific
			//throw npe.toNoPermissionEx();
			throw new NO_PERMISSION(npe.getMessage());
		}
		catch (Throwable ex)
		{
			CoreException hce = new CoreException(ex.getMessage(), ex);
			reportException(hce);

			// rethrow CORBA specific
			throw new UNKNOWN(ex.getMessage());
		}
		finally
		{
			pendingRequests.decrementAndGet();
		}

	}

	/**
	 * Unregister a Component from the Manager.
	 *
	 * @param id Identification of the caller. The caller must have the REGISTER_COMPONENT access right to perform this operation.
	 * @param h Component's handle. The Component must have been previously registered through the call to register_component. If there are clients still using this Component,
	 * a components_unavailable notification is issued to all of them, and the Component is unregistered.
	 */
	public void unregister_component(int id, int h)
		throws NoPermissionEx
	{
		pendingRequests.incrementAndGet();
		try
		{
			// simply unregistercomponent
			manager.unregisterComponent(id, h);
		}
		catch (BadParametersException bpe)
		{
			BadParametersException hbpe = new BadParametersException(bpe.getMessage(), bpe);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(bpe.getMessage());
		}
		catch (NoResourcesException nre)
		{
			NoResourcesException hnre = new NoResourcesException(nre.getMessage(), nre);
			reportException(hnre);

			// rethrow CORBA specific
			throw new NO_RESOURCES(nre.getMessage());
		}
		catch (AcsJBadParameterEx bpe)
		{
			reportException(bpe);
			
			// rethrow CORBA specific
			//throw bpe.toBadParameterEx();
			throw new BAD_PARAM(bpe.getMessage());
		}
		catch (AcsJNoPermissionEx npe)
		{
			//reportException(npe);
			
			// rethrow CORBA specific
			throw npe.toNoPermissionEx();
		} /*
		catch (AcsJCannotUnregisterComponentEx cuce)
		{
			reportException(cuce);

			// rethrow CORBA specific
			throw cuce.toCannotUnregisterComponentEx();
		} */
		catch (Throwable ex)
		{
			CoreException hce = new CoreException(ex.getMessage(), ex);
			reportException(hce);

			// rethrow CORBA specific
			throw new UNKNOWN(ex.getMessage());
		}
		finally
		{
			pendingRequests.decrementAndGet();
		}
	}

	/**
	 * Returns the default component of specific type.
	 * @param	id		identification of the caller.
	 * @param	type	component type, IDL ID.
	 * @return	<code>ComponentInfo</code> of requested component.
	 */
    public ComponentInfo get_default_component(int id, String type)
    	throws NoPermissionEx, NoDefaultComponentEx, CannotGetComponentEx
    {
		pendingRequests.incrementAndGet();
		try
		{
			// returned value
			ComponentInfo retVal = null;

			// transform to CORBA specific
			com.cosylab.acs.maci.ComponentInfo info = manager.getDefaultComponent(id, type);
			if (info == null || info.getComponent() == null)
				throw new AcsJCannotGetComponentEx();

			Object obj = null;

			obj = (Object)info.getComponent().getObject();
			String[] interfaces;

			if (info.getInterfaces() != null)
				interfaces = info.getInterfaces();
			else
				interfaces = new String[0];
				
			retVal = new ComponentInfo(info.getType(),
									    info.getCode(),
										obj,
										info.getName(),
										info.getClients().toArray(),
										info.getContainer(),
										info.getContainerName(),
									 	info.getHandle(),
									 	mapAccessRights(info.getAccessRights()),
									 	interfaces);

			return retVal;

		}
		catch (NoDefaultComponentException ndce)
		{
			NoDefaultComponentException hndce = new NoDefaultComponentException(ndce.getMessage(), ndce);
			reportException(hndce);

			// rethrow CORBA specific
			throw new AcsJNoDefaultComponentEx().toNoDefaultComponentEx();
		}
		catch (BadParametersException bpe)
		{
			BadParametersException hbpe = new BadParametersException(bpe.getMessage(), bpe);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(bpe.getMessage());
		}
		catch (NoResourcesException nre)
		{
			NoResourcesException hnre = new NoResourcesException(nre.getMessage(), nre);
			reportException(hnre);

			// rethrow CORBA specific
			throw new NO_RESOURCES(nre.getMessage());
		}
		catch (AcsJNoPermissionEx npe)
		{
			//reportException(npe);
			
			// rethrow CORBA specific
			throw npe.toNoPermissionEx();
		}
		catch (AcsJCannotGetComponentEx cgce)
		{
			//reportException(cgce);

			// rethrow CORBA specific
			throw cgce.toCannotGetComponentEx();
		}
		catch (Throwable ex)
		{
			CoreException hce = new CoreException(ex.getMessage(), ex);
			reportException(hce);

			// rethrow CORBA specific
			throw new UNKNOWN(ex.getMessage());
		}
		finally
		{
			pendingRequests.decrementAndGet();
		}
	}

	/**
	 * Activation of an dynamic component.
	 * @param	id 	identification of the caller.
	 * @param	c	component to be obtained.
	 * @param	mark_as_default	mark component as default component of its type.
	 * @return	<code>ComponentInfo</code> of requested component.
	 */
	public ComponentInfo get_dynamic_component(int id, si.ijs.maci.ComponentSpec c, boolean mark_as_default)
		throws NoPermissionEx, IncompleteComponentSpecEx, 
	               InvalidComponentSpecEx, 
	               ComponentSpecIncompatibleWithActiveComponentEx,
	               CannotGetComponentEx
	{
		pendingRequests.incrementAndGet();

		try
		{

			// returned value
			ComponentInfo retVal = null;

			/*
			URI uri = null;
			if (c.component_name != null)
				uri = CURLHelper.createURI(c.component_name);
			ComponentSpec componentSpec = new ComponentSpec(uri, c.component_type, c.component_code, c.container_name);
			*/
			// @todo si.ijs.maci.COMPONENT_SPEC_ANY -> ComponentSpec.COMPSPEC_ANY
			ComponentSpec componentSpec = 
			    new ComponentSpec(c.component_name, 
					      c.component_type, 
					      c.component_code, 
					      c.container_name);
			com.cosylab.acs.maci.ComponentInfo info = 
			    manager.getDynamicComponent(id, componentSpec, mark_as_default);

			// transform to CORBA specific
			if (info == null || info.getComponent() == null)
				throw new AcsJCannotGetComponentEx();

			Object obj = null;
			obj = (Object)info.getComponent().getObject();
			String[] interfaces;
			if (info.getInterfaces() != null)
				interfaces = info.getInterfaces();
			else
				interfaces = new String[0];
				
			retVal = new ComponentInfo(info.getType(),
						   info.getCode(),
						   obj,
						   info.getName(),
						   info.getClients().toArray(),
						   info.getContainer(),
						   info.getContainerName(),
						   info.getHandle(),
						   mapAccessRights(info.getAccessRights()),
						   interfaces);

			return retVal;

		}
		/*
		catch (URISyntaxException usi)
		{
			BadParametersException hbpe = new BadParametersException(usi.getMessage(), usi);
			hbpe.caughtIn(this, "get_dynamic_component");
			hbpe.putValue("c.component_name", c.component_name);
			// exception service will handle this
			reportException(hbpe);
	
			// rethrow CORBA specific
			throw new BAD_PARAM(usi.getMessage());
		}*/
		catch (AcsJInvalidComponentSpecEx ics)
		{
			//reportException(ics);
			
			// rethrow CORBA specific
			throw ics.toInvalidComponentSpecEx();
		}
		catch (AcsJIncompleteComponentSpecEx ics)
		{
			//reportException(ics);
			
			// rethrow CORBA specific
			throw ics.toIncompleteComponentSpecEx();
		}
		catch (AcsJComponentSpecIncompatibleWithActiveComponentEx cpiwac)
		{
			//reportException(cpiwac);
			
			// rethrow CORBA specific
			throw cpiwac.toComponentSpecIncompatibleWithActiveComponentEx();
		}
		catch (AcsJNoPermissionEx npe)
		{
			//reportException(npe);
			
			// rethrow CORBA specific
			throw npe.toNoPermissionEx();
		}
		catch (BadParametersException bpe)
		{
			BadParametersException hbpe = new BadParametersException(bpe.getMessage(), bpe);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(bpe.getMessage());
		}
		catch (NoResourcesException nre)
		{
			NoResourcesException hnre = new NoResourcesException(nre.getMessage(), nre);
			reportException(hnre);

			// rethrow CORBA specific
			throw new NO_RESOURCES(nre.getMessage());
		}
		catch (AcsJCannotGetComponentEx cgce)
		{
			reportException(cgce);

			// rethrow CORBA specific
			throw cgce.toCannotGetComponentEx();
		}
		catch (Throwable ex)
		{
			CoreException hce = new CoreException(ex.getMessage(), ex);
			reportException(hce);

			// rethrow CORBA specific
			throw new UNKNOWN(ex.getMessage());
		}
		finally
		{
			pendingRequests.decrementAndGet();
		}
	}


	
	/**
	 * Activation of an co-deployed component.
	 * @param	id 	identification of the caller.
	 * @param	c	component to be obtained.
	 * @param	mark_as_default	mark component as default component of its type.
	 * @param	target_component	target co-deployed component.
	 * @return	<code>ComponentInfo</code> of requested co-deployed component.
	 */
	public ComponentInfo get_collocated_component(int id, si.ijs.maci.ComponentSpec c,
			boolean mark_as_default, String target_component)
	    throws NoPermissionEx, IncompleteComponentSpecEx,
	           InvalidComponentSpecEx,
	           ComponentSpecIncompatibleWithActiveComponentEx, 
	           CannotGetComponentEx
	{
		pendingRequests.incrementAndGet();

		try
		{
			// returned value
			ComponentInfo retVal = null;

			/*
			URI uri = null;
			if (c.component_name != null)
				uri = CURLHelper.createURI(c.component_name);
			ComponentSpec componentSpec = new ComponentSpec(uri, c.component_type, c.component_code, c.container_name);
			*/
			URI targetComponentURI = null;
			if (target_component != null)
				targetComponentURI = CURLHelper.createURI(target_component);

			/// @TODO si.ijs.maci.COMPONENT_SPEC_ANY -> ComponentSpec.COMPSPEC_ANY
			ComponentSpec componentSpec = new ComponentSpec(c.component_name, c.component_type, c.component_code, c.container_name);
			com.cosylab.acs.maci.ComponentInfo info = manager.getCollocatedComponent(id, componentSpec, mark_as_default, targetComponentURI);

			// transform to CORBA specific
			if (info == null || info.getComponent() == null)
				throw new AcsJCannotGetComponentEx();

			Object obj = null;
			obj = (Object)info.getComponent().getObject();
			String[] interfaces;
			if (info.getInterfaces() != null)
				interfaces = info.getInterfaces();
			else
				interfaces = new String[0];
				
			retVal = new ComponentInfo(info.getType(),
									   info.getCode(),
										obj,
										info.getName(),
										info.getClients().toArray(),
										info.getContainer(),
										info.getContainerName(),
										info.getHandle(),
										mapAccessRights(info.getAccessRights()),
										interfaces);

			return retVal;

		}
		catch (URISyntaxException usi)
		{
			BadParametersException hbpe = new BadParametersException(usi.getMessage(), usi);
			reportException(hbpe);
	
			// rethrow CORBA specific
			throw new BAD_PARAM(usi.getMessage());
		}
		catch (AcsJInvalidComponentSpecEx ics)
		{
			//reportException(ics);
			
			// rethrow CORBA specific
			throw ics.toInvalidComponentSpecEx();
		}
		catch (AcsJIncompleteComponentSpecEx ics)
		{
			//reportException(ics);
			
			// rethrow CORBA specific
			throw ics.toIncompleteComponentSpecEx();
		}
		catch (AcsJComponentSpecIncompatibleWithActiveComponentEx cpiwac)
		{
			//reportException(cpiwac);
			
			// rethrow CORBA specific
			throw cpiwac.toComponentSpecIncompatibleWithActiveComponentEx();
		}
		catch (AcsJNoPermissionEx npe)
		{
			//reportException(npe);
			
			// rethrow CORBA specific
			throw npe.toNoPermissionEx();
		}
		catch (BadParametersException bpe)
		{
			BadParametersException hbpe = new BadParametersException(bpe.getMessage(), bpe);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(bpe.getMessage());
		}
		catch (NoResourcesException nre)
		{
			NoResourcesException hnre = new NoResourcesException(nre.getMessage(), nre);
			reportException(hnre);

			// rethrow CORBA specific
			throw new NO_RESOURCES(nre.getMessage());
		}
		catch (AcsJCannotGetComponentEx cgce)
		{
			//reportException(cgce);

			// rethrow CORBA specific
			throw cgce.toCannotGetComponentEx();
		}
		catch (Throwable ex)
		{
			CoreException hce = new CoreException(ex.getMessage(), ex);
			reportException(hce);

			// rethrow CORBA specific
			throw new UNKNOWN(ex.getMessage());
		}
		finally
		{
			pendingRequests.decrementAndGet();
		}
	}

	/**
	 * Get a service, activating it if necessary (components).
	 * The client represented by id (the handle) must have adequate access rights to access the service.
	 * NOTE: a component is also a service, i.e. a service activated by a container.
	 * 
	 * @param id Identification of the caller. If this is an invalid handle, or if the caller does not have enough access rights, a maciErrType::NoPermissionEx exception is raised.
	 * @param service_url CURL of the service whose reference is to be retrieved.
	 * @param activate True if the component is to be activated in case it does not exist. If set to False, and the Component does not exist, a nil reference is returned and status is set to COMPONENT_NOT_ACTIVATED.
	 * @param status Status of the request. One of COMPONENT_ACTIVATED, COMPONENT_DOES_NO_EXIST and COMPONENT_NOT_ACTIVATED.
	 * @return Reference to the service. If the service could not be obtained, a nil reference is returned,
	 *		  and the status contains an error code detailing the cause of failure (one of the COMPONENT_* constants).
	 * @see #get_component
	 */
	public Object get_service(int id, String service_url, boolean activate)
		throws NoPermissionEx, CannotGetComponentEx
	{
		pendingRequests.incrementAndGet();
		try
		{
			// returned value
			Object retVal = null;

			// returned status
			StatusHolder statusHolder = new StatusHolder();

			// transform to CORBA specific
			URI uri = null;
			if (service_url != null)
				uri = CURLHelper.createURI(service_url);
			Component component = manager.getService(id, uri, activate, statusHolder);

			if (component == null || (Object)component.getObject() == null)
				throw new AcsJCannotGetComponentEx();

 	        retVal = (Object)component.getObject();
            
			return retVal;
		}		
		catch (URISyntaxException usi)
		{
			BadParametersException hbpe = new BadParametersException(usi.getMessage(), usi);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(usi.getMessage());
		}
		catch (BadParametersException bpe)
		{
			BadParametersException hbpe = new BadParametersException(bpe.getMessage(), bpe);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(bpe.getMessage());
		}
		catch (NoResourcesException nre)
		{
			NoResourcesException hnre = new NoResourcesException(nre.getMessage(), nre);
			reportException(hnre);

			// rethrow CORBA specific
			throw new NO_RESOURCES(nre.getMessage());
		}
		catch (AcsJCannotGetComponentEx cgce)
		{
			//reportException(cgce);

			// rethrow CORBA specific
			throw cgce.toCannotGetComponentEx();
		}
		catch (AcsJNoPermissionEx npe)
		{
			//reportException(npe);
			
			// rethrow CORBA specific
			throw npe.toNoPermissionEx();
		}
		catch (Throwable ex)
		{
			CoreException hce = new CoreException(ex.getMessage(), ex);
			reportException(hce);

			// rethrow CORBA specific
			throw new UNKNOWN(ex.getMessage());
		}
		finally
		{
			pendingRequests.decrementAndGet();
		}
	}


	/**
	 * Restarts an component.
	 * @param	id 	identification of the caller. Called has to be an owner of the component.
	 * @param	component_url	CURL of the component to be restarted.
	 * @return	CORBA reference of the restarted component, <code>null</code> if it fails.
	 */
	public Object restart_component(int id, String component_url)
		throws NoPermissionEx
	{
		pendingRequests.incrementAndGet();
		try
		{
			// returned value
			Object retVal = null;

			// transform to CORBA specific
			URI uri = null;
			if (component_url != null)
				uri = CURLHelper.createURI(component_url);
			Component component = manager.restartComponent(id, uri);

			// extract component CORBA reference
			if (component != null)
				retVal = (Object)component.getObject();

			return retVal;
		}
		catch (URISyntaxException usi)
		{
			BadParametersException hbpe = new BadParametersException(usi.getMessage(), usi);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(usi.getMessage());
		}
		catch (BadParametersException bpe)
		{
			BadParametersException hbpe = new BadParametersException(bpe.getMessage(), bpe);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(bpe.getMessage());
		}
		catch (NoResourcesException nre)
		{
			NoResourcesException hnre = new NoResourcesException(nre.getMessage(), nre);
			reportException(hnre);

			// rethrow CORBA specific
			throw new NO_RESOURCES(nre.getMessage());
		}
		catch (AcsJBadParameterEx bpe)
		{
			reportException(bpe);
			
			// rethrow CORBA specific
			//throw bpe.toBadParameterEx();
			throw new BAD_PARAM(bpe.getMessage());
		}
		catch (AcsJNoPermissionEx npe)
		{
			//reportException(npe);
			
			// rethrow CORBA specific
			throw npe.toNoPermissionEx();
		}
		catch (Throwable ex)
		{
			CoreException hce = new CoreException(ex.getMessage(), ex);
			reportException(hce);

			// rethrow CORBA specific
			throw new UNKNOWN(ex.getMessage());
		}
		finally
		{
			pendingRequests.decrementAndGet();
		}
	}

	
	/**
	 * Shutdown a container.
	 * 
	 * @param	id 	identification of the caller. Called has to be an owner of the component.
	 * @param container_name	name of the container to shutdown.
	 * @param	action	The code to send to shutdown method of the container. If <code>0</code>, the Container's disconnect methods is called instead.
	 */
	public void shutdown_container(int id, String container_name, int action)
		throws NoPermissionEx
	{
		pendingRequests.incrementAndGet();
		try
		{
			// simply shutdown the container
			manager.shutdownContainer(id, container_name, action);
		}
		catch (BadParametersException bpe)
		{
			BadParametersException hbpe = new BadParametersException(bpe.getMessage(), bpe);
			reportException(hbpe);

			// rethrow CORBA specific
			throw new BAD_PARAM(bpe.getMessage());
		}
		catch (NoResourcesException nre)
		{
			NoResourcesException hnre = new NoResourcesException(nre.getMessage(), nre);
			reportException(hnre);

			// rethrow CORBA specific
			throw new NO_RESOURCES(nre.getMessage());
		}
		catch (AcsJNoPermissionEx npe)
		{
			//reportException(npe);
			
			// rethrow CORBA specific
			throw npe.toNoPermissionEx();
		}
		catch (Throwable ex)
		{
			CoreException hce = new CoreException(ex.getMessage(), ex);
			reportException(hce);

			// rethrow CORBA specific
			throw new UNKNOWN(ex.getMessage());
		}
		finally
		{
			pendingRequests.decrementAndGet();
		}

	}
	
    /* ************************ LoggingConfigurable ************************ */

	/**
	 * Gets the log levels of the default logging configuration. These levels
	 * are used by all loggers that have not been configured individually.
	 */
	public LogLevels get_default_logLevels() {
		pendingRequests.incrementAndGet();
		try {
			LogConfig logConfig = ClientLogManager.getAcsLogManager().getLogConfig();
			
			LogLevels logLevels = new LogLevels();
			logLevels.useDefault = false;
			logLevels.minLogLevel = (short) logConfig.getDefaultMinLogLevel().value;
			logLevels.minLogLevelLocal = (short) logConfig.getDefaultMinLogLevelLocal().value;
			return logLevels;
		}
		finally {
			pendingRequests.decrementAndGet();
		}
	}

	/**
	 * Sets the log levels of the default logging configuration. These levels
	 * are used by all loggers that have not been configured individually.
	 */
	public void set_default_logLevels(LogLevels levels) throws IllegalArgumentEx {
		pendingRequests.incrementAndGet();
		LogConfig logConfig = ClientLogManager.getAcsLogManager().getLogConfig();
		
		try {
			logConfig.setDefaultMinLogLevel(AcsLogLevelDefinition.fromInteger(levels.minLogLevel));
			logConfig.setDefaultMinLogLevelLocal(AcsLogLevelDefinition.fromInteger(levels.minLogLevelLocal));
		} catch (AcsJIllegalArgumentEx ex) {
			throw ex.toIllegalArgumentEx();
		}
		finally {
			pendingRequests.decrementAndGet();
		}
	}

	/**
	 * Gets the names of all loggers, to allow configuring their levels
	 * individually. The names are those that appear in the log records in the
	 * field "SourceObject". This includes the container logger, ORB logger,
	 * component loggers, and (only C++) GlobalLogger.
	 */
	public String[] get_logger_names() {
		pendingRequests.incrementAndGet();
		try {
			LogConfig logConfig = ClientLogManager.getAcsLogManager().getLogConfig();
			
			Set<String> loggerNames = logConfig.getLoggerNames();
			return loggerNames.toArray(new String[loggerNames.size()]);
		}
		finally {
			pendingRequests.decrementAndGet();
		}
	}

	/**
	 * Gets log levels for a particular named logger. If the returned field
	 * LogLevels.useDefault is true, then the logger uses the default levels,
	 * see get_default_logLevels(); otherwise the returned local and remote
	 * levels apply.
	 */
	public LogLevels get_logLevels(String logger_name) {
		pendingRequests.incrementAndGet();
		try {
			LogConfig logConfig = ClientLogManager.getAcsLogManager().getLogConfig();
			
			UnnamedLogger levels = logConfig.getNamedLoggerConfig(logger_name);
			boolean useDefault = !logConfig.hasCustomConfig(logger_name); 
			LogLevels ret = AcsLogLevelDefinition.createIdlLogLevelsFromXsd(useDefault, levels);
			return ret;
		}
		finally {
			pendingRequests.decrementAndGet();
		}
	}

	/**
	 * Sets log levels for a particular named logger. If levels.useDefault is
	 * true, then the logger will be reset to using default levels; otherwise it
	 * will use the supplied local and remote levels.
	 */
	public void set_logLevels(String logger_name, LogLevels levels) throws IllegalArgumentEx {
		pendingRequests.incrementAndGet();
		try {
			LogConfig logConfig = ClientLogManager.getAcsLogManager().getLogConfig();
	
			if (levels.useDefault) {
				logConfig.clearNamedLoggerConfig(logger_name);
			}
			else {
				try {
					UnnamedLogger config = AcsLogLevelDefinition.createXsdLogLevelsFromIdl(levels);
					logConfig.setNamedLoggerConfig(logger_name, config);
				} catch (AcsJIllegalArgumentEx ex) {
					throw ex.toIllegalArgumentEx();
				}
			}
		}
		finally {
			pendingRequests.decrementAndGet();
		}
	}

	/**
	 * Commands the container or manager to read in again the logging
	 * configuration from the CDB and to reconfigure the loggers accordingly.
	 * This allows for persistent changes in the logging configuration to become
	 * effective, and also for changes of more advanced parameters.
	 */
	public void refresh_logging_config() {
		pendingRequests.incrementAndGet();
		try {
			LogConfig logConfig = ClientLogManager.getAcsLogManager().getLogConfig();
			
			try {
				logConfig.initialize(true);
			} catch (LogConfigException ex) {
				// if the CDB can't be read, we still want to run the container, thus we only log the problem here
				logger.log(Level.FINE, "Failed to configure logging (default values will be used).", ex);
			}
		}
		finally {
			pendingRequests.decrementAndGet();
		}
	}

	/* ************************ END LoggingConfigurable ************************ */

 	/* *************************************************************************** */
	/* ************************ [ Mapping methods ] ****************************** */
	/* *************************************************************************** */

	/**
	 * Map <code>AccessRights</code> status codes to CORBA specific.
	 *
	 * @param	accessRights	access rights as defined in <code>AccessRights</code>
	 * @return	CORBA specific access rights code.
	 */
	public static int mapAccessRights(int accessRights)
	{
		int retVal = 0;
		
		if ((accessRights & AccessRights.INTROSPECT_MANAGER) == AccessRights.INTROSPECT_MANAGER)
			retVal |= si.ijs.maci.AccessRights.INTROSPECT_MANAGER.value;
			
		if ((accessRights & AccessRights.REGISTER_COMPONENT) == AccessRights.REGISTER_COMPONENT)
			retVal |= si.ijs.maci.AccessRights.REGISTER_COMPONENT.value;
			
		if ((accessRights & AccessRights.SHUTDOWN_SYSTEM) == AccessRights.SHUTDOWN_SYSTEM)
			retVal |= si.ijs.maci.AccessRights.SHUTDOWN_SYSTEM.value;

		return retVal;
	}

	/**
	 * Returns number of pending requests.
	 * @return	number of pending requests.
	 */
	public int getNumberOfPendingRequests()
	{
		return pendingRequests.get();
	}

	/**
	 * Returns a single-line rendition of this instance into text.
	 *
	 * @return internal state of this instance
	 */
	public String toString()
	{
		StringBuffer sbuff = new StringBuffer();
		sbuff.append("ManagerProxyImpl = { ");
		sbuff.append("manager = '");
		sbuff.append(manager);
		sbuff.append("' }");
		return new String(sbuff);
	}

}

