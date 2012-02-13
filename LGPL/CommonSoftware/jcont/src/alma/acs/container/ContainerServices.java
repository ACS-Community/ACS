/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.container;

import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.omg.PortableServer.Servant;

import si.ijs.maci.ComponentSpec;

import alma.ACS.OffShoot;
import alma.ACS.OffShootOperations;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.alarmsystem.source.AlarmSource;
import alma.acs.component.ComponentDescriptor;
import alma.acs.component.ComponentQueryDescriptor;
import alma.acs.component.ComponentStateManager;
import alma.entities.commonentity.EntityT;
import alma.maciErrType.wrappers.AcsJComponentDeactivationFailedEx;
import alma.maciErrType.wrappers.AcsJComponentDeactivationUncleanEx;

/**
 * Through this interface, the container offers services to its hosted components.
 * The component must call these methods explicitly; in this respect, 
 * <code>ContainerServices</code> is different from the other services that the container
 * provides without the component implementation knowing about it. 
 * It can be thought of as a callback handle or a library.
 * <p>
 * Currently, methods are added to this interface as the functionality becomes available. 
 * At some point we will have to declutter the interface by introducing 2nd-level interfaces
 * that harbor cohesive functionality. For example, instead of calling 
 * <code>myContainerServices.getComponent(...)</code>, the new call will then be something like
 * <code>myContainerServices.communication().getComponent(...)</code>.
 * <p>
 * created on Oct 24, 2002 12:56:36 PM
 * @author hsommer
 */
public interface ContainerServices extends ContainerServicesBase
{
	/////////////////////////////////////////////////////////////
	// basic needs (the rest comes from the base class)
	/////////////////////////////////////////////////////////////
		
	/**
	 * Delivers the <code>ComponentStateManager</code> object
	 * through which the component and the container administrate the
	 * state of the component.
	 * <p>
	 * The component needs to access the <code>ComponentStateManager</code>
	 * if it wishes to change its state. 
	 * If it doesn't, only the container will change the state based on 
	 * the information it has available.
	 * 
	 * @return the state manager
	 * @see alma.ACS.ComponentStates
	 */
	public ComponentStateManager getComponentStateManager();

	
	/////////////////////////////////////////////////////////////
	// access to other components
	/////////////////////////////////////////////////////////////
		
	/**
	 * Gets a component specified by its instance name.
	 * Delegates to {@link si.ijs.maci.ManagerOperations#get_component(int, java.lang.String, boolean, org.omg.CORBA.IntHolder) get_component}.
	 * <ul>
	 * <li> This method is necessarily generic and will require a cast to the 
	 * 		requested interface, like in <code>HelloComponent helloComp = 
	 * 		HelloComponentHelper.narrow(containerServices.getComponent("HELLOCOMP1"));</code>.
	 * 
	 * <li> the more general method <code>get_service</code> offered by the manager
	 * 		is deliberately not represented here. It would currently (Oct.03) offer
	 * 		access to "LogFactory", "NotifyEventChannelFactory", "ArchivingChannel",
	 * 		"LoggingChannel", "InterfaceRepository", "CDB", "ACSLogSvc", "PDB";
	 * 		It seems that if such access is needed, specialized methods should be added
	 * 		to this interface, like {@link #getCDB()}.
	 * 		
	 * <li> if requested, we may come up with some additional way 
	 * 		(e.g. a code-generated singleton class) 
	 * 		to give type safe access to other components, like done with EJB xxxHome.
	 * 
	 * <li> the Container will later shortcut calls for components inside the same process. 
	 *     Like in EJB and CORBA, the implementation must therefore not assume receiving a 
	 *     by-value copy of any parameter from the other component.
	 * </ul>
	 * 
	 * @param componentUrl  the ACS CURL of the deployed component instance.
	 * @return  the CORBA proxy for the component.
	 * @throws AcsJContainerServicesEx  if something goes wrong.
	 */
	public org.omg.CORBA.Object getComponent(String componentUrl) 
			throws AcsJContainerServicesEx;
	
	/**
	 * Gets a non-sticky reference to a component.
	 * This is typically used by "weak clients" such as graphical user interfaces that only want to observe the running system
	 * without interfering with its functioning.
	 * <p>
	 * A non-sticky reference does not bind the Manager to keep alive the Component, and the Client requesting a non-sticky reference 
	 * is not considered when checking for reference counts.
	 * The non-sticky reference should not be released, as that call will fail.
	 * The Manager can deactivate Components independently of any non-sticky reference.
	 * Since a non-sticky reference is not considered in reference counting, it will also not activate the component if it is 
	 * not already active. As a consequence, asking for a non-sticky reference to a not-active Component throws an exception. 
	 * The client represented by id (the handle) must have adequate access rights to access the component.
	 * @param curl the component URL (component instance name)
	 * @return  the CORBA proxy for the component.
	 * @throws AcsJContainerServicesEx if something goes wrong
	 * @since ACS 6.0
	 */
	public org.omg.CORBA.Object getComponentNonSticky(String curl) 
			throws AcsJContainerServicesEx;


	/**
	 * Gets the default component specified by the component type.
	 * The type is the IDL type, such as <code>IDL:alma/PS/PowerSupply:1.0</code>.
	 * <p>
	 * A default instance for the given type must have been declared beforehand
	 * (either statically in the CDB config files, or dynamically),
	 * otherwise an exception is thrown.
	 * <p>
	 * To get more information on the returned component, call
	 * {@link #getComponentDescriptor} with the instance name that
	 * you can get from the component using {@link alma.ACS.ACSComponentOperations#name}.
	 * <p>
	 * Delegates to {@link si.ijs.maci.ManagerOperations#get_default_component}.
	 * @param componentIDLType 
	 * @return 
	 * @throws AcsJContainerServicesEx 
	 */
	public org.omg.CORBA.Object getDefaultComponent(String componentIDLType) 
			throws AcsJContainerServicesEx;
	

	/**
	 * Gets a component that will run collocated with a given component.
	 * @param compUrl  the component's name (URL)
	 * @param targetCompUrl  the name (URL) of the target component, in whose container we also want <code>compUrl</code> to run.
	 * @return  the component reference, which should be cast using the appropriate CORBA narrow operation. Never null.
	 * @throws AcsJContainerServicesEx If the call failed and no component reference could be obtained.
	 * @since ACS 5.0.3
	 */
	public org.omg.CORBA.Object getCollocatedComponent(String compUrl, String targetCompUrl) throws AcsJContainerServicesEx;
	
	/**
	 * Dynamic version of {@link #getCollocatedComponent(String, String)}.
	 * @param compSpec  the description of the component to be created
	 * @param markAsDefaul  if true, the new component will become the default component for its IDL type.
	 * @param targetCompUrl targetCompUrl  the name (URL) of the target component, in whose container we also want <code>compUrl</code> to run.
	 * @return 
	 * @throws AcsJContainerServicesEx If the call failed and no component reference could be obtained.
	 * @since ACS 6.0.4
	 */
	public org.omg.CORBA.Object getCollocatedComponent(ComponentQueryDescriptor compSpec, boolean markAsDefaul, String targetCompUrl) throws AcsJContainerServicesEx;
	
	
	/**
	 * Gets a component whose instance is not registered in the CDB 
	 * at deployment time.
	 * <p>
	 * The fields of <code>compSpec</code> can be filled in at various levels of detail,
	 * allowing the manager to blend in missing data from static deployment information.
	 * <b>For a detailed description of the various options, 
	 * please refer to the ACS documentation.</b>
	 * <p>
	 * To get more information on the returned component, call
	 * {@link #getComponentDescriptor} with the instance name that
	 * you've specified or that you can get from the component in case it was 
	 * assigned (see {@link alma.ACS.ACSComponentOperations#name()}).
	 * <p>
	 * Delegates to {@link si.ijs.maci.ManagerOperations#get_dynamic_component}.
	 * 
	 * @param compSpec  the description of the component to be created
	 * @param markAsDefault  if true, the new component will become the default component for its IDL type.	
	 */
	public org.omg.CORBA.Object getDynamicComponent(ComponentQueryDescriptor compSpec, boolean markAsDefault) 
			throws AcsJContainerServicesEx;

    /**
	 * More powerful and thus more dangerous version of {@link #getDynamicComponent(ComponentQueryDescriptor, boolean)}
	 * which exposes a CORBA struct directly. 
	 * 
	 * @param compSpec  the description of the component to be created
	 * @param markAsDefault  if true, the new component will become the default component for its IDL type.
	 * @deprecated use {@link #getDynamicComponent(ComponentQueryDescriptor, boolean)} instead. 
	 * 				if you need to set not only the standard fields <code>component_name</code> or <code>component_type</code>,
	 * 				but also the more advanced values <code>component_code</code> or <code>container_name</code>,
	 * 				please send a request to ACS so that access to these fields be included in the 
	 * 				<code>ComponentQueryDescriptor</code> given to the recommended version of this method.    	
	 */
	public org.omg.CORBA.Object getDynamicComponent(ComponentSpec compSpec, boolean markAsDefault) 
			throws AcsJContainerServicesEx;


	
	
	/**
	 * Finds components by their instance name (curl) and/or by their type.
	 * Wildcards can be used for the curl and type.
	 * This method returns a possibly empty array of component curls; 
	 * for each curl, you may then call {@link #getComponent(String) getComponent} 
	 * to obtain the reference.
	 * 
	 * @param curlWildcard (<code>null</code> is understood as "*")
	 * @param typeWildcard (<code>null</code> is understood as "*")
	 * @return the curls of the component(s) that match the search.
	 * @see si.ijs.maci.ManagerOperations#get_component_info(int, int[], java.lang.String, java.lang.String, boolean)
	 */
	public String[] findComponents(String curlWildcard, String typeWildcard)  
		throws AcsJContainerServicesEx;


	/**
	 * Gets the component descriptor which contains meta data for the component.
	 * (Not to be confused with a component descriptor in the EJB sense.)
	 * <p>
	 * To be used primarily after retrieval of a component with
	 * {@link #getDefaultComponent(String) getDefaultComponent} 
	 * or {@link #getDynamicComponent(ComponentQueryDescriptor, boolean) getDynamicComponent},
	 * when some data like the instance name is not known.
	 * The idea behind having this method separately is that in many cases,
	 * components are not interested in this information, and are happier to 
	 * get back from these methods directly the remote reference to another component,
	 * instead of always dealing with a <code>ComponentDescriptor</code>.
	 * 
	 * @param componentUrl  the unique name of a component  
	 * @return the descriptor from which the various data items can be obtained.
	 * @throws ContainerException  if <code>componentUrl</code> is unknown 
	 * 				or anything else goes wrong
	 * @see si.ijs.maci.ComponentInfo	
	 */
	public ComponentDescriptor getComponentDescriptor(String componentUrl)
		throws AcsJContainerServicesEx;
	
	
	/**
	 * Releases the specified component. This involves notification of the manager,
	 * as well as calling <code>_release()</code> on the CORBA stub for that component.
	 * If the curl is not known to the container, the request will be ignored.
	 * <p>
	 * This method will return only when the component has actually been released,
	 * which may take some time in case there are still active requests being processed.
	 * <p>
	 * This method is kept for convenience, providing a specific subset of the functionality that
	 * the more flexible {@link #releaseComponent(String, ComponentReleaseCallback)} offers.
	 * It blocks the client until the component is released or a timeout of 60 seconds has 
	 * occurred, and logs all errors of component release at level DEBUG.
	 * 
	 * @param componentUrl  the name/curl of the component instance as used by the manager
	 */
	public void releaseComponent(String componentUrl);

	
	/**
	 * Callback that can be optionally given to 
	 * {@link ContainerServices#releaseComponent(String, ComponentReleaseCallback)}.
	 * Users may override the methods they need in their subclasses of <code>ComponentReleaseCallback</code>.
	 * <p>
	 * Note that {@link #awaitComponentRelease(long, TimeUnit)} is not a callback method
	 * but should make it easier to synchronize user code execution with component release.
	 * <p>
	 * An instance of <code>ComponentReleaseCallback</code> can be used for only one component release call.
	 */
	public static class ComponentReleaseCallback {
		private CountDownLatch sync = new CountDownLatch(1);
		/**
		 * Called by ACS to release the thread (if any) that blocks on {@link #awaitComponentRelease(long, TimeUnit)}.
		 */
		final void callOver() {
			sync.countDown();
		}
		/**
		 * Called when a CORBA or other communication error occurred.
		 */
		public void errorCommunicationFailure(Throwable thr) {}
		/**
		 * Called when the client cannot legally release the component, e.g. because it no longer holds a reference to it.
		 */
		public void errorNoPermission(String reason) {}
		/**
		 * Called when the component reference has been successfully released.
		 * @param deactivationUncleanEx If the component was deactivated with problems, this exception will be forwarded; otherwise <code>null</code> for clean deactivation.
		 */
		public void componentReleased(AcsJComponentDeactivationUncleanEx deactivationUncleanEx) {}
		/**
		 * Called when the target component deactivation failed.
		 * @param deactivationFailureEx to provide details about the failure.
		 */
		public void errorComponentReleaseFailed(AcsJComponentDeactivationFailedEx deactivationFailureEx) {}
		
		/**
		 * This is not a callback method but a convenience method to "park" the calling thread
		 * until 
		 * <ul>
		 *   <li>The component has been released, or 
		 *   <li>the given timeout has struck, or
		 *   <li>component release failed with an exception.
		 * </ul>
		 * A client that only wants to wait for component release without caring about the details
		 * does not have to subclass <code>ComponentReleaseCallback</code>, 
		 * but can simply call <code>awaitComponentRelease</code>.
		 * The client may in addition override the callback methods though.
		 *  
		 * @param timeout The maximum time to wait for the component release to succeed.
		 * @param unit  The unit of <code>timeout</code>.
		 * @return <code>true</code> if the component was released properly, <code>false</code> if the call returns because of a timeout.
		 *         See {@link CountDownLatch#await(long, TimeUnit)}.
		 * @throws InterruptedException
		 */
		public final boolean awaitComponentRelease(long timeout, TimeUnit unit) throws InterruptedException {
			return sync.await(timeout, unit);
		}
	}
	
	/**
	 * Variant of {@link ComponentReleaseCallback} which logs all errors.
	 */
	public static class ComponentReleaseCallbackWithLogging extends ComponentReleaseCallback {
		private final Logger logger;
		private Level level;
		public ComponentReleaseCallbackWithLogging(Logger logger, Level level) {
			this.logger = logger;
			this.level = level;
		}
		public void errorCommunicationFailure(Throwable thr) {
			logger.log(level, "errorCommunicationFailure: ", thr);
		}
		public void errorNoPermission(String reason) {
			logger.log(level, "errorNoPermission: " + reason);
		}
		public void componentReleased(AcsJComponentDeactivationUncleanEx deactivationUncleanEx) {
			logger.log(level, "componentReleased: ", deactivationUncleanEx);
		}
		public void errorComponentReleaseFailed(AcsJComponentDeactivationFailedEx deactivationFailureEx) {
			logger.log(level, "errorComponentReleaseFailed: ", deactivationFailureEx);
		}
	}
	
	/**
	 * Releases the reference to the specified component.
	 * Releasing a component reference may result in the deactivation of that component, 
	 * if no other clients hold (sticky) references.
	 * <p> 
	 * This call will return as soon as the release request has been delivered to the ACS manager, 
	 * which means that the reference count evaluation and possible component deactivation will happen 
	 * only after returning from this call. 
	 * <p>
	 * If your code must synchronize with the reference evaluation and possible component deactivation in the target container,
	 * or if you are interested in exceptions that may occur during component deactivation, 
	 * then you should supply the optional <code>ComponentRequestCallback</code> object and block execution of your 
	 * calling thread until you receive the callback.
	 * <p>
	 * If an exception (such as no-permission) is thrown during the synchronous first part 
	 * of the underlying call to the manager, then this exception will not be thrown upward by this method
	 * but will instead be reported to the optional callback object, just like any
	 * exception that happens later during the asynchronous part of the component release. 
	 * The idea here is to have either "interested" clients that want to get all exceptions, 
	 * or "easy" clients that do not care about any exceptions, thus do not provide a callback object, 
	 * and also do not want to bother about a try/catch block.
	 *
	 * @param componentUrl the name/curl of the component instance as used by the manager
	 * @param callback may be <code>null</code> if you do not need to wait for component activation or to see the results.
	 *                 An new instance of <code>ComponentReleaseCallback</code> is required for every call.
	 * @since ACS 9.1
	 */
	public void releaseComponent(String componentUrl, ComponentReleaseCallback callback);
	
	
	public static interface ComponentListener {
		/**
		 * Called to find out whether a filter should be applied 
		 * such that only notifications arrive for components to which the caller holds a reference.
		 */
		boolean includeForeignComponents();
		
		/**
		 * Called when components become available
		 */
		void componentsAvailable(List<ComponentDescriptor> comps);
		
		/**
		 * Called when components become unavailable
		 */
		void componentsUnavailable(List<String> compNames); 
	}
	
	/**
	 * Allows a client to register a callback object that gets notified when some  
	 * component(s) in use by the client (= components the client requested previously)
	 * dies or comes back to life (with <code>ComponentListener#includeForeignComponents()==false</code>). 
	 * <p>
	 * If the client wants to get notified even for components that it does not hold a reference to,
	 * then <code>ComponentListener#includeForeignComponents()</code> should return <code>true</code>;
	 * notification about components that this client does not use may be limited though, e.g. 
	 * to components collocated in the same container. 
	 * 
	 * @param listener
	 * @see si.ijs.maci.ClientOperations#components_available(si.ijs.maci.ComponentInfo[])
	 * @since ACS 6.0
	 */
	public void registerComponentListener(ComponentListener listener);
	

	/**
	 * Wraps a component reference (or offshoot reference etc) such that the given timeout is applied on
	 * the client side of calls to this (possibly remote) object. 
	 * If the total call, including de-/serialization and network travel, takes longer than the given timeout,
	 * an <code>org.omg.CORBA.TIMEOUT</code> exception will be thrown.
	 * <p>
	 * This allows us to override the general timeout given at the system level (e.g. orb.properties file in case of jacorb)
	 * and the container-level timeout given in the CDB container configuration.
	 * It is possible to set the timeout to values that are shorter or longer than the default timeout. 
	 * You should chose a timeout value that matches the expected response time, with a large safety margin of
	 *  
	 * <p>
	 * <b>Note that calls to which the specified timeout should apply must be made on the object reference returned from this method, 
	 *    and not on the original object that gets passed to this method! 
	 *    Some corba implementations may apply the timeout to both objects though, or return the original object.</b>
	 * <p>
	 * 
	 * @param corbaRef  Reference to a component or an offshoot as obtained from some of the other container services methods.
	 * @param timeoutSeconds  the custom client side timeout in seconds, to be used for all calls to the given object reference.
	 * @return A new object reference which should be used to make calls with the specified timeout applied.
	 */
	public org.omg.CORBA.Object getReferenceWithCustomClientSideTimeout(org.omg.CORBA.Object originalCorbaRef, double timeoutSeconds)
		throws AcsJContainerServicesEx;
	
	/////////////////////////////////////////////////////////////
	// support for XML entities and binding classes
	/////////////////////////////////////////////////////////////
		
	/**
	 * Creates a unique id and sets it on the given (binding class) entity object.
	 * The id will be redundantly stored in an encrypted format so that later it can be verified that 
	 * the id is indeed the one originally assigned. 
	 * 
	 * @param entity 	must be freshly created and 
	 */
	public void assignUniqueEntityId(EntityT entity) throws AcsJContainerServicesEx;
	
	
	/**
	 * Converts a "flat-XML" remote object (RO) interface 
	 * (as obtained from the various <code>getComponent</code> methods, in the
	 * case of components, or by retrieving offshoots from components,
	 * in the case of offshoots) to a "transparent-XML" RO interface.
	 * This is only applicable to ROs that contain XML entities
	 * in their IDL interface methods, and for which the build process
	 * has been set up to generate XML binding classes and the "transparent-XML"
	 * interface in addition to the standard Java-IDL compiler output.
	 * <p>
	 * In the case that the remote object is a component, the container can
	 * fulfill this request in two different ways:
	 * <ol>
	 * <li><b>Remote component</b>:
	 * 		if the specified component runs in a different container, 
	 * 		a dynamic wrapper object will be created around the provided CORBA stub.
	 * 		The wrapper object will translate between entity object parameters 
	 * 		in the form of serialized XML (as found in <code>flatXmlIF</code>) 
	 * 		and the corresponding Java binding classes (in <code>transparentXmlIF</code>).
	 * <li><b>Collocated component</b>:
	 * 		if the specified component runs in the same container, the container
	 * 		may choose to shortcut the (de-) serialization of XML binding classes.
	 * 		In this case, Java binding classes are transported in memory,
	 * 		and all communication with the other component is done without CORBA. 
	 * </ol>
	 * In either case, the returned <code>Object</code> implements the 
	 * <code>transparentXmlIF</code> interface. 
	 * The client component that calls this method should only cast to that interface,
	 * and does not need to know which of the two transport mechanisms are being used.
	 * 
	 * @param transparentXmlIF remote object interface with XML binding classes.
	 * @param objectReference  reference to the remote object to be wrapped, which
	 *                    implements <code>flatXmlIF</code>. 
	 *                    Currently supported remote objects are ACS Components and OffShoots.
	 * @param flatXmlIF   remote object interface ("xxxOperations") where entity objects are represented as 
	 *                    serialized XML inside a CORBA struct.
	 * @return the object that implements <code>transparentXmlIF</code>.
	 */
	public <T, F> T getTransparentXmlWrapper(
					Class<T> transparentXmlIF, F objectReference, Class<F> flatXmlIF)
			throws AcsJContainerServicesEx;


	/**
	 * Activates an Object as an OffShoot.
	 * <p>
	 * In contrast to the old {@link #activateOffShoot(Servant)} method, which always expects a
	 * Servant (either subclass of xyzPOA skeleton or a xyzPOATie instance), this method is more flexible and 
	 * receives two arguments: 
	 * <ul>
	 *   <li>The object implementing {@link alma.ACS.OffShootOperations}, but without
	 *       restricting it to be a Servant, and </li>
	 *   <li>The operations interface that this object directly implements.</li>
	 * </ul>
	 * This way, we support 
	 * <ul>
	 *   <li>Activation of "normal" OffShoots: <br>
	 *       <code>offshootImpl</code> should be a corba Servant, either a subclass of the
	 *        XyzPOA skeleton, or a XyzPOATie instance (delegation model). <br>
	 *        <code>idlOpInterface</code> should be <code>XyzOperations.class</code>.
	 *   </li>
	 *   <li>OffShoots featuring automatic XML (de-)serialization, similar to what has always been offered for ACS Components. <br>
	 *       <code>offshootImpl</code> should be an object that implements the generated <code>XyzJ</code> interface
	 *       (which uses XML binding classes and extends <code>OffShootOperations</code>). 
	 *       This object is not expected to be a Corba Servant.<br>
	 *        <code>idlOpInterface</code> should be <code>XyzJ.class</code>.
	 * </ul>
	 * Since ACS 9.0, when giving an OffShoot with XML entities (de-)serialization, automatic
	 * interception code is created to wrap the given object, and a XyzPOATie instance is
	 * created automatically and used as the servant.
	 * 
	 * Since ACS 4.1.2, a tie servant is detected, and interception code gets inserted between the 
	 * POATie skeleton and the offshoot implementation class. This way, the container
	 * can intercept (and log) calls to offshoots in the same way as it does for calls to components.
	 * <b>It is therefore recommended to use the tie approach for all offshoot servants, 
	 * unless there is a reason to avoid container interception.</b>
	 * <p>
	 * @param <T> The type of <code>offshootImpl</code>, one of the <code>xyzJ</code> or
	 *        <code>xyzOperations</code> interfaces
	 * @param offshootImpl   the object that implements the OffShoot logic
	 * @param idlOpInterface the IDL operations interface implemented by <code>offshootImpl</code>;
	 *          currently, must be one of <code>xyzOperations</code> or <code>xyzJ</code>
	 * @return  The Corba-activated offshoot object. Needs a narrow-cast to the subtype, like
	 * 			<code>CBdouble myCBdouble = alma.ACS.CBdoubleHelper.narrow(...)</code>. 
	 * @throws AcsJContainerServicesEx  if anything goes wrong, 
	 *            especially if <code>offshootImpl</code> is does not implement {@link alma.ACS.OffShootOperations}.
	 * @since ACS 9.0
	 */
	<T extends OffShootOperations> OffShoot activateOffShoot(T offshootImpl, Class<T> idlOpInterface) 
			throws AcsJContainerServicesEx;

    /////////////////////////////////////////////////////////////
    // Alarm Service API
    /////////////////////////////////////////////////////////////

	/**
	 * Raises the alarm described by the given triplet.
	 * 
	 * Alarms are described with a unique triplet, which is composed by a <code>faultFamily</code>,
	 * a <code>faultMember</code> and a <code>faultCode</code>, and referred to as <em>FaultState</em>.
	 * A particular alarm is mapped by exactly one <em>FaultState</em>, and vice-versa.
	 * <p>
	 * The <code>faultFamily</code> field identifies a set of elements of the same kind, and that present
	 * the same failures (e.g., all power supplies, which could be represented by the "PS" string).
	 * <p>
	 * The <code>faultMember</code> field identifies the particular instance of the given <code>faultFamily</code>
	 * that is currently raising the alarm.
	 * <p>
	 * The <code>faultCode</code> field identifies the particular error that is being informed. Its value must
	 * match the corresponding one stored in the Alarm Service configuration that describes the situation being reported
	 * as faulty.
	 * 
	 * @param faultFamily The alarm's fault family  
	 * @param faultMember The alarm's fault member
	 * @param faultCode The alarm's fault code
	 * @throws AcsJContainerServicesEx if anything goes wrong while raising the alarm
	 *
	 * @deprecated The new {@link #getAlarmSource()} method should be used instead,
	 *  calling {@link AlarmSource#raiseAlarm(String, String, int)} on the returned object
	 */
	public void raiseAlarm(String faultFamily, String faultMember, int faultCode) throws AcsJContainerServicesEx;

	/**
	 * Clears the alarm described by the given triplet.
	 * 
	 * Alarms are described with a unique triplet, which is composed by a <code>faultFamily</code>,
	 * a <code>faultMember</code> and a <code>faultCode</code>, and referred to as <em>FaultState</em>.
	 * A particular alarm is mapped by exactly one <em>FaultState</em>, and vice-versa.
	 * <p>
	 * The <code>faultFamily</code> field identifies a set of elements of the same kind, and that present
	 * the same failures (e.g., all power supplies, which could be represented by the "PS" string).
	 * <p>
	 * The <code>faultMember</code> field identifies the particular instance of the given <code>faultFamily</code>
	 * that is currently clearing the alarm.
	 * <p>
	 * The <code>faultCode</code> field identifies the particular error that is being informed. Its value must
	 * match the corresponding one stored in the Alarm Service configuration that describes the situation being reported
	 * as corrected.
	 * 
	 * @param faultFamily The alarm's fault family
	 * @param faultMember The alarm's fault member
	 * @param faultCode The alarm's fault code
	 * @throws AcsJContainerServicesEx if anything goes wrong while clearing the alarm
	 * 
	 * @deprecated The new {@link #getAlarmSource()} method should be used instead,
	 *  calling {@link AlarmSource#clearAlarm(String, String, int)} on the returned object
	 */
	public void clearAlarm(String faultFamily, String faultMember, int faultCode) throws AcsJContainerServicesEx;


	/**
	 * Returns the {@link AlarmSource} owned by this object. The {@link AlarmSource} object
	 * allows to raise and clear alarms, among other advanced operations
	 *
	 * @return The {@link AlarmSource} owned by this object
	 * @throws AcsJContainerServicesEx if anything goes wrong
	 */
	public AlarmSource getAlarmSource() throws AcsJContainerServicesEx;

    /////////////////////////////////////////////////////////////
    // other
    /////////////////////////////////////////////////////////////

}