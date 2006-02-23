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

import java.util.logging.Logger;

import org.omg.PortableServer.Servant;

import si.ijs.maci.ComponentSpec;

import com.cosylab.CDB.DAL;

import edu.emory.mathcs.backport.java.util.concurrent.ThreadFactory;

import alma.ACS.OffShoot;
import alma.acs.component.ComponentDescriptor;
import alma.acs.component.ComponentQueryDescriptor;
import alma.acs.component.ComponentStateManager;
import alma.entities.commonentity.EntityT;

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
public interface ContainerServices 
{
	/////////////////////////////////////////////////////////////
	// basic needs
	/////////////////////////////////////////////////////////////
		
	/**
	 * Delivers the unique instance name for the component.
	 * The name must have been specified at deployment time in the CDB
	 * (for static components like the famous "LAMP1" example),
	 * or must have been given to the manager at runtime by the client who creates
	 * a dynamic component (or was invented by the manager if none was given). 
	 * @return the unique component instance name
	 */
	public String getName();

        
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


	/**
	 * Gets a <code>Logger</code> object that the component should use for logging.
	 * <p>
	 * The <code>Logger</code> will be set up with a namespace specific to the component
	 * that uses this <code>ContainerServices</code> instance.  
	 *   
	 * @return Logger
	 */
	public Logger getLogger();
	
	
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
	 * @throws ContainerException  if something goes wrong.
	 */
	public org.omg.CORBA.Object getComponent(String componentUrl) 
			throws ContainerException;
	
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
	 * @throws ContainerException 
	 */
	public org.omg.CORBA.Object getDefaultComponent(String componentIDLType) 
			throws ContainerException;
	

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
			throws ContainerException;

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
			throws ContainerException;


	
	/**
	 * Convenience method for accessing the Configuration Database.
	 * <p>
	 * Currently more than a convenience, given that the CDB is not yet a Java component, 
	 * but a separate service...
	 * 
	 * @return  the CDB interface
	 * @throws ContainerException
	 */
	public DAL getCDB() throws ContainerException;
	
	
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
		throws ContainerException;


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
		throws ContainerException;
	
	
	/**
	 * Releases the specified component. This involves notification of the manager,
	 * as well as calling <code>_release()</code> on the CORBA stub for that component.
	 * If the curl is not known to the container, the request will be ignored.
	 * @param componentUrl  the name/curl of the component instance as used by the manager  
	 */
	public void releaseComponent(String componentUrl);
	
	
	/////////////////////////////////////////////////////////////
	// advanced stuff, only for special cases
	/////////////////////////////////////////////////////////////
		
    
	/**
	 * Activates a CORBA servant that implements alma.ACS.OffShoot.
	 * The purpose of the OffShoot marker interface is to crack down on uncontrolled
	 * activation of just any CORBA services by Java components, 
	 * while allowing this for selected subtypes of OffShoot, like 
	 * {@link alma.ACS.Callback}.
	 * <p>
	 * The OffShoot servant can be either a subclass of the xyzPOA skeleton, or a xyzPOATie instance
	 * (delegation model).
	 * Since ACS 4.1.2, a tie servant is detected, and interception code gets inserted between the 
	 * POATie skeleton and the offshoot implementation class. This way, the container
	 * can intercept (and log) calls to offshoots in the same way as it does for calls to components.
	 * <b>It is therefore recommended to use the tie approach for all offshoot servants, 
	 * unless there is a reason to avoid container interception.</b>
	 * 
	 * @param cbServant  the CORBA-generated servant, e.g. CBdoublePOA; 
	 * 						 must implement <code>alma.ACS.OffShootOperations</code>.
	 * @return  needs a narrow-cast to the subtype, like
	 * 			<code>CBdouble myCBdouble = alma.ACS.CBdoubleHelper.narrow(...)</code>.  
	 * @throws ContainerException  if anything goes wrong, 
	 * 								especially if <code>cbServant</code> is not an OffShoot.
	 */
	public OffShoot activateOffShoot(Servant cbServant) 
			throws ContainerException;
	
	/**
	 * Deactivates the offshoot corba object.
	 * Caution: this method returns immediately, while the underlying 
	 * {@link org.omg.PortableServer.POAOperations#deactivate_object(byte[])} still
	 * works on the deactivation. If {@link #activateOffShoot(Servant)} is called too shortly
	 * after deactivation, an exception will be thrown. TODO: find a remedy
	 * 
	 * @param cbServant  must implement {@link alma.ACS.OffShootOperations}.
	 * @throws ContainerException if something goes wrong, e.g., if the offshoot servant was not active.
	 */
	public void deactivateOffShoot(Servant cbServant) 
			throws ContainerException; 
	

    /**
     * More specialized methods are available from the <code>AdvancedContainerServices</code>. 
     */
    public AdvancedContainerServices getAdvancedContainerServices();
    

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
	public void assignUniqueEntityId(EntityT entity) throws ContainerException;
	
	
	/**
	 * Converts a "flat-XML" component interface 
	 * (as obtained from the various <code>getComponent</code> methods)
	 * to a "transparent-XML" component interface.
	 * This is only applicable to components that contain XML entities
	 * in their IDL interface methods, and for which the build process
	 * has been set up to generate XML binding classes and the "transparent-XML" interface
	 * in addition to the standard Java-IDL compiler output.
	 * <p>
	 * The container can fulfill this request in two different ways:
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
	 * @param transparentXmlIF  component interface with XML binding classes.
	 * @param componentReference  reference to the component to be wrapped, as 
	 * 								obtained through <code>getComponent(String)</code>, 
	 * 								thus implements <code>flatXmlIF</code>. 
	 * @param flatXmlIF  component interface where entity objects are represented as 
	 * 					serialized XML inside a CORBA struct.
	 * @return the object that implements <code>transparentXmlIF</code>.
	 */
	public Object getTransparentXmlComponent(
		Class transparentXmlIF,
		org.omg.CORBA.Object componentReference,
		Class flatXmlIF) 
		throws ContainerException;

    
    /////////////////////////////////////////////////////////////
    // other
    /////////////////////////////////////////////////////////////
    
    /**
     * Gets a <code>ThreadFactory</code>, to be used as input for other classes from the concurrent library 
     * (such as <code>ThreadPoolExecutor</code>), or simply to create new <code>Thread</code>s.
     * <p>
     * All user-created threads should come from the factory returned here, so "<code>new Thread(...)</code>" should
     * not appear anywhere in component code.
     * <p>
     * The returned thread factory creates new threads configured to run well in the container environment, e.g. 
     * <ul>
     * <li> all threads are <em>daeemon</em> threads so that they can't interfere with container shutdown.
     * <li> the priority is the same as that of the calling component thread (which is created by the ORB)
     * <li> uncaught user exceptions are logged as WARNING before the thread terminates.
     * <li> when a component is removed from the container, surviving user threads created by that component
     *      are killed using {@link Thread#stop()} (which even though deprecated, seems like the perfect choice 
     *      in this particular situation). 
     *      Otherwise a faulty components could permanently take away resources from a running container 
     *      and from the other components. 
     * </ul> 
     * @return the thread factory to be used by the component to create new threads.
     */
    public ThreadFactory getThreadFactory();
}
