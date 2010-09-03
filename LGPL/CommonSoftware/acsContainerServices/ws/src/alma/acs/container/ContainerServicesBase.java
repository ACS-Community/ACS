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

import java.util.concurrent.ThreadFactory;

import org.omg.PortableServer.Servant;

import alma.ACS.OffShoot;
import alma.ACS.OffShootOperations;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.acs.logging.AcsLogger;

import com.cosylab.CDB.DAL;

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
public interface ContainerServicesBase 
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
	 * Gets a <code>Logger</code> object that the component should use for logging.
	 * <p>
	 * The <code>Logger</code> will be set up with a namespace specific to the component
	 * that uses this <code>ContainerServices</code> instance.  
	 * <p>
	 * Specific logger extensions that only apply to certain subsystems can be used
	 * by wrapping this logger with a class such as {@link alma.acs.logging.domainspecific.AntennaContextLogger}. 
	 * 
	 * @return Logger
	 */
	public AcsLogger getLogger();
	
	/**
	 * Convenience method for accessing the Configuration Database.
	 * <p>
	 * Currently more than a convenience, given that the CDB is not yet a Java component, 
	 * but a separate service...
	 * 
	 * @return  the CDB interface
	 * @throws ContainerException
	 */
	public DAL getCDB() throws AcsJContainerServicesEx;
	
	
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
	public <T extends Servant & OffShootOperations> OffShoot activateOffShoot(T cbServant) 
			throws AcsJContainerServicesEx;

	/**
	 * Deactivates the offshoot object previously activated through the {@link #activateOffShoot(Object, Class)} method.
	 * Caution: this method returns immediately, while the underlying 
	 * {@link org.omg.PortableServer.POAOperations#deactivate_object(byte[])} still
	 * works on the deactivation. If {@link #activateOffShoot(Servant)} is called too shortly
	 * after deactivation, an exception will be thrown. TODO: find a remedy
	 * 
	 * @param offshootImpl  the offshoot object implementation
	 * @throws AcsJContainerServicesEx if something goes wrong, e.g., if the corresponding offshoot servant was not active.
	 */
	public void deactivateOffShoot(Object offshootImpl) 
			throws AcsJContainerServicesEx; 

    /**
     * More specialized methods are available from the <code>AdvancedContainerServices</code>. 
     */
    public AdvancedContainerServices getAdvancedContainerServices();
    

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
