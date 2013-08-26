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
package alma.acs.component;

import alma.acs.container.ContainerServices;
import alma.maciErrType.wrappers.AcsJComponentCleanUpEx;

/**
 * Interface that every component has to implement.
 * <p>
 * The methods are used by the container to start and stop the component, 
 * that is, to manage its lifecycle.
 * <p>
 * Note that the constructor of the component implementation should be
 * very small, preferably empty. Use the methods {@link #initialize(ContainerServices)}
 * and {@link #execute()} instead.
 * <p>
 * TODOs:
 * <ul>
 * <li> we still have to think about the exception mechanism.
 * <li> new methods for more advanced behavior, e.g. restarting a component transparently for its client, 
 *    will be added later. Their definition requires more analysis, and their implementation will also
 * 	require changes to the ACS Manager etc.
 * </ul>
 * @author hsommer
 */
public interface ComponentLifecycle
{
    /**
     * Called to give the component time to initialize itself. 
     * For instance, the component could retrieve connections, read in 
     * configuration files/parameters, build up in-memory tables, ...
     * <p>
     * Called before {@link #execute()}.
     * In fact, this method might be called quite some time before functional requests 
     * can be sent to the component.
     * <p>
     * Must be implemented as a synchronous (blocking) call.
     * 
     * @param containerServices  callback object for services provided by the container
     */
	public void initialize(ContainerServices containerServices) throws ComponentLifecycleException;


    /**
     * Called after {@link ComponentLifecycle#initialize(ContainerServices)} to tell the component that it has to be ready to accept 
     * incoming functional calls any time. 
     * <p>
     * Examples:
     * <ul>
     * <li>last-minute initializations for which <code>initialize</code> seemed too early
     * <li>component could start actions which aren't triggered by any functional call, 
     * 	 e.g. the Scheduler could start to rank SBs in a separate thread.
     * </ul>
     * <p>
     * Must be implemented as a synchronous (blocking) call (can spawn threads though).
     */
    public void execute() throws ComponentLifecycleException; 


    /**
     * Called after the last functional call to the component has finished.
     * The component should then orderly release resources etc.
     */
    public void cleanUp() throws AcsJComponentCleanUpEx;


    /**
     * Called when due to some error condition the component is about to be forcefully removed
     * some unknown amount of time later (usually not very much...).
     * <p>
     * The component should make an effort to die as neatly as possible.
     * <p>
     * Because of its urgency, this method will be called asynchronously to the execution of 
     * any other method of the component.
     */
    public void aboutToAbort();


}
