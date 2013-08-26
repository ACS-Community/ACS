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

/**
 * Marker interface to allow a component to declare that it is stateless and can be restarted 
 * on the same machine without surprising effects for its clients.
 * <p>
 * Note that restarting a component on a different machine will always require cooperation from the client,
 * which must evaluate the data received through
 * {@link ContainerServices#registerComponentListener(alma.acs.container.ContainerServices.ComponentListener)}.
 * <p>
 * All component developers are encouraged to revisit their components and change the interface
 * from <code>ComponentLifecycle</code> to <code>StatelessComponentLifecycle</code> if applicable.
 * Doing this can improve the flexibility with which we can restart or relocate component instances
 * in a life system. 
 * 
 * @author hsommer
 * @since ACS 7.0.1
 */
public interface StatelessComponentLifecycle extends ComponentLifecycle
{
	// nothing in addition to its base class.
}
