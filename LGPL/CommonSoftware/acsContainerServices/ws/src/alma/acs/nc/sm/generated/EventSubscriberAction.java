/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.nc.sm.generated;

import alma.acs.nc.sm.generic.AcsScxmlDispatchingAction;

/**
 * All SM actions get mapped to the generic {@link AcsScxmlDispatchingAction}
 * that is parametrized with an action enum such as this one. 
 * <p>
 * Note that the SM framework mandates that all actions can be listed in an
 * enum class. 
 * However, the action implementation can be done in one or many classes. 
 * {@link EventSubscriberAllActionsHandler} is useful in case you want to implement all actions
 * in one class. 
 * <p>
 * This enum class replaces the config file config/SMActionMap.txt used in the ESO SM framework, 
 * together with the option of setting arbitrary action handlers.
 * 
 * @author hsommer
 */
public enum EventSubscriberAction {
	createEnvironment,
	createConnection,
	suspendConnection,
	resumeConnection,
	destroyConnection,
	destroyEnvironment
}
