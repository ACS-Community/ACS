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
package alma.acs.nc.sm.generic;

import java.util.Collection;

import org.apache.commons.logging.Log;
import org.apache.commons.scxml.ErrorReporter;
import org.apache.commons.scxml.EventDispatcher;
import org.apache.commons.scxml.SCInstance;
import org.apache.commons.scxml.SCXMLExpressionException;
import org.apache.commons.scxml.model.Action;
import org.apache.commons.scxml.model.ModelException;
import org.apache.commons.scxml.semantics.ErrorConstants;

/**
 * Action implementation class to be instantiated by the SCXML framework.
 * Forwards the execute call to the user-supplied dispatcher,
 * from where it goes to user-instantiated action impl objects.
 * <p>
 * The idea is to overcome Apache SCXML's limitation that action handlers 
 * cannot be injected, but instead get created by the framwork.
 * This allows flexible implementation of one or many actions in one class,
 * the use of normal JDK/ACS loggers in the action code, 
 * and flexibility in how the action objects can delegate among themselves
 * and to other objects.
 * 
 * @author hsommer
 */
public class AcsScxmlDispatchingAction<E extends Enum<E>> extends Action
{
	
	/** Serial version UID. */
	private static final long serialVersionUID = 1L;

	/**
	 * Set by the framework right after construction, via {@link #setName(String)}.
	 * This mechanism is based on beanutils, working with SCXML elements such as 
	 * &lt;customActionDomain:createEnvironment name="createEnvironment"/&gt;
	 * that have a name attribute.
	 */
	private String actionName;
	
	
	/**
	 * The enum version of {@link #actionName}. 
	 * Only available after the first call to {@link #getActionDispatcher(SCInstance)}
	 * because we need the concrete enum class for the String-enum conversion,
	 * which is only available through the user-supplied dispatcher.
	 */
	private E action;
	
	/**
	 * Cache, see {@link #getActionDispatcher(SCInstance)}.
	 */
	private volatile AcsScxmlActionDispatcher<E> disp;


	/**
	 * Called by the SCXML framework
	 */
	public AcsScxmlDispatchingAction() {
	}

	/**
	 * Gets the action name.
	 * 
	 * @return Returns the name.
	 */
	public String getName() {
		return actionName;
	}

	/**
	 * Sets the name. Stores it in {@link #actionName}.
	 */
	public void setName(String name) {
		actionName = name;
	}

//	@SuppressWarnings("rawtypes")
	public void execute(final EventDispatcher evtDispatcher, final ErrorReporter errRep, final SCInstance scInstance,
			final Log appLog, final Collection derivedEvents) throws ModelException, SCXMLExpressionException {
		
		try {
			getActionDispatcher(scInstance);
		} catch (Exception ex) {
			errRep.onError(ErrorConstants.UNDEFINED_VARIABLE, "Failed to get UserActionDispatcher object from root context.", ex);
		}

		if (disp != null && action != null) {
			// forward the action call to the dispatcher
			disp.execute(action, evtDispatcher, errRep, scInstance, derivedEvents);
		}
		else {
			errRep.onError(ErrorConstants.UNDEFINED_VARIABLE, "Failed to get UserActionDispatcher object from root context.", null);
		}
	}

	/**
	 * Retrieves the user-supplied action dispatcher from the sm root context and
	 * caches it in {@link #disp}. 
	 * @param scInstance
	 */
	private void getActionDispatcher(SCInstance scInstance) {
		if (disp == null) {
			synchronized (this) {
				if (disp == null) {
					disp = (AcsScxmlActionDispatcher<E>) scInstance.getRootContext().get(AcsScxmlActionDispatcher.class.getName());
					// cache the action enum
					action = Enum.valueOf(disp.getActionType(), this.actionName);
				}
			}
		}
	}
}
