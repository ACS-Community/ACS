/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
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
package alma.acs.genfw.sm;

import java.util.Iterator;

import org.openarchitectureware.core.constraint.DesignError;
import org.openarchitectureware.meta.uml.state.State;
import org.openarchitectureware.meta.uml.state.Transition;


/**
 * Custom behavior common to AcsSimpleState and AcsCompositeState.
 * <p>
 * With multiple inheritance, these classes would extend their non-abstract framework state classes
 * as well as the framework state superclass {@link State}; 
 * this can't be, so we fake the latter using delegation. 
 * 
 * @author Heiko Sommer
 */
public class AcsStateDelegate extends State
{
	public static boolean isToplevelState(State thisState) 
	{
		return ( thisState.Container().Container() == null );
	}
	
	/**
	 * Asserts that no state can have more than one completion transition.
	 * This must be so at least until Guards are supported.
	 * todo: check uniqueness of transitions in general (no 2 trans. per event)
	 * 
	 * @param thisState
	 * @throws DesignError
	 */
	static void checkCompletionTransition(State thisState) throws DesignError
	{
		Transition complTrans = null;
		for (Iterator transitionIter = thisState.OutTransition().iterator(); transitionIter.hasNext();) {
			Transition tr = (Transition) transitionIter.next();
			if (!tr.hasTrigger()) {
				if (complTrans == null) {
					complTrans = tr;					
				}
				else {
					throw new DesignError("state '" + thisState.Name() + "' has more than one completion transitions.");
				}
			}
		}
	}
	
	public static Transition getCompletionTransition(State thisState) throws DesignError
	{
		Transition complTrans = null;
		for (Iterator transitionIter = thisState.OutTransition().iterator(); transitionIter.hasNext();) {
			Transition tr = (Transition) transitionIter.next();
			if (!tr.hasTrigger()) {
				complTrans = tr;
				break;
			}
		}
		if (complTrans == null) {
			throw new DesignError("state '" + thisState.Name() + "' is supposed to have a completion transition!");
		}
		return complTrans;
	}
	
}
