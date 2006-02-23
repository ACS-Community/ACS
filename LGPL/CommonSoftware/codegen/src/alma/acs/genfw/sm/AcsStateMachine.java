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

import alma.acs.genfw.UniqueNameModelElementSet;

import de.bmiag.genfw.meta.DesignError;
import de.bmiag.genfw.meta.ElementSet;
import de.bmiag.genfw.meta.state.Action;
import de.bmiag.genfw.meta.state.ActionSequence;
import de.bmiag.genfw.meta.state.CompositeState;
import de.bmiag.genfw.meta.state.Entry;
import de.bmiag.genfw.meta.state.State;
import de.bmiag.genfw.meta.state.StateMachine;
import de.bmiag.genfw.meta.state.StateVertex;
import de.bmiag.genfw.meta.state.Transition;

/**
 * @author Heiko Sommer
 */
public class AcsStateMachine extends StateMachine
{
	public String CheckConstraints() {
		if (!(TopState() instanceof CompositeState)) {
			throw new DesignError("top state must be a CompositeState.");
		}		
		
		return "ok";
	}

	/**
	 * Gets the initial state of this state machine.
	 * @return
	 */
	public State InitialState() {
		State initialState = null;
		CompositeState top = (CompositeState) TopState();
		for (Iterator iter = top.SubVertex().iterator(); iter.hasNext();) {
			StateVertex vertex = (StateVertex) iter.next();
			if (vertex instanceof Entry) {
				try {
					Transition tr = (Transition) vertex.OutTransition().get(0);
					initialState = (State) tr.TargetVertex();
					break;
				}
				catch (Exception e) {
					throw new DesignError("failed to find initial state from 'Entry' vertex" + vertex.Name());
				}
			}
		}
		return initialState;
	}
	
	/**
	 * Gets all actions that are defined in this state machine, including 
	 * actions from inter-state and internal transitions, entry, exit, and do actions.
	 * @return set of {@link Action} objects that does not include {@link ActionsSequence} 
	 * 			objects as these have been resolved into their constituent actions.
	 */
	public ElementSet DeepAction() {
		ElementSet actionSet = new UniqueNameModelElementSet();
		
		ElementSet allStates = ((AcsCompositeState) TopState()).DeepSubState();
		for (Iterator iter = allStates.iterator(); iter.hasNext();) {
			State state = (State) iter.next();
			if (state.hasEntryAction()) {
				actionSet.add(resolveActions(state.EntryAction()));
			}
			if (state.hasExitAction()) {
				actionSet.add(resolveActions(state.ExitAction()));
			}
			if (state.hasDoAction()) {
				actionSet.add(resolveActions(state.DoAction()));
			}
			ElementSet transitions = new ElementSet();
			transitions.add(state.InternalTransition());
			transitions.add(state.OutTransition());
			for (Iterator transIter = transitions.iterator(); transIter.hasNext();) {
				Transition transi = (Transition) transIter.next();
				if (transi.hasAction()) {
					actionSet.add(resolveActions(transi.Action()));
				}
			}
		}
			
		return actionSet;
	}
	
	private ElementSet resolveActions(Action action) {
		ElementSet actions = new ElementSet();
		if (action instanceof ActionSequence) {
			ElementSet actionList = ((ActionSequence)action).Action();
			for (Iterator actionIter = actionList.iterator(); actionIter.hasNext();) {
				Action act = (Action) actionIter.next();
				actions.add(act);						
			}
		}
		else {
			actions.add(action);
		}
		return actions;
	}
	
	
//	// cache
//	private ElementSet m_states;
//	
//	/**
//	 * TODO: base the implementation on the stateMachine#topState rather than the transition set
//	 * @return
//	 */
//	public ElementSet State() {
//		
//		if (m_states != null) {
//			return m_states;
//		}
//		
//		// all State objects (not Fork, Entry, Exit..) that can be reached
//		ElementSet states = MMUtil.filter(Transition(), new Predicate() {
//			public boolean filter(ModelElement el)
//			{
//				Transition tr = (Transition) el;
//				StateVertex stv = tr.TargetVertex();
//				return ( stv instanceof State );
//			}
//			public ModelElement mapElement(ModelElement el) {
//				Transition tr = (Transition) el;
//				return tr.TargetVertex();
//			}
//		});
//		// add composite states 
//		ElementSet compStates = new ElementSet();
//		for (Iterator iter = states.iterator(); iter.hasNext();)
//		{
//			State state = (State) iter.next();
//			// top level container is useless ("{top}"), so don't take it
//			while (state.Container() != null && state.Container().Container() != null) {
//				compStates.add(state.Container());
//				state = state.Container();
//			}				
//		}
//		states.add(compStates);
//		
//		// remove multiple occurances of the same state object
//		states = MMUtil.removeDuplicateInstances(states);
//		
//		m_states = states;
//		
//		return states;
//	}
//	
//	public ElementSet TopLevelState() {
//		ElementSet topStates = MMUtil.filter(State(), new Predicate() {
//			public boolean filter(ModelElement el)
//			{
//				State st = (State) el;
//				return ( st.Container().Container() == null );
//			}});
//		return topStates;
//	}
}
