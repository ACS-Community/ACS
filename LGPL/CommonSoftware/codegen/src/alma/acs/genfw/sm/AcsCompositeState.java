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

import java.util.HashMap;
import java.util.Iterator;

import de.bmiag.genfw.meta.DesignError;
import de.bmiag.genfw.meta.ElementSet;
import de.bmiag.genfw.meta.state.CompositeState;
import de.bmiag.genfw.meta.state.Event;
import de.bmiag.genfw.meta.state.SimpleState;
import de.bmiag.genfw.meta.state.State;
import de.bmiag.genfw.meta.state.StateVertex;
import de.bmiag.genfw.meta.state.Transition;

/**
 * @author Heiko Sommer
 */
public class AcsCompositeState extends CompositeState implements AcsState
{
	public String CheckConstraints() 
	{
		if (hasDoAction()) {
			throw new DesignError("/do-actions are currently only supported for simple states.");
		}		

		AcsStateDelegate.checkCompletionTransition(this);
		
		return "ok";
	}

	public boolean isToplevelState() 
	{
		return AcsStateDelegate.isToplevelState(this);
	}
	
	public Transition CompletionTransition() 
	{
		return AcsStateDelegate.getCompletionTransition(this);
	}
	
	/**
	 * Gets all states that are part of this composite state, 
	 * not including this state itself.
	 * 
	 * @return list of {@link State} objects.
	 */
	public ElementSet DeepSubState() {
		ElementSet substates = new ElementSet();
		recursiveSubState(this, substates);
		return substates;
	}
	
	private void recursiveSubState(State state, ElementSet collection) {
		if (state instanceof CompositeState) {
			CompositeState compState = (CompositeState) state;
			for (Iterator iter = compState.SubVertex().iterator(); iter.hasNext();) {
				StateVertex vertex = (StateVertex) iter.next();
				if (!collection.contains(vertex)) { // this check may not be necessary
					if (vertex instanceof CompositeState) {
						CompositeState subCompState = (CompositeState) vertex; 
						collection.add(subCompState);
						recursiveSubState(subCompState, collection);
					}
					else if (vertex instanceof SimpleState) {
						SimpleState subSimpleState = (SimpleState) vertex; 
						collection.add(subSimpleState);
					}
					// nothing for Fork, Join, Branch, Entry, Exit vertices
				}
			}
		}			
	}
	
	/**
	 * Gets all events that trigger outgoing transitions from this composite state
	 * or any of its substates.
	 * Multiple occurances of an event with the same name are only returned once,
	 * independent of the event parameters.
	 * @return list of {@link Event} objects.
	 */
	public ElementSet DistinctDeepOutEvent() {
		HashMap eventMap = new HashMap(); // key=(String)eventName, to avoid multiple instances
		ElementSet states = DeepSubState();
		states.add(this);
		for (Iterator iter = states.iterator(); iter.hasNext();) {
			State state = (State) iter.next();			
			for (Iterator transitionIter = state.OutTransition.iterator(); transitionIter.hasNext();) {
				Transition tr = (Transition) transitionIter.next();
				if (tr.hasTrigger()) {
					Event ev = tr.Trigger();
					eventMap.put(ev.Name().Name(), ev);
				}
			}
		}
		ElementSet events = new ElementSet();
		events.addAll(eventMap.values());
		return events;
	}
	
	/**
	 * Gets all internal events from this composite state
	 * or any of its substates.
	 * Multiple occurances of an event with the same name are only returned once,
	 * independent of the event parameters.
	 * TODO: share code with DistinctDeepOutEvent
	 * @return list of {@link Event} objects.
	 */
	public ElementSet DistinctDeepInternalEvent() {
		HashMap eventMap = new HashMap(); // key=(String)eventName, to avoid multiple instances
		ElementSet states = DeepSubState();
		states.add(this);
		for (Iterator iter = states.iterator(); iter.hasNext();) {
			State state = (State) iter.next();			
			for (Iterator transitionIter = state.InternalTransition().iterator(); transitionIter.hasNext();) {
				Transition tr = (Transition) transitionIter.next();
				if (tr.hasTrigger()) {
					Event ev = tr.Trigger();
					eventMap.put(ev.Name().Name(), ev);
				}
			}
		}
		ElementSet events = new ElementSet();
		events.addAll(eventMap.values());
		return events;
	}
	
	/**
	 * Gets the substate of this composite state which is itself the initial state
	 * of the state machine, or is the container for the initial state.
	 * If the SM's entry state is not part of this substate, <code>null</code>
	 * is returned.
	 */
	public State InitialState() {
		State unwoundState = null;
		State initialState = ((AcsStateMachine) StateMachine()).InitialState();
		// unwind to direct substate of this composite state, 
		// or find out that this composite state has no entry substate 
		while (initialState.Container() != null) {
			if (initialState.Container() == this) {
				unwoundState = initialState;
				break;
			}
			initialState = initialState.Container();
		}
		return unwoundState;
	}
	
	public boolean hasInitialState() {
		return (InitialState() != null);
	}
}
