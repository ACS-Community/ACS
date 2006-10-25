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

import org.openarchitectureware.core.meta.core.ElementSet;
import org.openarchitectureware.meta.uml.state.Event;
import org.openarchitectureware.meta.uml.state.SimpleState;
import org.openarchitectureware.meta.uml.state.Transition;

/**
 * @author Heiko Sommer
 */
public class AcsSimpleState extends SimpleState implements AcsState
{
	public String CheckConstraints() 
	{
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
	
	public ElementSet DistinctOutEvent() {
		HashMap<String, Event> eventMap = new HashMap<String, Event>(); // key=(String)eventName, to avoid multiple instances
		for (Iterator transitionIter = OutTransition().iterator(); transitionIter.hasNext();) {
			Transition tr = (Transition) transitionIter.next();
			if (tr.hasTrigger()) {
				Event ev = tr.Trigger();
				eventMap.put(ev.NameS(), ev);
			}
		}
		ElementSet events = new ElementSet();
		events.addAll(eventMap.values());
		return events;
	}


}
