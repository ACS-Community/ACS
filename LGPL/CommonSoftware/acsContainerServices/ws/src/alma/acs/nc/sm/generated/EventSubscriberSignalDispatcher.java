package alma.acs.nc.sm.generated;

import static alma.acs.nc.sm.generated.EventSubscriberSignal.cleanUpEnvironment;
import static alma.acs.nc.sm.generated.EventSubscriberSignal.resume;
import static alma.acs.nc.sm.generated.EventSubscriberSignal.setUpEnvironment;
import static alma.acs.nc.sm.generated.EventSubscriberSignal.startReceivingEvents;
import static alma.acs.nc.sm.generated.EventSubscriberSignal.stopReceivingEvents;
import static alma.acs.nc.sm.generated.EventSubscriberSignal.suspend;

import alma.acs.nc.sm.generic.AcsScxmlEngine;

/**
 * Dispatches SM signals (events) by calling {@link AcsScxmlEngine#fireSignal(String)}
 * on the provided state machine. 
 * This is a convenience class that can be used as a base class for the ACS component
 * or other classes that use the state machine.
 * 
 * @author hsommer
 */
public abstract class EventSubscriberSignalDispatcher implements EventSubscriberSignalHandler
{
	protected abstract AcsScxmlEngine<EventSubscriberSignal, EventSubscriberAction> getScxmlEngine();
	
	@Override
	public boolean setUpEnvironment() {
		return fireSignal(setUpEnvironment);
	}

	@Override
	public boolean startReceivingEvents(){
		return fireSignal(startReceivingEvents);
	}

	@Override
	public boolean suspend(){
		return fireSignal(suspend);
	}

	@Override
	public boolean resume(){
		return fireSignal(resume);
	}

	@Override
	public boolean stopReceivingEvents(){
		return fireSignal(stopReceivingEvents);
	}

	@Override
	public boolean cleanUpEnvironment(){
		return fireSignal(cleanUpEnvironment);
	}
	
	@Override
	public boolean fireSignal(EventSubscriberSignal signal) {
		return getScxmlEngine().fireSignal(signal);
	}
	
}
