package alma.acs.nc.sm.generated;

/**
 * Signal (event) for the EventSubscriber state machine.
 */
public enum EventSubscriberSignal
{
	setUpEnvironment,
	startReceivingEvents, 
	suspend,
	resume, 
	stopReceivingEvents, 
	cleanUpEnvironment
}
