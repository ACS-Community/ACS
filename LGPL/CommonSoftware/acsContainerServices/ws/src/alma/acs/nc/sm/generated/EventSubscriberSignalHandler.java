package alma.acs.nc.sm.generated;

/**
 * Interface to send events to the SM, similar
 * to the IDL-generated component "Operations" interface 
 * 
 * @author hsommer
 */
public interface EventSubscriberSignalHandler
{
	boolean setUpEnvironment();

	boolean startReceivingEvents();

	boolean suspend();

	boolean resume();

	boolean stopReceivingEvents();

	boolean cleanUpEnvironment();
	

	boolean fireSignal(EventSubscriberSignal signal);
}
