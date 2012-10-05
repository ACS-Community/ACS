package alma.acs.nc.sm.generated;

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.ACSErrTypeCommon.wrappers.AcsJStateMachineActionEx;

/**
 * Interface to send events to the SM, similar
 * to the IDL-generated component "Operations" interface 
 * <p>
 * TODO: A code-generator option should decide whether the named
 * event methods delegate to {@link #fireSignal(Enum)}
 * or to {@link #fireSignalWithErrorFeedback(Enum)}, 
 * and add the declared exceptions accordingly.
 * Currently we use fireSignalWithErrorFeedback.
 * @author hsommer
 */
public interface EventSubscriberSignalHandler
{
	boolean setUpEnvironment() throws AcsJIllegalStateEventEx, AcsJStateMachineActionEx;

	boolean startReceivingEvents() throws AcsJIllegalStateEventEx, AcsJStateMachineActionEx;

	boolean suspend() throws AcsJIllegalStateEventEx, AcsJStateMachineActionEx;

	boolean resume() throws AcsJIllegalStateEventEx, AcsJStateMachineActionEx;

	boolean stopReceivingEvents() throws AcsJIllegalStateEventEx, AcsJStateMachineActionEx;

	boolean cleanUpEnvironment() throws AcsJIllegalStateEventEx, AcsJStateMachineActionEx;
	

	boolean fireSignal(EventSubscriberSignal signal);

	boolean fireSignalWithErrorFeedback(EventSubscriberSignal signal) 
			throws AcsJIllegalStateEventEx, AcsJStateMachineActionEx;

}
