package alma.ACS.impl;

import alma.ACS.Alarmdouble;
import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.Callback;
import alma.ACS.SubscriptionOperations;
import alma.ACS.jbaci.BACIDispatchAction;
import alma.ACS.jbaci.BACIDispatchAction.DispatchFailedListener;
import alma.ACS.jbaci.BACIDispatchAction.DispatchRequest;
import alma.ACS.jbaci.BACIPriority;
import alma.ACS.jbaci.CompletionUtil;
import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.DataAccess.ValueChangeListener;
import alma.ACS.jbaci.PrioritizedRunnable;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.acs.util.UTCUtility;

public class AlarmdoubleSubscriptionImpl implements SubscriptionOperations, DispatchFailedListener, ValueChangeListener, PrioritizedRunnable {

	/**
	 * TODO make inline document
	 */
	private ROdoubleImpl property;
	
	/**
	 * Dispatch action.
	 */
	private BACIDispatchAction dispatchAction;
	
 	/**
	 * Suspend/resume status.
	 */
	private boolean isSuspended;

	/**
	 * Destruction status.
	 */
	private boolean isDestroyed;
	
	/**
	 * Internal state of the current alarm status.
	 */
	enum AlarmStatus {HIGH, LOW, CLEARED};
	private AlarmStatus alarmStatus;

	/**
	 * TODO make inline document
	 * @param property
	 */
	public AlarmdoubleSubscriptionImpl(ROdoubleImpl property, Alarmdouble callback, CBDescIn descIn) {
		if (property == null)
			throw new NullPointerException("property == null");
		
		if (callback == null)
			throw new NullPointerException("callback == null");
		
		if (descIn == null)
			throw new NullPointerException("descIn == null");
		
		this.property = property;
		this.isSuspended = true;
		this.isDestroyed = false;
		
		// create dispatch action
		this.dispatchAction = new BACIDispatchAction(callback, descIn, property, getPriority());
		
		// TODO 
		// make override policy configurable per instance, perhaps using a finite queue...
		// Currently the ACS mastercomp module relies on having the following line commented out,
		// since otherwise a few state change notifications are discarded.
		// this happens when changing into an activity state, which shortly afterwards changes
		// to the next state, thus producing two change events separated by a few ms only.
		//
		// (this comment was copied from CommonMonitorImpl.java)
//		dispatchAction.setOverridePolicy(true);
		
		dispatchAction.addDispatchFailedListener(this);
			
		// TODO register this alarm subscription to the property 
		//      so that this alarm subscription will be destroyed
		//      when the property is destroyed.
		
		// TODO Run periodic timer and calls working() method
		//      periodically.
		
		resume();
	}
	
	@Override
	public void run() {
		// TODO retrieve value and request calling working()
	}

	@Override
	public BACIPriority getPriority() {
		// TODO Reconsider the priority for alarm. Maybe the priority of the alarm should be higher than normal monitor.
		return BACIPriority.NORMAL;
	}

	@Override
	public void valueChanged(DataAccess<?> source, Object oldValue, Object newValue) {	
		// TODO make the alarm condition compatible with C++ implementation.
		
		// TODO Figure out what to be set to the forth argument of the constructor of Completion.
		
		double current_value = ((Double)newValue).doubleValue();
		if (alarmStatus != AlarmStatus.CLEARED &&
		    current_value >= property.alarm_low_off() &&
		    current_value <= property.alarm_high_off()) {
			Completion completion
			  = new Completion(UTCUtility.utcJavaToOmg(System.currentTimeMillis()),
					alma.ACSErr.ACSErrTypeAlarm.value,
					alma.ACSErrTypeAlarm.ACSErrAlarmCleared.value,
					new alma.ACSErr.ErrorTrace[0]);
			dispatchAction.dispatchAlarmClearedRequest(completion, current_value);
			alarmStatus = AlarmStatus.CLEARED;
		} else if (alarmStatus != AlarmStatus.HIGH &&
		    current_value >= property.alarm_high_on()) {
			Completion completion
			  = new Completion(UTCUtility.utcJavaToOmg(System.currentTimeMillis()),
					alma.ACSErr.ACSErrTypeAlarm.value,
					alma.ACSErrTypeAlarm.ACSErrAlarmHigh.value,
					new alma.ACSErr.ErrorTrace[0]);
			dispatchAction.dispatchAlarmRaisedRequest(completion, current_value);
			alarmStatus = AlarmStatus.HIGH;
		} else if (alarmStatus != AlarmStatus.LOW &&
				   current_value <= property.alarm_low_on()) {
			Completion completion
			  = new Completion(UTCUtility.utcJavaToOmg(System.currentTimeMillis()),
					alma.ACSErr.ACSErrTypeAlarm.value,
					alma.ACSErrTypeAlarm.ACSErrAlarmLow.value,
					new alma.ACSErr.ErrorTrace[0]);
			dispatchAction.dispatchAlarmRaisedRequest(completion, current_value);
			alarmStatus = AlarmStatus.LOW;
		}		
	}

	@Override
	public void dispatchFailed(BACIDispatchAction action, DispatchRequest failedRequest) {
		// TODO make sure the document addresses that the subscription
		//      object may be destroyed by BACI itself when an internal
		//      error occurs, and some notification (work() method) is
		//      sent to the client.
		destroy();
	}

	/**
	 * This method request calling done() method of the callback
	 * object that the client passed.
	 * 
	 * TODO This is basically the copy of CommonMonitorImpl.retrieveValueAndDispatch()
	 * method implementation. Some portion of the code in this method
	 * is not 100% clear, and it should be finally clarified or replaced.
	 * 
	 * @param keyTime TODO clarify what it means 
	 */
	private void requestCallingDone(long keyTime) {
		// create new holder (done expeditiously)
		CompletionHolder completionHolder = CompletionUtil.createCompletionHolder();
		
		// retrieve value
		// TODO this code was copied from CommonMonitorImpl.retrieveValuAndDispatch(),
		//      but maybe it is not necessary to obtain the value from the property
		//      as the value can be also cached in this object.
		//      It seems that mnemonicValue() calls wait() method internally in some
		//      occasion. However, this requestCallingDone() is called from destroy()
		//      which is a "synchronize" method. It is generally not good to do
		//      something taking long time in a "synchronize" method.
		Object value = property.mnemonicValue(keyTime, completionHolder);
		
		Completion completion = CompletionUtil.cloneCompletion(completionHolder.value);
		completion.timeStamp = UTCUtility.utcJavaToOmg(keyTime);
		
		// TODO Make sure that all completion fields are fulfilled.
		
		dispatchAction.dispatchDoneRequest(completion, value);
	}
	
	/**
	 * @see alma.ACS.SubscriptionOperations#suspend()
	 */
	@Override
	public synchronized void suspend() {
		if (isSuspended)
			return;
		
		property.getDataAccess().removeValueChangeListener(this);
		
		isSuspended = true;
	}

	/**
	 * @see alma.ACS.SubscriptionOperations#resume()
	 */
	@Override
	public synchronized void resume() {
		if (!isSuspended)
			return;
		
		try {
			// TODO check the current value and request calling alarm_raised() or alarm_cleared(). 
			
			property.getDataAccess().addValueChangeListener(this);
		} catch (DataAccess.OnChangeNotSupportedException ex) {
			// TODO start polling thread to acquire the value from
			//      DataAccess at a certain interval (use ex.recommendedPoolTime()).
			throw new RuntimeException("Timer trigger for alarm is not supported.", ex);
		}
		
		isSuspended = false;
	}
	
	/**
	 * @see alma.ACS.SubscriptionOperations#destroy()
	 */
	@Override
	public synchronized void destroy() {
		if (isDestroyed)
			return;
		
		isDestroyed = true;
		dispatchAction.removeDispatchFailedListener(this);
		
		if (!isSuspended)
			suspend();
		
		// Notify the client of the death of this subscription.
		requestCallingDone(System.currentTimeMillis());
		
		// TODO unregister this alarm subscription from the
		//      property alarm list.
	}
}
