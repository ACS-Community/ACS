/**
 * 
 */
package alma.ACS.impl;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

import org.omg.CORBA.NO_IMPLEMENT;
import org.omg.CORBA.NO_RESOURCES;

import alma.ACS.Alarmpattern;
import alma.ACS.CBDescIn;
import alma.ACS.CBDescOut;
import alma.ACS.CBpattern;
import alma.ACS.Callback;
import alma.ACS.Condition;
import alma.ACS.Monitor;
import alma.ACS.Monitorpattern;
import alma.ACS.MonitorpatternHelper;
import alma.ACS.MonitorpatternPOATie;
import alma.ACS.NoSuchCharacteristic;
import alma.ACS.Subscription;
import alma.ACS.TimeSeqHolder;
import alma.ACS.jbaci.CallbackDispatcher;
import alma.ACS.jbaci.CompletionUtil;
import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.PropertyInitializationFailed;
import alma.ACSErr.Completion;
import alma.ACSErr.CompletionHolder;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.exceptions.AcsJException;

/**
 * Base enum class. Contains common methods that does not contain <code>enumClass</code> class in method' signatures.
 * <code>enumClass</code> must has standard Java CORBA generated metods of called <code>fromInt(int)</code> and <code>value()</code>.
 * @author msekoranja
 */
public class CommonROEnumPropertyImpl extends ROCommonPropertyImpl {

	/**
	 * @param name
	 * @param parentComponent
	 * @throws PropertyInitializationFailed
	 */
	CommonROEnumPropertyImpl(Class enumClass, String name,
			CharacteristicComponentImpl parentComponent)
			throws PropertyInitializationFailed {
		super(enumClass, name, parentComponent);
		initialize();
	}

	/**
	 * @param name
	 * @param parentComponent
	 * @param dataAccess
	 * @throws PropertyInitializationFailed
	 */
	CommonROEnumPropertyImpl(Class enumClass, String name,
			CharacteristicComponentImpl parentComponent, DataAccess dataAccess)
			throws PropertyInitializationFailed {
		super(enumClass, name, parentComponent, dataAccess);
		initialize();
	}
	
    private Method from_int_method;
    private Method value_method;
    
    @SuppressWarnings("unchecked")
	protected void initialize()
    	throws PropertyInitializationFailed
    {
    	try {
        	from_int_method = propertyType.getMethod("from_int", new Class[] { int.class });
        	value_method = propertyType.getMethod("value", (Class[])null);
    	} catch (NoSuchMethodException nse) {
    		throw new PropertyInitializationFailed("given enum class does not implement required method(s)", nse);
    	}
    }

    protected int value(Object value)
	{
    	try {
        	return ((Integer)value_method.invoke(value, (Object[])null)).intValue();
    	} catch (Throwable th) {
    		throw new RuntimeException("failed to get int value from enum instance", th);
    	}
	}
	
	protected Object from_int(int value)
	{
    	try {
		// needed since it cen be called from super-constructor
    		if (from_int_method == null)
    			initialize();
        	return from_int_method.invoke(null, new Object[] { value });
    	} catch (Throwable th) {
    		throw new RuntimeException("failed to get enum instance from int value", th);
    	}
	}
	
	/* (non-Javadoc)
	 */
	public Condition[] condition() {
		try
		{
			int[] temp = characteristicModelImpl.getIntegerSeq("condition");
			Condition[] ret = new Condition[temp.length];
			for (int i=0; i<temp.length; i++)
				ret[i] = Condition.from_int(temp[i]);
			return ret;
		} catch (NoSuchCharacteristic e) {
			throw new NO_RESOURCES();
		}
	}

	/* (non-Javadoc)
	 */
	public String[] statesDescription() {
		try {
			return characteristicModelImpl.getStringSeq("statesDescription");
		} catch (NoSuchCharacteristic e) {
			throw new NO_RESOURCES();
		}
	}

	/* (non-Javadoc)
	 */
	public Subscription new_subscription_AlarmEnum(Alarmpattern cb,
			CBDescIn desc) {
		// TODO NO_IMPLEMENT
		throw new NO_IMPLEMENT();
	}

	/* (non-Javadoc)
	 */
	public Monitorpattern create_monitor(CBpattern cb, CBDescIn desc) {
		return (Monitorpattern) create_postponed_monitor(0, cb, desc);
	}

	/* (non-Javadoc)
	 */
	public Monitor create_postponed_monitor(long start_time, CBpattern cb, CBDescIn desc) {
		
		// create monitor and its servant
		MonitorpatternImpl monitorImpl = new MonitorpatternImpl(this, cb, desc, start_time);
		MonitorpatternPOATie monitorTie = new MonitorpatternPOATie(monitorImpl);

		// register and activate		
		return MonitorpatternHelper.narrow(this.registerMonitor(monitorImpl, monitorTie));
	}

	/* (non-Javadoc)
	 */
	public void get_async(CBpattern cb, CBDescIn desc) {
		getAsync(cb, desc);
	}
	
	/* (non-Javadoc)
	 */
	@Override
	public Object readPropertyTypeCharacteristic(String name)
			throws NoSuchCharacteristic {
		return from_int(characteristicModelImpl.getInteger(name));
	}


	/* (non-Javadoc)
	 * @see alma.ACS.jbaci.CallbackDispatcher#dispatchCallback(int, java.lang.Object, alma.ACS.Callback, alma.ACSErr.Completion, alma.ACS.CBDescOut)
	 */
	public boolean dispatchCallback(int type, Object value, Callback callback,
			Completion completion, CBDescOut desc) {
		// NOTE: CBpattern is being used for enums
		try
		{	
			if (type == CallbackDispatcher.DONE_TYPE)
				((CBpattern)callback).done(value(value), completion, desc);
			else if (type == CallbackDispatcher.WORKING_TYPE)
				((CBpattern)callback).working(value(value), completion, desc);
			else 
				return false;
				
			return true;
		}
		catch (Throwable th)
		{
			return false;
		}
	}

	
	/* (non-Javadoc)
	 */
	private Object getEnumArrayFromCharacteristicModel(String name) {
		try
		{
			int[] temp = characteristicModelImpl.getIntegerSeq(name);
			Object ret = Array.newInstance(propertyType, temp.length);
			for (int i=0; i<temp.length; i++)
				Array.set(ret, i, from_int(temp[i]));
			return ret;
		} catch (NoSuchCharacteristic e) {
			throw new NO_RESOURCES();
		}
	}
	
	/* (non-Javadoc)
	 */
	public Object alarm_on() {
		return getEnumArrayFromCharacteristicModel("alarm_on");
	} 
	
	/* (non-Javadoc)
	 */
	public Object alarm_off() {
		return getEnumArrayFromCharacteristicModel("alarm_off");
	} 

	/* (non-Javadoc)
	 */
	public Object allStates() {
		int statesCount = 0;
		for (; statesCount < Integer.MAX_VALUE; statesCount++)
		{
			try
			{
				from_int(statesCount);
			} catch (RuntimeException noMore) {
				break;
			}
		}

		Object states = Array.newInstance(propertyType, statesCount);
		for (int i=0; i<statesCount; i++)
			Array.set(states, i, from_int(i));
		
		return states;
	}


	/* (non-Javadoc)
	 */
	public Object default_value() {
		try {
			return from_int(characteristicModelImpl.getInteger("default_value"));
		} catch (NoSuchCharacteristic e) {
			return from_int(0);
		}
	}
	
	/* (non-Javadoc)
	 */
	public Object get_sync(CompletionHolder c) {
		try
		{
			return getSync(c);
		}
		catch (AcsJException acsex)
		{
			AcsJCouldntPerformActionEx cpa =
				new AcsJCouldntPerformActionEx("Failed to retrieve value", acsex);
			c.value = CompletionUtil.generateCompletion(cpa);
			// return default value in case of error
			return default_value();
		}
	}
	
	
	
	/**
	 * Create default in-memory RO enum structure.
	 * @see #createEnumProperty(Class, Class, String, CharacteristicComponentImpl, DataAccess)
	 */
	public static Object createEnumProperty(
			Class operationsIF,
			Class propertyType, String name,
			CharacteristicComponentImpl parentComponent)
		throws PropertyInitializationFailed
	{
		return createEnumProperty(operationsIF, propertyType, name, parentComponent, null);
	}

	/**
	 * Create RO enum structure.
	 * Example: 
	 * <code>
	 * BasicStatesOperations basicStatesEnumImpl = 
	 * 		(BasicStatesOperations)CommonROEnumPropertyImpl.createEnumProperty(
	 * 			BasicStatesOperations.class,
	 *			BasicStates.class,
	 *			"state",
	 *			parentComponent,
	 *			dataAccess);
	 * </code>
	 * @param operationsIF CORBA <enum type>Operations class.
	 * @param propertyType CORBA enum class.
	 * @param name name of the property.
	 * @param parentComponent property component.
	 * @param dataAccess data access to be used.
	 * @return CORBA <enum type>Operations instance.
	 * @throws PropertyInitializationFailed
	 */
	public static Object createEnumProperty(
			Class operationsIF,
			Class propertyType, String name,
			CharacteristicComponentImpl parentComponent,
			DataAccess dataAccess)
		throws PropertyInitializationFailed
	{
		CommonROEnumPropertyImpl propertyImpl;
		if (dataAccess == null)
			propertyImpl = new CommonROEnumPropertyImpl(propertyType, name, parentComponent);
		else
			propertyImpl = new CommonROEnumPropertyImpl(propertyType, name, parentComponent, dataAccess);
		
		return Proxy.newProxyInstance(
			propertyImpl.getClass().getClassLoader(),
			new Class[] { operationsIF }, 
			new ROEnumProxy(propertyImpl));
	}
	

	/**
	 * RO enum proxy class.
	 */
	public static class ROEnumProxy implements InvocationHandler {

		private final CommonROEnumPropertyImpl delegate;

		ROEnumProxy(CommonROEnumPropertyImpl delegate) {
			this.delegate = delegate;
		}

		public Object invoke(Object proxy, Method method, Object[] args)
				throws Throwable
		{
			final String methodName = method.getName();
			
			if (methodName.equals("get_history"))
			{
				int n_last_values = ((Integer)args[0]).intValue();
				Object seqHolder = args[1];
				TimeSeqHolder ts = (TimeSeqHolder)args[2];

				Object historyArray = delegate.getHistory(n_last_values, ts);
				
				Field valueField = seqHolder.getClass().getField("value");
				valueField.set(seqHolder, historyArray);
				
				return Array.getLength(historyArray);
				
			}
			else
			{
				// delegate
				try {
					// TODO cache
					final Method localMethod = delegate.getClass().getMethod(method.getName(), method.getParameterTypes());
					return localMethod.invoke(delegate, args);
				} catch (InvocationTargetException e) {
					throw e.getTargetException();
				} catch (Throwable e) {
					throw e;
				}
			}
		}
	}
	
	
}
