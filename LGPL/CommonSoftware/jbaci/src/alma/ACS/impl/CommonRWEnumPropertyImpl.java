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

import alma.ACS.CBDescIn;
import alma.ACS.CBvoid;
import alma.ACS.TimeSeqHolder;
import alma.ACS.jbaci.CompletionUtil;
import alma.ACS.jbaci.DataAccess;
import alma.ACS.jbaci.PropertyInitializationFailed;
import alma.ACSErr.Completion;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.exceptions.AcsJException;

/**
 * Base RW enum class.
 * @author msekoranja
 */
public class CommonRWEnumPropertyImpl extends CommonROEnumPropertyImpl {

	/**
	 * @param name
	 * @param parentComponent
	 * @throws PropertyInitializationFailed
	 */
	CommonRWEnumPropertyImpl(Class enumClass, String name,
			CharacteristicComponentImpl parentComponent)
			throws PropertyInitializationFailed {
		super(enumClass, name, parentComponent);
	}

	/**
	 * @param name
	 * @param parentComponent
	 * @param dataAccess
	 * @throws PropertyInitializationFailed
	 */
	CommonRWEnumPropertyImpl(Class enumClass, String name,
			CharacteristicComponentImpl parentComponent, DataAccess dataAccess)
			throws PropertyInitializationFailed {
		super(enumClass, name, parentComponent, dataAccess);
	}
	
	public Completion set_sync(Object value) {
		try
		{
			return setSync(value);
		}
		catch (AcsJException acsex)
		{
			AcsJCouldntPerformActionEx cpa =
				new AcsJCouldntPerformActionEx("Failed to set value", acsex);
			return CompletionUtil.generateCompletion(cpa);
		}
	}
	
	/**
	 * Create RW enum structure.
	 * Example: 
	 * <code>
	 * BasicStatesOperations basicStatesEnumImpl = 
	 * 		(BasicStatesOperations)CommonRWEnumPropertyImpl.createEnumProperty(
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
		CommonRWEnumPropertyImpl propertyImpl;
		if (dataAccess == null)
			propertyImpl = new CommonRWEnumPropertyImpl(propertyType, name, parentComponent);
		else
			propertyImpl = new CommonRWEnumPropertyImpl(propertyType, name, parentComponent, dataAccess);
		
		return Proxy.newProxyInstance(
			propertyImpl.getClass().getClassLoader(),
			new Class[] { operationsIF }, 
			new RWEnumProxy(propertyImpl));
	}
	

	/**
	 * RW enum proxy class.
	 */
	public static class RWEnumProxy implements InvocationHandler {

		private final CommonRWEnumPropertyImpl delegate;

		RWEnumProxy(CommonRWEnumPropertyImpl delegate) {
			this.delegate = delegate;
		}

		public Object invoke(Object proxy, Method method, Object[] args)
				throws Throwable
		{
			final String methodName = method.getName();
			
			if (methodName.equals("set_sync"))
			{
				return delegate.set_sync(args[0]);
			}
			else if (methodName.equals("set_nonblocking"))
			{
				delegate.setNonblocking(args[0]);
				return null;
			}
			else if (methodName.equals("set_async"))
			{
				delegate.setAsync(args[0], (CBvoid)args[1], (CBDescIn)args[2]);
				return null;
			}
			else if (methodName.equals("get_history"))
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
