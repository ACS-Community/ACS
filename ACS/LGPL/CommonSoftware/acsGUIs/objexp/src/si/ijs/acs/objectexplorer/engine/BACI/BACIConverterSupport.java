package si.ijs.acs.objectexplorer.engine.BACI;

import java.lang.reflect.Array;
import java.util.HashMap;
import java.util.Map;

import si.ijs.acs.objectexplorer.engine.Converter;
import si.ijs.acs.objectexplorer.engine.DataType;

/**
 * Support class for BACI (property) converter.
 */
public abstract class BACIConverterSupport implements Converter {

	/**
	 * List of BACI property operations related to property value.
	 */
	public static final String[] BACI_PROPERTY_OPERATIONS =
	{
			"get_sync",
			// value obtained via callbacks - "get_async",
			"get_history",
			"get_value_trigger",
			
			// monitors
			"working",
			"done"
	};
	
	/**
	 * List of BACI property operations related to property value (inverse conversion).
	 */
	public static final String[] BACI_PROPERTY_INVERSE_OPERATIONS =
	{
			"set_nonblocking",
			"set_sync",
			"set_async",
			"set_value_trigger",
	};

	/**
	 * List of BACI property attributes related to property value.
	 */
	public static final String[] BACI_PROPERTY_ATTRIBUTES =
	{
			"min_delta_trigger",
			"default_value",
			"graph_min",
			"graph_max",
			"min_step",
			"min_value",
			"max_value",
			
			"alarm_low_off",
			"alarm_low_on",
			"alarm_high_off",
			"alarm_high_on",
	};
	
	public static final String BACI_UNIT_ATTRIBUTE = "units";
	
	/**
	 * Operations lookup table. 
	 */
	protected Map operationMap = null;
	
	/**
	 * Operations (inverse) lookup table. 
	 */
	protected Map operationInverseMap = null;

	/**
	 * Attributes lookup table. 
	 */
	protected Map attributeMap = null;

	/**
	 * Constructor.
	 */
	public BACIConverterSupport()
	{
		initializeTables();
	}

	/**
	 * Initialized lookup table.
	 *
	 */
	protected void initializeTables()
	{
		operationMap = new HashMap();
		for (int i = 0; i < BACI_PROPERTY_OPERATIONS.length; i++)
			operationMap.put(BACI_PROPERTY_OPERATIONS[i], null);
		
		attributeMap = new HashMap();
		for (int i = 0; i < BACI_PROPERTY_ATTRIBUTES.length; i++)
			attributeMap.put(BACI_PROPERTY_ATTRIBUTES[i], null);

		operationInverseMap = new HashMap();
		for (int i = 0; i < BACI_PROPERTY_INVERSE_OPERATIONS.length; i++)
			operationInverseMap.put(BACI_PROPERTY_INVERSE_OPERATIONS[i], null);
	}

	/**
	 * Returns <code>true</code> if operation contains property value(s) to be converted.
	 * @see si.ijs.acs.objectexplorer.engine.Converter#accept(java.lang.String)
	 */
	public boolean acceptConvert(String operation) {
		return operationMap.containsKey(operation) ||
			   attributeMap.containsKey(operation) ||
			   BACI_UNIT_ATTRIBUTE.equals(operation);
	}
	
	/**
	 * Returns <code>true</code> if operation contains property value(s) to be inverse converted.
	 * @see si.ijs.acs.objectexplorer.engine.Converter#acceptInverseConvert(java.lang.String)
	 */
	public boolean acceptInverseConvert(String operation) {
		return operationInverseMap.containsKey(operation);
	}

	/**
	 * @see si.ijs.acs.objectexplorer.engine.Converter#convert(java.lang.String, java.lang.Object[], java.lang.Object)
	 */
	public Object convert(String operation, Object[] params, Object returnValue) {
		if (operationMap.containsKey(operation))
		{
			if (operation.equals("get_sync"))
				return convertPropertyValue(returnValue);
			else if (operation.equals("get_history"))
			{
				try
				{
					
					int length = Array.getLength(params[1]);

					// same classes (converted and inverse converted)
					if (params[1].getClass().getComponentType() == getInverseConvertPropertyParameterType().getType())
					{
						for (int i = 0; i < length; i++)
							Array.set(params[1], i, convertPropertyValue(Array.get(params[1], i)));
					}
					else
					{
						Object newArray = Array.newInstance(getInverseConvertPropertyParameterType().getType(), length);
						for (int i = 0; i < length; i++)
							Array.set(newArray, i, convertPropertyValue(Array.get(params[1], i)));
						params[1] = newArray;
					}

				}
				catch (Throwable th)
				{
					th.printStackTrace();
				}
			}
			else if (operation.equals("get_value_trigger"))
				params[0] = convertPropertyValue(params[0]);
			else if (operation.equals("working") || operation.equals("done"))
				// do not handle CBvoid callbacks
				if (params.length == 3)
					params[0] = convertPropertyValue(params[0]);
			
			return returnValue;
		}
		else if (attributeMap.containsKey(operation))
			return convertPropertyValue(returnValue);
		else if (operation.equals(BACI_UNIT_ATTRIBUTE))
			return getConvertedPropertyValueUnits((String)returnValue);
		else
			return returnValue;
	}
	
	/**
	 * @see si.ijs.acs.objectexplorer.engine.Converter#inverseConvert(java.lang.String, java.lang.Object[])
	 */
	public void inverseConvert(String operation, Object[] params) {
		// "set_nonblocking", "set_sync", "set_async", "set_value_trigger"
		params[0] = inverseConvertPropertyValue(params[0]);
	}

	/**
	 * @see si.ijs.acs.objectexplorer.engine.Converter#getInverseConvertParameterTypes(java.lang.String)
	 */
	public DataType[] getInverseConvertParameterTypes(String operation, DataType[] parameterTypes) {
		DataType[] types = new DataType[parameterTypes.length];
		System.arraycopy(parameterTypes, 0, types, 0, parameterTypes.length);
		types[0] = getInverseConvertPropertyParameterType();
		return types;
	}

	/**
	 * Convert property value.
	 * @param	value to be converted.
	 * @return	converted value.
	 */
	public abstract Object convertPropertyValue(Object value);

	/**
	 * Inverse convert property value.
	 * @param	value to be inverse converted.
	 * @return	inverse converted value.
	 */
	public abstract Object inverseConvertPropertyValue(Object value);

	/**
	 * Class type of converted property value.
	 * This is returned class by convertPropertyValue method and expected class type of inverseConvertPropertyValue.
	 * @return	class type of converted property value
	 */
	public abstract DataType getInverseConvertPropertyParameterType();

	/**
	 * Get converted property value units.
	 * @return converted property value units.
	 */
	public abstract String getConvertedPropertyValueUnits(String units);
}
