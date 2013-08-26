
package si.ijs.acs.objectexplorer.engine;

/**
 * Converter interface.
 */
public interface Converter {
	
	/**
	 * Get the description of the converter, e.g. "kph -> mph".
	 * @return description of the converter
	 */
	public String getDescription();
	
	/**
	 * Check if conversion is to be applied on the given operation.
	 * @param operation	name of the operation.
	 * @return	<code>true</code> if conversion on any parameter (or return value) of the given operation is to be made, otherwise <code>false</code>. 
	 */
	public boolean acceptConvert(String operation);
	
	/**
	 * Check if inverse conversion is to be applied on the given operation.
	 * @param operation	name of the operation.
	 * @return	<code>true</code> if inverse conversion on any parameter (or return value) of the given operation is to be made, otherwise <code>false</code>. 
	 */
	public boolean acceptInverseConvert(String operation);

	/**
	 * Convert value (by chaning params array and returning converted <code>returnValue</code>).
	 * @param	operation	operation name owning the parameter.
	 * @param	params	operation parameters (response) or auxiliary parameters (see RemoteCall#getAuxReturnValues())
	 * @param	returnValue operation return value, if not available <code>null</code>.
	 * @return	converted <code>returnValue</code> value, or just unchanged <code>returnValue</code>.
	 * @see RemoteCall#getAuxReturnValues()
	 */
	public Object convert(String operation, Object[] params, Object returnValue);

	/**
	 * Inverse convert value.
	 * @param	operation	operation name owning the parameter.
	 * @param	params		operation parameters
	 * @param	returnValue operation return value, if not available <code>null</code>.
	 */
	public void inverseConvert(String operation, Object[] params);
	
	/**
	 * Get parameters class types.
	 * This method returns classes expected by <code>inverseConvert</code> method.
	 * This method is needed to support conversion between different class types,
	 * e.g. degrees/radians to a structure of (hours, minutes, seconds).
	 * @param operation			operation name owning the parameters.
	 * @param parameterTypes	operation decalred parameter types.
	 * @return	classes of parameters of given operation.
	 */
	public DataType[] getInverseConvertParameterTypes(String operation, DataType[] parameterTypes);
	
}
