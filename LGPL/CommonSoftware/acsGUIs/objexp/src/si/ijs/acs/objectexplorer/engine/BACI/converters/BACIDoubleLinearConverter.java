
package si.ijs.acs.objectexplorer.engine.BACI.converters;

import si.ijs.acs.objectexplorer.engine.BACI.BACIConverterSupport;
import si.ijs.acs.objectexplorer.engine.DataType;
import si.ijs.acs.objectexplorer.engine.BACI.BACIDataType;

/**
 * Implementation of BACI double property linear converter, <pre>cx + n</pre>.
 */

public class BACIDoubleLinearConverter extends BACIConverterSupport {
	
	/**
	 * Coeficient.
	 */
	protected double c;
	
	/**
	 * Offset.
	 */
	protected double n;
	
	/**
	 * Constructor 
	 * @param c	coeficient.
	 * @param n offset.
	 */
	public BACIDoubleLinearConverter(double c, double n) {
		super();
		this.c = c;
		this.n = n;
	}
	
	/**
	 * @see si.ijs.acs.objectexplorer.engine.BACI.BACIConverterSupport#convertPropertyValue(java.lang.Object)
	 */
	public Object convertPropertyValue(Object value) {
		return new Double(c * ((Double)value).doubleValue() + n);
	}
	
	/**
	 * @see si.ijs.acs.objectexplorer.engine.BACI.BACIConverterSupport#inverseConvertPropertyValue(java.lang.Object)
	 */
	public Object inverseConvertPropertyValue(Object value) {
		return new Double((((Double)value).doubleValue() - n) / c);
	}
	
	/**
	 * @see si.ijs.acs.objectexplorer.engine.BACI.BACIConverterSupport#getConvertedPropertyValueUnits(String)
	 */
	public String getConvertedPropertyValueUnits(String units) {
		StringBuffer retVal = new StringBuffer(64);
		if (c != 0.0)
		{
			retVal.append(String.valueOf(c));
			retVal.append('*');
		}
		retVal.append(units);
		if (n != 0.0)
		{
			retVal.append('+');
			retVal.append(String.valueOf(n));
		}
		return retVal.toString();
	}
	
	/**
	 * @see si.ijs.acs.objectexplorer.engine.Converter#getDescription()
	 */
	public String getDescription() {
		return "Double BACI property linear converter.";
	}
	
	/**
	 * @see si.ijs.acs.objectexplorer.engine.BACI.BACIConverterSupport#getInverseConvertPropertyParameterType()
	 */
	public DataType getInverseConvertPropertyParameterType() {
		return new BACIDataType(double.class);
	}
}
