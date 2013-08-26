package si.ijs.acs.objectexplorer.engine.BACI.converters;

import si.ijs.acs.objectexplorer.engine.BACI.BACIConverterSupport;
import si.ijs.acs.objectexplorer.engine.DataType;
import si.ijs.acs.objectexplorer.engine.BACI.BACIDataType;

/**
 * BACI property degree (double type) -> dd:mm:ss converter. 
 * @author matej.sekoranjaATcosylab.com
 */
public class BACIDegreesToDDMMSSConverter extends BACIConverterSupport {

	/**
	 * Required default constructor. 
	 */
	public BACIDegreesToDDMMSSConverter() {
		super();
	}

	/**
	 * @see si.ijs.acs.objectexplorer.engine.BACI.BACIConverterSupport#convertPropertyValue(java.lang.Object)
	 */
	public Object convertPropertyValue(Object value) {
		double degrees = ((Double)value).doubleValue();
		int dd = (int)degrees; degrees -= dd;
		degrees *= 60;
		int mm = (int)(degrees); degrees -= mm;
		degrees *= 60;
		double ss = degrees;
		return new DDMMSS(dd, mm, ss);
	}
	
	/**
	 * @see si.ijs.acs.objectexplorer.engine.BACI.BACIConverterSupport#inverseConvertPropertyValue(java.lang.Object)
	 */
	public Object inverseConvertPropertyValue(Object value) {
		DDMMSS ddmmss = (DDMMSS)value;
		return new Double(ddmmss.dd + ddmmss.mm/60.0 + ddmmss.ss/3600.0);
	}
	
	/**
	 * @see si.ijs.acs.objectexplorer.engine.BACI.BACIConverterSupport#getInverseConvertPropertyParameterType()
	 */
	public DataType getInverseConvertPropertyParameterType() {
		return new BACIDataType(DDMMSS.class);
	}
	
	/**
	 * @see si.ijs.acs.objectexplorer.engine.BACI.BACIConverterSupport#getConvertedPropertyValueUnits(java.lang.String)
	 */
	public String getConvertedPropertyValueUnits(String units) {
		return "dd:mm:ss";
	}
	
	/**
	 * @see si.ijs.acs.objectexplorer.engine.Converter#getDescription()
	 */
	public String getDescription() {
		return "BACI property 'deg' to 'dd:mm:ss' converter";
	}
}
