
package si.ijs.acs.objectexplorer.engine.BACI.converters;

/**
 * Converter for BACI properties simply multiplying by 1000.
 */
public class BACIDouble1000Multiplier extends BACIDoubleLinearConverter {
	
	/**
	 * Default constructor is required.
	 */
	public BACIDouble1000Multiplier()
	{
		super(1000, 0);
	}
	
	/**
	 * @see si.ijs.acs.objectexplorer.engine.Converter#getDescription()
	 */
	public String getDescription() {
		return "BACI property by 1000 multiplier";
	}
}
