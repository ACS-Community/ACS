package alma.acs.tmcdb;

/**
 * This class has been automatically generated from the 'HwConfigMonitoring' TMCDB table model,
 * and represents the 'IFDelIFSwitchEnum' CHECK constraint defined for the 'IFSwitch' column of the 'IFDelay' table.
 *
 * <p>This is automatic generated code, so don't try to change it by yourself!
 */
public enum IFDelIFSwitchEnum {

	USB_HIGH("USB_HIGH"),
	USB_LOW("USB_LOW"),
	LSB_HIGH("LSB_HIGH"),
	LSB_LOW("LSB_LOW");

	private String _stringValue;

	IFDelIFSwitchEnum(String value) {
		_stringValue = value;
	}

	public String toString() {
		return _stringValue;
	}

	public static IFDelIFSwitchEnum valueOfForEnum(String value) {
		if( value.equals("USB_HIGH") )
			return USB_HIGH;
		if( value.equals("USB_LOW") )
			return USB_LOW;
		if( value.equals("LSB_HIGH") )
			return LSB_HIGH;
		if( value.equals("LSB_LOW") )
			return LSB_LOW;
		else
			throw new RuntimeException("Invalid value for IFDelIFSwitchEnum enumeration: " + value);
	}

}
