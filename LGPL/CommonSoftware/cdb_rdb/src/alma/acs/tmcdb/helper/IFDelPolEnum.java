package alma.acs.tmcdb;

/**
 * This class has been automatically generated from the 'HwConfigMonitoring' TMCDB table model,
 * and represents the 'IFDelPolEnum' CHECK constraint defined for the 'Polarization' column of the 'IFDelay' table.
 *
 * <p>This is automatic generated code, so don't try to change it by yourself!
 */
public enum IFDelPolEnum {

	X("X"),
	Y("Y");

	private String _stringValue;

	IFDelPolEnum(String value) {
		_stringValue = value;
	}

	public String toString() {
		return _stringValue;
	}

	public static IFDelPolEnum valueOfForEnum(String value) {
		if( value.equals("X") )
			return X;
		if( value.equals("Y") )
			return Y;
		else
			throw new RuntimeException("Invalid value for IFDelPolEnum enumeration: " + value);
	}

}
