package alma.acs.tmcdb;

/**
 * This class has been automatically generated from the 'HwConfigMonitoring' TMCDB table model,
 * and represents the 'ArrayType' CHECK constraint defined for the 'Type' column of the 'Array' table.
 *
 * <p>This is automatic generated code, so don't try to change it by yourself!
 */
public enum ArrayType {

	AUTOMATIC("automatic"),
	MANUAL("manual");

	private String _stringValue;

	ArrayType(String value) {
		_stringValue = value;
	}

	public String toString() {
		return _stringValue;
	}

	public static ArrayType valueOfForEnum(String value) {
		if( value.equals("automatic") )
			return AUTOMATIC;
		if( value.equals("manual") )
			return MANUAL;
		else
			throw new RuntimeException("Invalid value for ArrayType enumeration: " + value);
	}

}
