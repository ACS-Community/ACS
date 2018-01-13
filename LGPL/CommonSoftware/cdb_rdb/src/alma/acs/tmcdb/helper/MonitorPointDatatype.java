package alma.acs.tmcdb;

/**
 * This class has been automatically generated from the 'HwConfigMonitoring' TMCDB table model,
 * and represents the 'MonitorPointDatatype' CHECK constraint defined for the 'DataType' column of the 'MonitorPoint' table.
 *
 * <p>This is automatic generated code, so don't try to change it by yourself!
 */
public enum MonitorPointDatatype {

	FLOAT("float"),
	DOUBLE("double"),
	BOOLEAN("boolean"),
	STRING("string"),
	INTEGER("integer"),
	ENUM("enum"),
	CLOB("clob");

	private String _stringValue;

	MonitorPointDatatype(String value) {
		_stringValue = value;
	}

	public String toString() {
		return _stringValue;
	}

	public static MonitorPointDatatype valueOfForEnum(String value) {
		if( value.equals("float") )
			return FLOAT;
		if( value.equals("double") )
			return DOUBLE;
		if( value.equals("boolean") )
			return BOOLEAN;
		if( value.equals("string") )
			return STRING;
		if( value.equals("integer") )
			return INTEGER;
		if( value.equals("enum") )
			return ENUM;
		if( value.equals("clob") )
			return CLOB;
		else
			throw new RuntimeException("Invalid value for MonitorPointDatatype enumeration: " + value);
	}

}
