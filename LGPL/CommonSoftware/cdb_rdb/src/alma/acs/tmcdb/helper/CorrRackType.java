package alma.acs.tmcdb;

/**
 * This class has been automatically generated from the 'HwConfigMonitoring' TMCDB table model,
 * and represents the 'CorrRackType' CHECK constraint defined for the 'RackType' column of the 'CorrQuadrantRack' table.
 *
 * <p>This is automatic generated code, so don't try to change it by yourself!
 */
public enum CorrRackType {

	STATION("Station"),
	CORRELATOR("Correlator");

	private String _stringValue;

	CorrRackType(String value) {
		_stringValue = value;
	}

	public String toString() {
		return _stringValue;
	}

	public static CorrRackType valueOfForEnum(String value) {
		if( value.equals("Station") )
			return STATION;
		if( value.equals("Correlator") )
			return CORRELATOR;
		else
			throw new RuntimeException("Invalid value for CorrRackType enumeration: " + value);
	}

}
