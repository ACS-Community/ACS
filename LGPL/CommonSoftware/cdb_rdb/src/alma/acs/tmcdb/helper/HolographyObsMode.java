package alma.acs.tmcdb;

/**
 * This class has been automatically generated from the 'HwConfigMonitoring' TMCDB table model,
 * and represents the 'HolographyObsMode' CHECK constraint defined for the 'ObsMode' column of the 'Holography' table.
 *
 * <p>This is automatic generated code, so don't try to change it by yourself!
 */
public enum HolographyObsMode {

	TOWER("TOWER"),
	ASTRO("ASTRO");

	private String _stringValue;

	HolographyObsMode(String value) {
		_stringValue = value;
	}

	public String toString() {
		return _stringValue;
	}

	public static HolographyObsMode valueOfForEnum(String value) {
		if( value.equals("TOWER") )
			return TOWER;
		if( value.equals("ASTRO") )
			return ASTRO;
		else
			throw new RuntimeException("Invalid value for HolographyObsMode enumeration: " + value);
	}

}
