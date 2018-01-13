package alma.acs.tmcdb;

/**
 * This class has been automatically generated from the 'HwConfigMonitoring' TMCDB table model,
 * and represents the 'BEStartupBEType' CHECK constraint defined for the 'BaseElementType' column of the 'BaseElementStartup' table.
 *
 * <p>This is automatic generated code, so don't try to change it by yourself!
 */
public enum BEStartupBEType {

	ANTENNA("Antenna"),
	PAD("Pad"),
	FRONTEND("FrontEnd"),
	WEATHERSTATIONCONTROLLER("WeatherStationController"),
	CENTRALLO("CentralLO"),
	AOSTIMING("AOSTiming"),
	HOLOGRAPHYTOWER("HolographyTower"),
	ARRAY("Array"),
	PHOTONICREFERENCE1("PhotonicReference1"),
	PHOTONICREFERENCE2("PhotonicReference2"),
	PHOTONICREFERENCE3("PhotonicReference3"),
	PHOTONICREFERENCE4("PhotonicReference4"),
	PHOTONICREFERENCE5("PhotonicReference5"),
	PHOTONICREFERENCE6("PhotonicReference6");

	private String _stringValue;

	BEStartupBEType(String value) {
		_stringValue = value;
	}

	public String toString() {
		return _stringValue;
	}

	public static BEStartupBEType valueOfForEnum(String value) {
		if( value.equals("Antenna") )
			return ANTENNA;
		if( value.equals("Pad") )
			return PAD;
		if( value.equals("FrontEnd") )
			return FRONTEND;
		if( value.equals("WeatherStationController") )
			return WEATHERSTATIONCONTROLLER;
		if( value.equals("CentralLO") )
			return CENTRALLO;
		if( value.equals("AOSTiming") )
			return AOSTIMING;
		if( value.equals("HolographyTower") )
			return HOLOGRAPHYTOWER;
		if( value.equals("Array") )
			return ARRAY;
		if( value.equals("PhotonicReference1") )
			return PHOTONICREFERENCE1;
		if( value.equals("PhotonicReference2") )
			return PHOTONICREFERENCE2;
		if( value.equals("PhotonicReference3") )
			return PHOTONICREFERENCE3;
		if( value.equals("PhotonicReference4") )
			return PHOTONICREFERENCE4;
		if( value.equals("PhotonicReference5") )
			return PHOTONICREFERENCE5;
		if( value.equals("PhotonicReference6") )
			return PHOTONICREFERENCE6;
		else
			throw new RuntimeException("Invalid value for BEStartupBEType enumeration: " + value);
	}

}
