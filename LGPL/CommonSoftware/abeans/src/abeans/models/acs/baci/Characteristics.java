/*
 * @@COPYRIGHT@@
 */

package abeans.models.acs.baci;

/**
 * Names of characteristics used by ACS. 
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version		@@VERSION@@
 */
public interface Characteristics {

	/**
	 * Name of the name characteristic.
	 */
	public static final String NAME_DISPLAY_NAME = "name";

	/**
	 * Name of the characteristic describing the property.
	 */
	public static final String NAME_DESCRIPTION = "description";
	
	/**
	 * Name of the characteristic describing the property.
	 */
	public static final String NAME_FORMAT = "format";

	/**
	 * Name of the characteristic describing the property.
	 */
	public static final String NAME_UNITS = "units";

	/**
	 * Name of the characteristic defining property resolution.
	 */
	public static final String NAME_RESOLUTION = "resolution";

	/**
	 * Name of the characteristic defining property minimal value.
	 */
	public static final String NAME_MINIMUM = "min_value";
	
	/**
	 * Name of the characteristic defining property maximal value.
	 */
	public static final String NAME_MAXIMUM = "max_value";
	
	/**
	 * Name of the characteristic defining property minimal step value.
	 */
	public static final String NAME_MIN_STEP = "min_step";

	/**
	 * Name of the characteristic defining property defaul value.
	 */
	public static final String NAME_DEFAULT_VALUE = "default_value";

	/**
	 * Name of the characteristic defining property minimal value to be graphed.
	 */
	public static final String NAME_GRAPH_MIN = "graph_min";
	
	/**
	 * Name of the characteristic defining property maximal value to be graphed.
	 */
	public static final String NAME_GRAPH_MAX = "graph_max";

	/**
	 * Name of the characteristic defining property default timer trigger in 100th of ns.
	 */
	public static final String NAME_DEFAULT_TIMER_TRIGGER = "default_timer_trigger";

	/**
	 * Name of the characteristic defining property minimal timer trigger in 100th of ns.
	 */
	public static final String NAME_MINIMAL_TIMER_TRIGGER = "min_timer_trigger";

	/**
	 * Name of the characteristic defining property minimal delta trigger value.
	 */
	public static final String NAME_MINIMAL_DELTA_TRIGGER = "min_delta_trigger";

	/**
	 * Name of the characteristic defining the upper alarm limit when the alarm state changes from off to on.
	 */
	public static final String NAME_ALARM_HIGH_ON = "alarm_high_on";

	/**
	 * Name of the characteristic defining the upper alarm limit when the alarm state changes from on to off.
	 */
	public static final String NAME_ALARM_HIGH_OFF = "alarm_high_off";

	/**
	 * Name of the characteristic defining the lower alarm limit when the alarm state changes from off to on.
	 */
	public static final String NAME_ALARM_LOW_ON = "alarm_low_on";

	/**
	 * Name of the characteristic defining the lower alarm limit when the alarm state changes from on to off.
	 */
	public static final String NAME_ALARM_LOW_OFF = "alarm_low_off";
	
	/**
	 *  Name of the characteristic defining descriptions for each bit.
	 */
	public static final String NAME_BIT_DESCRIPTION = "bitDescription";
	
	/**
	 * Name of the characteristic defining the colour of LED when bit is set.
	 */
	public static final String NAME_WHEN_SET = "whenSet";

	/**
	 * Name of the characteristic defining the colour of LED when bit is cleared.
	 */
	public static final String NAME_WHEN_CLEARED = "whenCleared";

}
