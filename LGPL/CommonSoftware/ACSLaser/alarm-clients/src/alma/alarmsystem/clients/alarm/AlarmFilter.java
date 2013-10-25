/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2013 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.alarmsystem.clients.alarm;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The filter for alarms based on the triplet &lt;FF,FM,FC&gt;.
 * <P>
 * A alarm is accepted if:
 * <UL>
 * 	<LI>The FF of the alarm matches with the regular expression for the family
 *  <LI>The FM of the alarm matches with the regular expression for the member
 *  <LI>The FC is in the passed range ie. [Min, Max]
 * </UL>
 * At least one criteria must be set: if filtering is not desired then 
 * it must not be instantiated.
 * <BR>
 * The filter checks if a triplet matches with the given constraints
 * where the triplet is passed in the form FF,FM,FC so that this filter
 * can be used for source clients as well as for category clients.
 * <P>
 * A {@link AlarmFilter} is immutable.
 * 
 * @author acaproni
 * @since ACS 12-2
 */
public class AlarmFilter {

	/**
	 * The regular expression for filtering the FaultFamiliy.
	 * If <code>null</code> the fault family of the alarm is accepted
	 */
	public final String faultFRegExp;
	
	/**
	 * Pattern for the fault family
	 */
	private final Pattern ffPattern;
	
	/**
	 * The regular expression for filtering the FaultMember
	 * If <code>null</code> the fault member of the alarm is accepted
	 */
	public final String faultMRegExp;
	
	/**
	 * Pattern for the fault member
	 */
	private final Pattern fmPattern;
	
	/**
	 * The minimum accepted value of the fault code
	 * If <code>null</code> The fault code is not checked against a minimum value 
	 */
	public final Integer faultCMin;
	
	/**
	 * The maximum accepted value of the fault code
	 * If <code>null</code> The fault code is not checked against a minimum value 
	 */
	public final Integer faultCMax;
	
	/**
	 * Constructor
	 * 
	 * @param faultFRegExp The regular expression to check against the FF of alarm triplet
	 * @param faultMRegExp The regular expression to check against the FM of alarm triplet
	 * @param min The min accepted value of a fault code (inclusive)
	 * @param max The max accepted value of a fault code (inclusive)
	 */
	public AlarmFilter(String fFRegExp, String fMRegExp, Integer min, Integer max) {
		if (fFRegExp==null && fMRegExp==null && min==null && max==null) {
			// No criteria: this filter will be rejected
			throw new IllegalArgumentException("Invalid filter: at leas on constraint must be present");
		}
		
		this.faultFRegExp=fFRegExp;
		this.faultMRegExp=fMRegExp;
		this.faultCMin=min;
		this.faultCMax=max;
		
		this.ffPattern=(faultFRegExp==null)?null:Pattern.compile(fFRegExp);
		this.fmPattern=(faultMRegExp==null)?null:Pattern.compile(fMRegExp);
	}
	
	/**
	 * Check if the passed triplet matches with the filter.
	 * 
	 * @param ff The FF of the triplet
	 * @param fm The FM of the triplet
	 * @param fc The FC of the triplet
	 * @return
	 */
	public boolean matches(String ff, String fm, Integer fc) {
		if (ff==null || ff.isEmpty()) {
			throw new IllegalArgumentException("Invalid fault family to match against");
		}
		if (fm==null || fm.isEmpty()) {
			throw new IllegalArgumentException("Invalid fault member to match against");
		}
		if (fc==null) {
			throw new IllegalArgumentException("Invalid fault code to match against");
		}
		
		// Start with the FC to improve performances
		if (faultCMin!=null) {
			if (fc.intValue()<faultCMin.intValue()) {
				return false;
			}
		}
		if (faultCMax!=null) {
			if (fc.intValue()>faultCMax.intValue()) {
				return false;
			}
		}
		
		// FM 
		if (fmPattern!=null) {
			Matcher m = fmPattern.matcher(fm);
			if (!m.matches()) {
				return false;
			}
		}
		// FF
		if (ffPattern!=null) {
			Matcher m = ffPattern.matcher(ff);
			if (!m.matches()) {
				return false;
			}
		}
		// Accepetd
		return true;
	}
}
