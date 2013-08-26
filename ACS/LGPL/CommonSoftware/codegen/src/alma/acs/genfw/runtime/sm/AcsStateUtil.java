/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.genfw.runtime.sm;

/**
 * Todo: move out of this package once we have general ACS state machine generation set up
 * 
 * @author hsommer
 * created Apr 15, 2004 5:43:38 PM
 */
public class AcsStateUtil
{
	public static String stateHierarchyToString(AcsState[] stateHierarchy) {
		StringBuffer buf = new StringBuffer();
		for (int i = 0; i < stateHierarchy.length; i++) {
			AcsState state = stateHierarchy[i];
			buf.append(state.stateName());
			if (i < stateHierarchy.length - 1) {
				buf.append('/');
			}
		}
		return buf.toString();
	}
	
	/**
	 * Concatenates the nested state names (path elements) of a substate to a <code>String</code>.
	 * For example, if "busy" is a substate of "operational", passing the <code>String[]</code>
	 * <code>{"operational", "busy"}</code> will return <code>"operational/busy"</code>.
	 * <p>
	 * The main use of this method is to hide the "path separator" for nested states and spare
	 * an application the (admittedly not so awful) concatenation.  
	 * @param stateHierarchyNames outmost state first, nested child (leaf) state last
	 * @return concatenated hierarchical state 
	 */
	public static String stateHierarchyNamesToString(String[] stateHierarchyNames) {
		StringBuffer buf = new StringBuffer();
		for (int i = 0; i < stateHierarchyNames.length; i++) {
			buf.append(stateHierarchyNames[i]);
			if (i < stateHierarchyNames.length - 1) {
				buf.append('/');
			}
		}
		return buf.toString();
	}

}
