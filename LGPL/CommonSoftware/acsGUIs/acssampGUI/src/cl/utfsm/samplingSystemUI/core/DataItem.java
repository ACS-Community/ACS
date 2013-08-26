/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
/**
 *      @author Julio Araya (jaray[at]alumnos.inf.utfsm.cl)
 *      @author Nicolas Troncoso (ntroncos[at]alumnos.inf.utfsm.cl)
 **/

package cl.utfsm.samplingSystemUI.core;

/**
* Class that contains the current result of the analyzed Component. Also compares the current DataItem with an object.
*/
public class DataItem {
	long time=0;
	double value=0;

	/**
	* Constructor, initializes the time and value variables.
	* @param nTime Time (in milliseconds) since last sampling
	* @param nValue Value obtained of a given sample on nTime
	*/
	public DataItem(long nTime, double nValue) {
		time = nTime;
		value = nValue;
	}

	/**
	* Regular getter for the time of the Item.
	*/
	public long getTime() {
		return time;
	}

	/**
	* Regular getter for the value of the Item.
	*/
	public double getValue(){
		return value;
	}

	/**
	* Method to get the information of the DataItem as a hash
	* @return The values of time and value as an integer hash.
	*/
	public int  hashCode(){
		String temp = String.valueOf(time) + '_' + String.valueOf(value);
		return temp.hashCode();
	} 

	 /**
        * compare with an object and see if they are equal. This is an
        * overloaded version of Object.equals(), this member returns true if
        * anObject is of class DataItem, and if all attributes are the same.
        *
        * @param anObject object type to which to compare to.
        * @return true or false.
        */
        public boolean equals(Object anObject){
                if(anObject instanceof DataItem){
                        DataItem dItem = (DataItem)anObject;
                        return time==dItem.getTime() && value==dItem.getValue(); 
                }
                else
                        return false;
        }

}
