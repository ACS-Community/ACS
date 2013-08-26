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
 *      @author Julio Araya (jaray[at]alumnos.inf.utfsm.cl) &
 *      Nicolas Troncoso (ntroncos[at]alumnos.inf.utfsm.cl)
 **/

package cl.utfsm.samplingSystemUI.core;
import java.lang.String;

/**
 * Class SampDetail hold the detail for a sampling order. Each one of this
 * objects should be unique. Two of theese objects are considered equals if
 * their component, property, frequency and reportRate are equals. For this
 * reason the equals meber had to be overloaded. The hashcode member was also
 * overloaded. This class has no seter members, for that reason values may only
 * be set upon creation, this is a failsafe so you don't accidentaly modify the
 * values once sampling has began. As values can only be set at creation, the
 * hashcode is computed only once, at creation. If later on this class is
 * modified to allow seters this behavior must be kept in mind, and be
 * modified.
 */ 
public class SampDetail {
	String component = "";
	String property = "";
	long frequency;
	long reportRate;
	int hashcode=0;

	public SampDetail(String component,String property,long frequency,long reportRate){
		this.component=component;
		this.property=property;
		this.frequency=frequency;
		this.reportRate=reportRate;
		rehash();
	}

	public String getComponent(){
		return component;
	}
	public String getProperty(){
		return property;
	}
	public long getFrequency(){
		return frequency;
	}
	public long getReportRate(){
		return reportRate;
	}
	/**
	* compare with an object and see if they are equal. This is an
	* overloaded version of Object.equals(), this meber returns true if
	* anObject is of class SampDetail, and if all attributes are the same.
	*
	* @param anObject object type to which to compare to.
	* @return true or false.
	*/
	public boolean equals(Object anObject){
		if(anObject instanceof SampDetail){
			SampDetail sDetail = (SampDetail)anObject;
			return sDetail.getComponent().equals(component) && sDetail.getProperty().equals(property) && sDetail.getFrequency()==frequency && sDetail.getReportRate()==reportRate;
		}
		else
			return false;
	}

	/** Return de overloaded version of hashCode. This version return the
 	* value of hashcode computed by rehash().  
	* @return int has value
	*/
	public int hashCode(){
		return hashcode;
	}

	/** Hashing funtion. This function computes the hashcode that is
	* returned by invocations of hashCode() member. Since the contract of a
	* Hash Code function is demanding, this function makes use of the
	* hashing function of a tested class. The hashing is done by
	* concatenating the name of the component, property, frequency and
	* reportRate into a String and then calculating the hascode of such
	* string. This ensures us a unique hashcode
 	* and a uniformly distributed.
	*
	* hash is calculated over:
	* 	'COMPONET_PROPERTY_FREQUENCY_REPORTRATE'
	*
	* @see String
	*/
	private void rehash(){
		String temp1 = component+'_'+property+'_'+String.valueOf(frequency)+'_'+String.valueOf(reportRate);
		hashcode=temp1.hashCode();
	}
}
