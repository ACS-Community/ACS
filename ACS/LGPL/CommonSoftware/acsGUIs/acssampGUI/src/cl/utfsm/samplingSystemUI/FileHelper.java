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
package cl.utfsm.samplingSystemUI;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;

import alma.acs.util.IsoDateFormat;
import alma.acs.util.UTCUtility;

import cl.utfsm.samplingSystemUI.core.DataItem;

/** Takes the data from the sampling groups and dumps it in correlated timestamps.
 * 
 * This class allows takes the ArrayLists that contains the data from a Sampling Group, <br />
 * and when indicated so by the dumpToFile method, start the writing to a preestablished file.
 * 
 * @author javarias, abaltra
 * @author  Arturo Hoffstadt Urrutia <ahoffsta[AT]inf.utfsm.cl>
 */
public class FileHelper {
	
	private String filename="";
	private String group="";
	private FileWriter file;
	private BufferedWriter writer;
	private String header;
	private ArrayList<ArrayList<DataItem>> data;
	
	
	/**
	 * Default constructor. Initialize the data array with empty values.
	 */
	public FileHelper(){
		data = new ArrayList<ArrayList<DataItem>>();
	}
	
	/**
	 * Overloaded constructor that sets the prefix/group of the Sampling Group.
	 * @param group
	 */
	public FileHelper( String group ){
		data = new ArrayList<ArrayList<DataItem>>();
		this.group = group;
	}

	/**
	 * Adds a Sampling Set to the array containing some (or all) the data to be printed to file.
	 * @param samp Sampling Set (data from one property), to be added to the set of data to be printed to the file.
	 */
	public void addSamplingSet(ArrayList<DataItem> samp){
		data.add(samp);
	}
	
	/**
	 * Re initialize the data, erasing everything.
	 */
	public void removeSamplingSets(){
		data = new ArrayList<ArrayList<DataItem>>();
	}
	
	/**
	 * Sets the header that will be printed at the beginning of the file.
	 * @param header Header of the content of the file. Is a resume of the properties.
	 */
	public void setHeaderFile(String header){
		this.header=header;
	}
	
	/**
	 * Sets the prefix for the filename. Usually the sampling group name.
	 * @param prefix
	 */
	public void setFilePrefix( String prefix ){
		this.group = prefix;
	}
		
	/**
	 * @return Name of the file which the data will be dumped.
	 */
	public String getFileName(){
		return filename;
	}

	public void initialize(int freq){

		IsoDateFormat fo = new IsoDateFormat();
		if( group == "" )
			filename = "samp_"+10000000L/freq+"_"+fo.format(new Date()) +".csv";
		else
			filename = group+"_"+10000000L/freq+"_"+fo.format(new Date()) +".csv";
	}
	
	/**
	 * Start the dumping process.
	 * @param frequency Frequency at which the data is to be separated in the printout file. 
	 */
	public void dumpToFile(long frequency){
		dumpToFile(frequency,0.5);
	}
	
	/**
	 * Start the dumping process. This method should only be called if you know what you are doing.<br /> 
	 * Setting the prec lower will get leaks of data, and setting it higher, will get you duplicated data.
	 * @param frequency Frequency at which the data is to be separated in the printout file.
	 * @param prec How much of the Frequency will each entry take as a valid interval of time to look forward and backward for data.
	 */
	public void dumpToFile(long frequency, double prec){

		IsoDateFormat formater = new IsoDateFormat();
		long timestamp=data.get(0).get(0).getTime();
		boolean done = false;
		frequency=1000000L/frequency;
		long w = (long) (frequency*prec);
		int [] c= new int[data.size()];
		openFile();
		try {
			writer.write(header+"\n");
		} catch (IOException e1) {
			e1.printStackTrace();
		}
		while(!done){
			String line = "" + formater.format(new Date(UTCUtility.utcOmgToJava(timestamp)));
			boolean dataPresent = true;
			for(int i=0;i<data.size();i++){
				dataPresent = false;
				if(c[i]==data.get(i).size()){
					line+=";";
					continue;
				}
				DataItem item = data.get(i).get(c[i]);
				if((item.getTime()>=(timestamp-w)) && 
				   (item.getTime()<=(timestamp+w))) {
					line+=";"+item.getValue();
					c[i]++;
					dataPresent = true;
				}
				else if((item.getTime()>=(timestamp+w)) &&  
						(item.getTime()<=(timestamp+frequency-w))){
					line+=";";
					c[i]++;
				}
				else
					line+=";";
			}
			try {
				if( dataPresent ){
					writer.write(line+"\n");
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
			/*Check if we passed over all dataItem recolected*/
			int flag=0;
			for(int i=0;i<c.length;i++){
				if(c[i]==data.get(i).size())
					flag++;
			}
			if (flag==c.length)
				done=true;
			
			timestamp+=frequency;
		}
		try {
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Open the file with the filename specificated in the object.
	 */
	private void openFile(){
		try {
	 	file=new FileWriter(filename);
	
		} catch (IOException e) {
			e.printStackTrace();
		}
		writer=new BufferedWriter(file);
	}
}
