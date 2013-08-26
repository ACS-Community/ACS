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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Date;
import java.util.StringTokenizer;

import alma.acs.util.IsoDateFormat;
import alma.acs.util.UTCUtility;
import cl.utfsm.samplingSystemUI.core.DataItem;

/**
 * This class correlates various sampling sets through time, and outputs them in a nicely csv file.
 * 
 * This class takes various Sampling Data file outputs, which are passed through the addSamplingSet method.
 * Once finished adding Sampling Set, just call the dumpToFile method, and the data will be correlated.
 * Please notice that the usage of memory is minimal and has to be kept whis way.
 * 
 * @author Arturo Hoffstadt Urrutia <ahoffsta@inf.utfsm.cl>
 */
public class SamplingDataCorrelator {
	
	private String filename="";
	private String group="";
	private long frequency;
	private Date startTimestamp = null;
	private FileWriter file;
	private BufferedWriter writer;
	private String header;
	private ArrayList<ArrayList<DataItem>> data;
	private ArrayList<BufferedReader> readers;
	private ArrayList<String> headers;
	private IsoDateFormat formater;
	private int meanQty;
	private ArrayList<ArrayList<Double>> meanData; 
	
	

	/**
	 * Default constructor, takes the parameters, stores them, and creates the filename.
	 * @param group The name of the Sampling Group to which the data belongs to.
	 * @param frequency The Frequency at which the data was sampled.
	 * @param startTimestamp At which time that sampling process started.
	 */
	public SamplingDataCorrelator( String group, long frequency, Date startTimestamp ){
		data = new ArrayList<ArrayList<DataItem>>();
		readers = new ArrayList<BufferedReader>();
		headers = new ArrayList<String>();
		this.group = group;
		this.frequency = frequency;
		this.startTimestamp = startTimestamp;
		formater = new IsoDateFormat();
		filename = "" + this.group.replace('/', '-') + "_" + this.frequency + "_" + formater.format( this.startTimestamp ) + ".csv";
		header = "\"Timestamp in ISO Format\"";
		meanQty = 4;
		meanData = new ArrayList<ArrayList<Double>>();
	}
	
	/**
	 * Registers a Sampling Set, and read the first line, obtaining the component and the property which sampled.
	 * @param filaname Name of the file in which the data for this Sampling Set was dumped.
	 */
	public void addSamplingSet( String filename ){
		BufferedReader br = openReadOnly(filename);
		String line = null;
		try {
			if( ( line = br.readLine() ) != null ){
				StringTokenizer st = new StringTokenizer( line , ";" );
				while( st.hasMoreTokens() ){
					st.nextToken();
					headers.add(st.nextToken());
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		readers.add(br);
		data.add(new ArrayList<DataItem>());
		header = header + ";" + headers.get(headers.size()-1);
		meanData.add( new ArrayList<Double>() );
	}

	/**
	 * Start the Correlation and Dumping to File process. 
	 */
	public void dumpToFile(){
		dumpToFile( 0.5 );
		for(BufferedReader br: readers){
			try {
				br.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
		}		
	}
	
	/**
	 * Starts the correlation process. This method should only be called if you know what you are doing.<br /> 
	 * Setting the prec lower or higher will get you leaks of data.
	 * @param prec How much of the Frequency will each entry take as a valid interval of time to look forward and backward for data.
	 */
	public void dumpToFile( double prec ){
		boolean done = false;
		int flag = 0;
		frequency = 1000000L / frequency;
		long w = (long) (frequency*prec);
		
		
		// A brief buffer is created, according to how much data the mean needs.		
		consume();
		for( int k = 0; k < meanQty; k++ ){
			consume();
		}
		
		//The First timestamp (the earliest), is the one that is used to align the data.
		long timestamp = data.get(0).get(0).getTime();
		
		// The file is open, and the header is written
		openReadWrite();	
		try {
			writer.write(header+"\n");
		} catch (IOException e1) {
			e1.printStackTrace();
		}
		
		while( !done ){
			consume();
			String line = "" + formater.format(new Date(UTCUtility.utcOmgToJava(timestamp)));
			boolean dataPresent = true;
			
			for( ArrayList<DataItem> i: data ){
				dataPresent = false;
				if( i.isEmpty() ){
					line += ";";
					continue;
				}
				
				DataItem item = i.get(0);
				if(	(item.getTime() >= (timestamp-w) ) &&
					(item.getTime() <= (timestamp+w) )){
					line += ";" + item.getValue();
					dataPresent = true;
					addValueToMean( data.indexOf(i), item.getValue() );
					i.remove(item);
				}else{
					if(	( item.getTime() >= ( timestamp + w )) &&
						( item.getTime() <= ( timestamp + frequency - w ))){
						line += ";" + mean(data.indexOf(i));
						addValueToMean( data.indexOf(i), mean( data.indexOf(i)) );
						i.remove(item);
					}else{
						line += ";" + mean(data.indexOf(i));
						addValueToMean( data.indexOf(i), mean( data.indexOf(i)) );
					}
						
				}
				if( i.isEmpty() )
					flag++;
			}
			if( dataPresent ){
				try {
					writer.write( line + "\n" );
				}catch(IOException e){
					e.printStackTrace();
				}
				
			}
			// Check if we passed over all dataItem recollected			
			if( flag == data.size() )
				done = true;
			timestamp += frequency;
			//System.out.println("Array completed: " + flag + "\t Array Size: " + data.get(0).size());
		}
		try {
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Method that consumes one line on every cvs file, storing them on memory.
	 * @return True in case that there remains lines to be consumed, and false in case that no lines remain in every file.
	 */
	public boolean consume(){
		
		String line = null;
		String timestamp = null;
		String value = null;
		int finished = 0;
		int j = 0;
		for( BufferedReader i : readers){
			try {
				if( ( line = i.readLine() ) != null ){
					StringTokenizer st = new StringTokenizer( line , ";" );
					while( st.hasMoreTokens() ){
						timestamp = st.nextToken();
						value = st.nextToken();
					}
					//System.out.println("Parsed: " + formater.format(new Date( formater.parse( timestamp ).getTime() )) + "; " + value );
					data.get(j).add( new DataItem( UTCUtility.utcJavaToOmg((formater.parse(timestamp).getTime())), Double.parseDouble(value) ) );
				}else{
					finished++;
				}
			} catch (IOException e) {
				e.printStackTrace();
			} catch (ParseException e) {
				e.printStackTrace();
			}
			j++;
		}
		if( finished == readers.size() )
			return false;
		else
			return true;
	}
	
		
		

	
	/**
	 * Open the file with the filename specified in the object.
	 */
	private void openReadWrite(){
		try {
			file = new FileWriter(filename);
		} catch (IOException e) {
			e.printStackTrace();
		}
		writer = new BufferedWriter(file);
	}
	
	private BufferedReader openReadOnly(String filename){
		try {
			return new BufferedReader( new FileReader( filename ) );
		} catch (FileNotFoundException e) {
			e.printStackTrace();
			return null;
		}
	}
	
	private double mean( int index ){
		if( meanData.get(index).isEmpty() )
			return 0.0;
		else{
			double sum = 0;
			for( Double i : meanData.get(index) ){
				sum += i.doubleValue();
			}
			return sum/(double)(meanData.get(index).size());
		}
	}
	
	private void addValueToMean( int index, double value ){
		meanData.get( index ).add( new Double( value ) );
		if( meanData.get( index ).size() > meanQty )
			meanData.get( index ).remove(0);
	}

	public String getFilename() {
		return filename;
	}
}
