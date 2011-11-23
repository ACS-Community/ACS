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
package cl.utfsm.example;

import java.util.Collection;
import java.util.LinkedList;
import java.util.concurrent.LinkedBlockingQueue;

import alma.JavaContainerError.wrappers.AcsJContainerEx;

import cl.utfsm.samplingSystemUI.core.AcsInformationException;
import cl.utfsm.samplingSystemUI.core.DataItem;
import cl.utfsm.samplingSystemUI.core.SampDetail;
import cl.utfsm.samplingSystemUI.core.SamplingManagerException;
import cl.utfsm.samplingSystemUI.core.SamplingManagerUITool;
import cl.utfsm.samplingSystemUI.core.ThreadCommunicator;

/**
 * Class that provides the comand line funtionality. This tool is interactive.
 *
 *
 * This file contains an example of how the samplingSystemUI package must be used. 
 */ 
public class SampTool extends SamplingManagerUITool{

	static String prop;	

	public static void main(String[] args) throws AcsJContainerEx{
		String component = "";
		String property = "";
		long frequency;
		long reportRate;
		Display2 display = new Display2();

		try{
			spinUp("SampTool","SAMP1");
		}
		catch (AcsInformationException e){
			System.out.print(e.getMessage());
			System.exit(-1);
		}
		catch (SamplingManagerException e){
			System.out.print(e.getMessage());
			System.exit(-1);
		}


		/*The previous code should be more or less the same for every tool, perhaps it could be cotaines withind a class tool and future tools inherit this. */
		display.start();
		try{
			while(true){
			//Input Reading.
			java.io.BufferedReader in = new java.io.BufferedReader(new java.io.InputStreamReader(System.in));
			System.out.print("Component name(type - LAMP1):");
			component = in.readLine();
			if(component == null)
				continue;
			if(component.compareToIgnoreCase("exit")==0){
				break;
			}
			if(!componentExists(component)){
				System.out.print("No such component");
				continue;
			}
			System.out.print("Property name(type - brightness):");
			property = in.readLine();
			prop = property;
			if(property == null)
				property="";
			if(!propertyExists(component,property)){
				System.out.print("No such property in "+component+" component");
				continue;
			}
			System.out.print("Sampling Frequency (hz) (type - 1):");
			frequency = (long)10000000/((new Long(in.readLine())).longValue());
			System.out.print("Reporting Frequency (s) (type - 1):");
			reportRate = (new Long(in.readLine())).longValue();

			startSample(new SampDetail(component,property,frequency,reportRate));
	

			}
	}
		catch(Exception e){
			e.printStackTrace();
		}
	finally{
			try{
				display.halt();
				tearDown();
			}catch (Exception e){
				e.printStackTrace();
			}
		}
	}
}

/**
 * This class should be incharge of displaying the results. Does nothing right
 * now.
 */ 
class Display2 extends Thread{
	
	

	boolean stop = false;

	public void halt() {
		stop=true;
	}
	
	public void run() {

		//FileWriter fileW = new FileWriter(AcsInformation.getInstance(), SamplingManager.getInstance());	
		LinkedBlockingQueue<DataItem> cChannel;
		Collection<DataItem> c = new LinkedList<DataItem>(); 
		while(true){
			try {
				if(stop==true)
					return;
				Thread.sleep(5000);
				cChannel = ThreadCommunicator.getInstance().getChannel("NC_LAMP1_brightness_10000000_1");
				if(cChannel==null)
					continue;
				cChannel.drainTo(c);
				if(c.isEmpty())
					continue;
				for(DataItem item: c){
					//DataItem i = (DataItem)item;
					System.out.println(item.getTime() + " " + item.getValue() );
					//fileW.initializeComponents();
					//fileW.showData();
			}		
					
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}
	}
}

