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

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.concurrent.LinkedBlockingQueue;

import alma.ACSErrTypeCommon.CouldntAccessComponentEx;
import alma.ACSErrTypeCommon.TypeNotSupportedEx;
import alma.JavaContainerError.wrappers.AcsJContainerEx;
import alma.ACSErrTypeCommon.CouldntAccessPropertyEx;

import cl.utfsm.samplingSystemUI.core.AcsInformationException;
import cl.utfsm.samplingSystemUI.core.DataItem;
import cl.utfsm.samplingSystemUI.core.SampDetail;
import cl.utfsm.samplingSystemUI.core.SamplingManagerException;
import cl.utfsm.samplingSystemUI.core.SamplingManagerUITool;
import cl.utfsm.samplingSystemUI.core.ThreadCommunicator;



/**
 * Class that handles the comunication with the acs sampling system.
 * This class extends the core funtionality given by SamplingManagerUITool
 * @see SamplingManagerUITool
 *
 */ 
public abstract class DataPrinter extends SamplingManagerUITool{
	
    /*Information required by the acs sampling system.
     * @param name          component to be sampled (e.g. LAMP1)
     * @param property      property to be sampled (e.g brightness)
     * @param frequency     sampling frequency: period between two consecutive
     *                      sampling
     *                      (units are 100ns; e.g. 1000000 means 0.1 sec i.e.
     *                       10 sample per second)
     * @param reportRate    number of second the process should buffer 
     *                      before actually sending (notifying) data
     *                      (units are 100ns; e.g. 10000000 means collect data
     *                       for 1 second)
     */
    private static long FREQ_CONV=10000000L;//1E7
	protected long frequency=FREQ_CONV; //set frequency default to 1 Hz.
	protected long reportRate=1;
	protected String component;
	protected String property;

	protected SamplingWidget widget;
	public Sampler samp = null;
	private static int initializations=0;
	private boolean componentAvailable = true;
	private SamplingSystemGUI ssg = null;
	
	public DataPrinter(SamplingSystemGUI ssg) {
		this.ssg = ssg;
	}
	
	/**
	* Returns the frequency in Hz.
	*
	* @return double. frequency in Hz.
	*/
	public double getFrequency() {
		double freq = ((double)FREQ_CONV/(double)this.frequency);
		String tmp = new Double(freq).toString();
		tmp = tmp.substring(0,tmp.lastIndexOf(".")+2);
		freq = Double.parseDouble(tmp);
		return freq;
	}

	public long getReportRate() {
		return reportRate;
	}
	
	public String getComponent() {
		return component;
	}

	public String getProperty() {
		return property;
	}
	
	public SamplingWidget getSamplingWidget(){
		return widget;
	}
	public void setComponent(String component) {
		this.component = component;
	}

	public void setProperty(String property) {
		this.property = property;
	}

	/**
	* Sets the frequency in the sampObj.
	*
	* @param double frequency in Hz. Internally its converted into
    *        the value that sampObj requires.
    * @see SampObj
	*/
	public void setFrequency(double f) {
		long freq = (long)(FREQ_CONV/f);
		this.frequency = freq;
	}

	public void setReportRate(long reportRate) {
		this.reportRate = reportRate;
	}
	
	public IGraphicalUpdater getWidget() {
		return widget;
	}
	
	protected abstract void  updateValue(DataItem item);
	
	public abstract ArrayList<DataItem> getSamples();
	
	/**
	 * Do a post processing once stop all samplings threads. The child class
	 * must implement it if is necessary do a post processing on samplings data
	 * stored.
	 * @see Sampler
	 */
	public abstract void postProcessing();
	
	/**
	 * Starts the sampling, connecting to ACS Manager and the Sampling Manager.
	 * @throws CouldntAccessComponentEx Component wasn't available at the time.
	 * @throws TypeNotSupportedEx Sampling Manager specific exception. Some types are currently not supported in acssamp.
	 * @throws CouldntAccessPropertyEx
	 */
	public void startSample() throws CouldntAccessComponentEx, TypeNotSupportedEx , CouldntAccessPropertyEx, SamplingManagerException{
		samp = new Sampler();
		synchronized(this){
			System.out.println("Initialization:"+ initializations);
			if(initializations==0){
				try{
					spinUp(SampTool.NAME,ssg.MAN_NAME);
					}
					catch (AcsInformationException e){
						System.out.print(e.getMessage());
						System.exit(-1);
					}
					catch (SamplingManagerException e){
						System.out.print(e.getMessage());
						System.exit(-1);
					} catch (AcsJContainerEx e) {
						System.out.println(e.getMessage());
						System.exit(-1);
					}
			}
			initializations++;
		}

		try {
			SamplingManagerUITool.startSample(new SampDetail(component,property,
					(long)this.frequency,reportRate));
		} catch(alma.ACSErrTypeCommon.CouldntAccessComponentEx e) {
			setComponentAvailable(false,"Cannot access component implementation");
			throw e;
		} catch(alma.ACSErrTypeCommon.TypeNotSupportedEx e) {
			setComponentAvailable(false,"Type not supported");
			throw e;
	 	} catch (CouldntAccessPropertyEx e){
			setComponentAvailable(false,"Cannot access the property");
                        throw e;
		} catch(SamplingManagerException e){ 
			setComponentAvailable(false,e.getMessage());
                        throw e;
                }
		samp.start();
	}
	
	public void stopSampling(){
		synchronized(this){
		initializations--;
			if( componentAvailable == true ) {
				samp.halt();
				SamplingManagerUITool.stopSample(new SampDetail(component, property, (long)frequency, reportRate));
				postProcessing();
			}
		}
	}

	/**
	 * Pauses/unpauses the displaying of the sampling process 
	 * @param p Pause status
	 */
	public void pauseSampling(boolean p) {
		if( componentAvailable == true )
			samp.setPause(p);
	}
	
	public void finalize(){
		stopSampling();
		synchronized (this) {
			if(initializations==0){
				try {
					tearDown();
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}
	}
	
	
	/**
	 * Allows to handle whether a component is or isn't available to be sampled.
	 * @param available True if the component is currently available.
	 * @param reason A explanation of the cause why the component isn't available.
	 */
	public void setComponentAvailable(boolean available, String reason) {
		componentAvailable = available;
	}
	
	public boolean isComponentAvailable() {
		return componentAvailable;
	}
	
	public boolean isStopped() {
		if(samp == null){
			return true;
		}
		return samp.isStopped();
	}
	
	
	public SerializableProperty getSerializableProperty(){
		SerializableProperty sp = new SerializableProperty();
		sp.setComponent(getComponent());
		sp.setFrequency(getFrequency());
		sp.setProperty(getProperty());
		return sp;
	}
	
	/**
	 * Thread class for receive data from one notification channel. <br/>
	 * For each time that is called {@startSample} is created a new instance of
	 * this class.
	 * 
	 * @see startSample
	 * 
	 * @author Jorge Avarias <javarias[at]inf.utfsm.cl>
	 */
	class Sampler extends Thread {
		
		private boolean stop = false;
		
		/**
		 * Flag to freeze the information displayer (graphic or whatever).
		 * Anyways, the sampling processes continues 
		 */
		private boolean pause = false;

		public boolean isStopped() {
			return stop;
		}
		
		public boolean isPaused() {
			return pause;
		}
		public void setPause(boolean p) {
			pause = p;
		}
		
		public void halt() {
			stop=true;
		}

		public void run(){
			//int timeout_ms = (int)(1000/getFrequency())*10; //maximum of 10 times of delay
			//Timer timer = new Timer( timeout_ms );
			boolean runningTimer = false;
			LinkedBlockingQueue<DataItem> cChannel;
			LinkedList<DataItem> c = new LinkedList<DataItem>(); 
			int n=0;
			while(true){
				try {					
					if(stop==true) {
						//timer.reset();
						return;
					}
					Thread.sleep(100);
					//"NC_LAMP1_brightness_10000000_1"
					cChannel = ThreadCommunicator.getInstance().getChannel("NC_"+component+"_"+
							property+"_"+frequency+"_"+reportRate);
					
					if(cChannel==null)
						continue;
					if( (cChannel.size() == 0) && !runningTimer ) {
						runningTimer = true;
						//timer = new Timer(timeout_ms);
						//timer.start();
					}
					if(runningTimer) {
						if( cChannel.size() == 1 ) {
							runningTimer = false;
							//timer.reset();
						}
					}
					
					cChannel.drainTo(c);
	
					//TODO:Improve the way to drain data: Actually drainTo method
					//drain all sampled data from the sampling start.
					if(c.isEmpty())
						continue;

					if( !pause )
						for(int i=n;i<c.size();i++){
							updateValue(c.get(i));
						}
					n=c.size();
						
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
		}
	}	

}
