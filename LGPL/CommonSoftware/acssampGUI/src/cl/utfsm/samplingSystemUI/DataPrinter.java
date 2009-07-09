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

public abstract class DataPrinter extends SamplingManagerUITool{
	
	protected long frecuency=1000000; //set frequency default to 10Hz
	protected long reportRate=1;
	protected String component;
	protected String property;
	protected SamplingWidget widget;
	private Sampler samp;
	private static int initializations=0;
	private boolean componentAvailable = true;
	private SamplingSystemGUI ssg = null;
	
	public DataPrinter(SamplingSystemGUI ssg) {
		this.ssg = ssg;
	}
	
	public long getFrecuency() {
		return (long)(10000000L/frecuency);
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
	
	public void setComponent(String component) {
		this.component = component;
	}

	public void setProperty(String property) {
		this.property = property;
	}

	public void setFrecuency(long frecuency) {
		this.frecuency=(long)10000000L/frecuency;
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
			System.out.println(initializations);
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
					this.frecuency,reportRate));
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
				SamplingManagerUITool.stopSample(new SampDetail(component, property, frecuency, reportRate));
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
	
	public SerializableProperty getSerializableProperty(){
		SerializableProperty sp = new SerializableProperty();
		sp.setComponent(getComponent());
		sp.setFrequency(getFrecuency());
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

		public void setPause(boolean p) {
			pause = p;
		}
		
		public void halt() {
			stop=true;
		}

		public void run(){
			LinkedBlockingQueue<DataItem> cChannel;
			LinkedList<DataItem> c = new LinkedList<DataItem>(); 
			int n=0;
			while(true){
				try {
					if(stop==true)
						return;
					Thread.sleep(100);
					//"NC_LAMP1_brightness_10000000_1"
					cChannel = ThreadCommunicator.getInstance().getChannel("NC_"+component+"_"+
							property+"_"+frecuency+"_"+reportRate);
					if(cChannel==null)
						continue;
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
