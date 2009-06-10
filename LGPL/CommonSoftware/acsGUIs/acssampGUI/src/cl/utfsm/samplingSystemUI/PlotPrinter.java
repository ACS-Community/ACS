package cl.utfsm.samplingSystemUI;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;

import alma.ACSErrTypeCommon.CouldntAccessComponentEx;
import alma.ACSErrTypeCommon.TypeNotSupportedEx;
import alma.acs.util.IsoDateFormat;
import alma.acs.util.UTCUtility;
import alma.ACSErrTypeCommon.CouldntAccessPropertyEx;

import cl.utfsm.samplingSystemUI.DataPrinter;
import cl.utfsm.samplingSystemUI.core.DataItem;
import cl.utfsm.samplingSystemUI.core.SamplingManagerException;

public class PlotPrinter extends DataPrinter {
	
	private long samplesCounter;
	private String filename="";
	private FileWriter file;
	private BufferedWriter writer;
	private IsoDateFormat formater;
	private boolean stopped;
	private int position = 0;
	private boolean dumpToFile = true;

	/**
	 * Constructor, initialize the sampling counter and creates a BeanMemoryWidget for representation.
	 * @param ssg Sampling System Gui reference.
	 */
	public PlotPrinter(SamplingSystemGUI ssg) {
		super(ssg);
		samplesCounter=0;
		widget = new PlotWidget(0);
		formater = new IsoDateFormat();
		stopped = true;
		position = 0;
	}
	
	/**
	 * Constructor, initialize the sampling counter and sets the Widget as the one passed by reference. <br />
	 * Very useful when you need to draw in one widget, many Printers.
	 * @param ssg Sampling System Gui reference.
	 * @param widget The Widget in that this Printer will output graphically. 
	 */
	public PlotPrinter(SamplingSystemGUI ssg, PlotWidget widget, int position) {
		super(ssg);
		samplesCounter=0;
		this.widget = widget;
		formater = new IsoDateFormat();
		stopped = true;
		this.position = position;
		widget.initializeNewPosition(position);
	}

	/**
	 * Since the samples are not stored in memory, nothing can be returned. 
	 */
	public ArrayList<DataItem> getSamples() {
		return null;
	}
	
	/**
	 * Return the name of the file where the data is being dumped to, or the last file used.
	 * @return
	 */
	public String getFilename() {
		return filename;
	}
	
	/**
	 * Starts the sampling, connecting to ACS Manager and the Sampling Manager.
	 * @throws CouldntAccessComponentEx Component wasn't available at the time.
	 * @throws TypeNotSupportedEx Sampling Manager specific exception. Some types are currently not supported in acssamp.
	 */
	public void startSample() throws CouldntAccessComponentEx, TypeNotSupportedEx,CouldntAccessPropertyEx, SamplingManagerException {
		super.startSample();
		stopped = false;		
	}

	/**
	 * This method should be called after stopping the sampling.
	 */
	public void postProcessing() {
		stopped = true;
		widget.resetSampleCount();
		samplesCounter = 0;
		if( dumpToFile ){
			try {
				writer.close();
			} catch (IOException e) {
				e.printStackTrace();
			}			
		}
		
	}

	protected void updateValue(DataItem item) {
		if(!stopped){
			samplesCounter++;
			if( dumpToFile ){
				if( samplesCounter == 1){
					openFile();
					String line = "\"Timestamp ISO Format\";" + component + "_" + property;
					try {
						writer.write(line + "\n");
					} catch (IOException e) {
						e.printStackTrace();
					}
				}	
			}				
			widget.updateValues(item.getTime(), item.getValue(), position);
			if( dumpToFile ){
				String line = "" + formater.format(new Date(UTCUtility.utcOmgToJava(item.getTime()))) + ";"+ item.getValue();
				try {
					writer.write(line + "\n");
				} catch (IOException e) {
					e.printStackTrace();
				}
			}		
			
		}		
	}
	
	public void setComponent(String component) {
		super.setComponent(component);
		widget.setValues(component, property, position);
	}

	public void setProperty(String property) {
		super.setProperty(property);
		widget.setValues(component, property, position);
	}
	
	public void setComponentAvailable(boolean available,String reason) {
		super.setComponentAvailable(available,reason);
		widget.setComponentAvailable(available,reason, position);
	}
	
	private boolean openFile(){
		IsoDateFormat fo = new IsoDateFormat();
		filename = component.replace('/', '-') + "_" + property.replace('/', '-') + "_" + getFrecuency() + "_" + fo.format(new Date()) +".csv";
		try{
			file = new FileWriter( filename );
		}catch( IOException e ){
			e.printStackTrace();
			return false;
		}
		writer = new BufferedWriter( file );
		return true;
	}
	
	public void setTimeWindow(int freq, int time) {
		widget.setTimeWindow( frecuency, time);
	}
	
	public void setDumpToFile( boolean value){
		dumpToFile = value;
	}
	
}
