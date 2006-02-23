/*
 * Created on Feb 17, 2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */


package antMountGUI;

import com.cosylab.gui.displayers.DoubleConsumer;
import com.cosylab.gui.displayers.LongSeqConsumer;
import com.cosylab.gui.displayers.DataSourceSupport;
import com.cosylab.gui.displayers.DataConsumer;
import com.cosylab.gui.displayers.DataState;

import antMountGUI.AntennaMountGUI;

import java.util.Map;

/**
 * @author acaproni
 *
 * Take a double (property) and converts it in a sequence of long
 * It is used to convert radians (double) in degree in the form dd:mm:ss.xxx
 * 
 * @invariant theGUI!=null
 * @invariant delegate!=null
 * 
 */
public class ValueConverter extends DataSourceSupport implements DoubleConsumer{
	
	/**
	 * The consumer
	 */
	private LongSeqConsumer delegate;
	
	/**
	 * The main window
	 */
	private AntennaMountGUI theGUI;
	
	/** 
	 * Build a new object
	 * 
	 * @param disp The LongSeqConsumer that receives the values to display as string
	 * 
	 * @pre disp!=null && theGUI!=null
	 */
	public ValueConverter(LongSeqConsumer disp, AntennaMountGUI theGUI) {
		// In this version it return the 
		super(new Class[]{DoubleConsumer.class });
		delegate=disp;
		this.theGUI=theGUI;
	}
	
	/**
	 * The method that effectively convert the randian in degree
	 * 
	 * @param timestamp The timestamp
	 * @param value The radiant
	 */
	public void updateValue(long timestamp, double value) {
		long[] res = new long[2];
		if (theGUI.showDegrees()) {
			// The user selected degrees
		} else {
			// The user selected radiants
			value=Math.toRadians(value);
		}
		res[0]=(long)Math.floor(value);
		res[1]=(long)Math.abs(Math.round(value-(double)res[0])*100);
		try {
			delegate.updateValue(timestamp, res);
		} catch (Exception e) {
			System.err.println("Exception caught: "+e.toString());
		}
	}
	
	
	public DataConsumer getDataConsumer(Class type) { 
		if (!type.isAssignableFrom(this.getClass())) {
			return null;
		}
		return this;
	}
	
	public DataConsumer getDefaultDataConsumer() {
		return this;
	}
	
	public String getName() {
		return delegate.getName();
	}
	
	public String[] getSupportedCharacteristics() {
		return delegate.getSupportedCharacteristics();
	}
	
	public void setCharacteristics(Map attributes) {
		delegate.setCharacteristics(attributes);
	}
	
	public void updateDataState(DataState state) {
		delegate.updateDataState(state);
	}
	
	public Class[] getSupportedConsumerTypes() {
		return new Class[] { DoubleConsumer.class };
	}
}
