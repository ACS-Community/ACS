/**
 * @author Jorge Avarias <javarias[at]inf.utfsm.cl>
 * 
 */

package cl.utfsm.samplingSystemUI;

import java.util.ArrayList;

import cl.utfsm.samplingSystemUI.core.DataItem;

public class MemoryPrinter extends DataPrinter {

	protected ArrayList<DataItem> samples;
	protected static final int INITIAL_THREAD_SUPPORT_SIZE = 20;
	private long samplesCounter;
	
	public MemoryPrinter(SamplingSystemGUI ssg){
		super(ssg);
		samples=new ArrayList<DataItem>();
		samplesCounter=0;
		widget=new BeanMemoryWidget();
	}
	
	public void updateValue(DataItem item) {
		samples.add(item);
		samplesCounter++;
		widget.updateValues(item.getTime(), item.getValue(), 0);
	}

	/**
	 * In memory printer postProcesing do nothing with sampled data stored, 
	 * they will be destroyed in object destruction.
	 */
	public void postProcessing() {
		widget.resetSampleCount();
	}

	public void setComponent(String component) {
		super.setComponent(component);
		widget.setValues(component, property, 0);
	}

	public void setProperty(String property) {
		super.setProperty(property);
		widget.setValues(component, property, 0);
	}

	public ArrayList<DataItem> getSamples() {
		return samples;
	}
	
	public void setComponentAvailable(boolean available,String reason) {
		super.setComponentAvailable(available,reason);
		widget.setComponentAvailable(available,reason, 0);
	}
}
