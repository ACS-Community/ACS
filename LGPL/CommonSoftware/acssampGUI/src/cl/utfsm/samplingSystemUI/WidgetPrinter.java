package cl.utfsm.samplingSystemUI;

import java.util.ArrayList;

import cl.utfsm.samplingSystemUI.core.DataItem;

public class WidgetPrinter extends DataPrinter {
	
	public WidgetPrinter(SamplingSystemGUI ssg){
		super(ssg);
		BeanLister widget= new BeanLister();
		if(widget==null)
			throw new NullPointerException();
		this.widget = widget;
	}
	
	public void setComponent(String component) {
		super.setComponent(component);
		widget.setValues(component, property);
	}

	public void setProperty(String property) {
		super.setProperty(property);
		widget.setValues(component, property);
	}

	protected synchronized void updateValue(DataItem item) {
		widget.updateValues(item.getTime(),item.getValue());
	}

	/**
	 * Do nothing
	 */
	public final void postProcessing() {
		
	}

	public IGraphicalUpdater getWidget() {
		return widget;
	}

	public ArrayList<DataItem> getSamples() {
		return null;
	}
}
