package cl.utfsm.samplingSystemUI;

import java.util.ArrayList;

import cl.utfsm.samplingSystemUI.core.DataItem;

public class WidgetPrinter extends DataPrinter {
	
	/**
	 * Construct the new objects and assigns a new BeanLister as the widget for the superclass.
	 * @param ssg The Sampling System GUI reference.
	 * @throws NullPointerException Used as an assertion. This should never happen.
	 */
	public WidgetPrinter(SamplingSystemGUI ssg) throws NullPointerException{
		super(ssg);
		BeanLister widget= new BeanLister();
		if(widget==null)
			throw new NullPointerException();
		this.widget = widget;
	}
	
	/** 
	 * This method is used to assign the component to be sampled to the WidgetPrinter. <br /> 
	 * It also calls his super-method, and pass a message to the widget to notify the need <br />
	 * for update of the property and component change.
	 * @param component Name of the Component that contains the Property to be sampled.
	 * @see cl.utfsm.samplingSystemUI.DataPrinter#setComponent(java.lang.String)
	 */
	public void setComponent(String component) {
		super.setComponent(component);
		widget.setValues(component, property, 0);
	}

	/** 
	 * This method is used to assign the property to be sampled to the WidgetPrinter. <br />
	 * It also calls his super-method, and pass a message to the widget to notify the need <br />
	 * for update of the property and component change.
	 * @param property Name of the Property to be sampled.
	 * @see cl.utfsm.samplingSystemUI.DataPrinter#setProperty(java.lang.String)
	 */
	public void setProperty(String property) {
		super.setProperty(property);
		widget.setValues(component, property, 0);
	}

	/** 
	 * Updates the values 
	 * @see cl.utfsm.samplingSystemUI.DataPrinter#updateValue(cl.utfsm.samplingSystemUI.core.DataItem)
	 */
	protected synchronized void updateValue(DataItem item) {
		widget.updateValues(item.getTime(),item.getValue(), 0);
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
