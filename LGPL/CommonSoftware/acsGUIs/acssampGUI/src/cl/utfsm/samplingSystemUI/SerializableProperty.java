package cl.utfsm.samplingSystemUI;

import java.io.Serializable;

public class SerializableProperty implements Serializable {
	
	private static final long serialVersionUID = -850908451943987979L;
	
	private String component;
	private String property;
	private String samplingGroup;
	private String frequency;
	private String samplingTime;
	private String timeWindow;
	
	public long getFrequency() {
		return Long.parseLong(frequency);
	}
	public void setFrequency(double d) {
		this.frequency = String.valueOf(d);
	}
	public int getSamplingTime() {
		return Integer.parseInt(samplingTime);
	}
	public void setSamplingTime(int samplingTime) {
		this.samplingTime = String.valueOf(samplingTime);
	}
	public int getTimeWindow() {
		return Integer.parseInt(timeWindow);
	}
	public void setTimeWindow(int timeWindow) {
		this.timeWindow = String.valueOf(timeWindow);
	}
	
	public String getComponent() {
		return component;
	}
	public void setComponent(String component) {
		this.component = component;
	}
	public String getProperty() {
		return property;
	}
	public void setProperty(String property) {
		this.property = property;
	}
	public String getSamplingGroup() {
		return samplingGroup;
	}
	public void setSamplingGroup(String samplingGroup) {
		this.samplingGroup = samplingGroup;
	}

}
