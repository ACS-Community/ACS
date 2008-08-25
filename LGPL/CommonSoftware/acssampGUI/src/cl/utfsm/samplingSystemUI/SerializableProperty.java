package cl.utfsm.samplingSystemUI;

import java.io.Serializable;

public class SerializableProperty implements Serializable {
	
	private static final long serialVersionUID = -850908451943987979L;
	
	private String component;
	private String property;
	private String samplingGroup;
	
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
