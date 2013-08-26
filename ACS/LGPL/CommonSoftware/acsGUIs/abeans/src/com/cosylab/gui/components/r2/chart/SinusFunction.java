package com.cosylab.gui.components.r2.chart;

/**
 * This is an example of function, which calculate sinus of argument, pre-defined amplitude, phase, frequency, and then adds constant called offset.
 */
public class SinusFunction implements Function {
	private double offset = 1.0;
	private double amplitude = 2.0;
	private double phase = 0.0;
	private double frequency = 1.0;
/**
 * SimusFunction constructor with no arguments.
 */
public SinusFunction() {
	super();
}
/**
 * SimusFunction constructor with no arguments.
 */
public SinusFunction(double amplitude, double frequency, double offset, double phase) {
	super();
	this.amplitude= amplitude;
	this.frequency= frequency;
	this.offset=  offset;
	this.phase= phase;
}
/**
 * This method return amplitude of function.
 */
public double getAmplitude() {
	return amplitude;
}
/**
 * This method return frequency of function.
 */
public double getFrequency() {
	return frequency;
}
/**
 * This method return offset of function. That is constant value, which is added to sinus value.
 */
public double getOffset() {
	return offset;
}
/**
 * This method return phase of function.
 */
public double getPhase() {
	return phase;
}
/**
 * This method sets amplitude of function.
 */
public void setAmplitude(double newAmplitude) {
	amplitude = newAmplitude;
}
/**
 * This method sets frequency of function.
 */
public void setFrequency(double newFrequency) {
	frequency = newFrequency;
}
/**
 * This method sets offset of function.
 */
public void setOffset(double newOffset) {
	offset = newOffset;
}
/**
 * This method sets phase of function.
 */
public void setPhase(double newPhase) {
	phase = newPhase;
}
/**
 * This method calculate value and return it.
 */
public double y(double x) {
	return offset+amplitude*Math.sin((frequency*x+phase)*2.0*Math.PI);
}
}
