package com.cosylab.gui.components.r2.chart;

/**
 * This interface is to defne mathematical functions, i.e. method that takes one double value as argument
 * and calculate another value from this argument and return it.
 */
public interface Function {
/**
 * This is method method for calculating value of function and return it.
 */
double y(double x);
}
